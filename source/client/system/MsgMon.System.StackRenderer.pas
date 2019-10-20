unit MsgMon.System.StackRenderer;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,
  Winapi.Messages,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Image,
  MSgMon.System.StackBase; // todo: rename?

type
  TStackRendererEvent = procedure(Sender: TObject; const Message: string) of object;
  TStackRendererDataEvent = procedure(Sender: TObject; event_id: Int64; rows: TStackRows) of object;

  TStackRenderer = class
  private
    FThread: TThread;
    FSystemModules: TMMImages;
    FOnEvent: TStackRendererEvent;
    FOnData: TStackRendererDataEvent;
  protected
    procedure DataResult(event_id: Int64; rows: TStackRows);
  public
    constructor Create(const ADbghelpPath, ASymbolPath: string; ASystemModules: TMMImages);
    destructor Destroy; override;
    function Render(event_id: Int64; stack_in: TArrayOfByte; images_in: TMMImages): TStackRows;
//    property DbghelpLoaded: Boolean read FDbghelpLoaded;
    property OnEvent: TStackRendererEvent read FOnEvent write FOnEvent;
    property OnData: TStackRendererDataEvent read FOnData write FOnData;
  end;

implementation

uses
  MsgMon.Win.DbgHelp;

type
  TStackRenderInput = class
  public
    images: TMMImages;
    firstKernelModule: Integer;
    stack: TArrayOfByte;
    event_id: Int64;
    constructor Create(images: TMMImages; firstKernelModule: Integer; stack: TArrayOfByte; event_id: Int64);
    destructor Destroy; override;
  end;

  TStackRenderResult = class
    event_id: Int64;
    rows: TStackRows;
    constructor Create(event_id: Int64; rows: TStackRows);
  end;

  TStackRenderThread = class(TThread)
  private
    hSymProcess: THandle;
    FDbghelpLoaded: Boolean;
    FSymbolFiles: TDictionary<string,string>;


    FOwner: TStackRenderer;
    FResultWnd: THandle;
    FDataEvent: TEvent;
    FNext, FCurrent: TStackRenderInput;
    FLock: TCriticalSection;
    FDbghelpPath: string;
    FSymbolPath: string;
    procedure DataResultProc(var Message: TMessage);
    function ProcessStack: TStackRenderResult;
    procedure SetupSymbols;
    procedure CleanupSymbols;
    function SymCallback(hProcess: THandle; ActionCode: ULONG; CallbackData,
      UserContext: ULONG64): BOOL;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TStackRenderer; const ADbghelpPath, ASymbolPath: string);
    destructor Destroy; override;
    procedure QueueProcessing(
      event_id: Int64;
      firstKernelModule: Integer;
      ims: TMMImages;
      stack: TArrayOfByte);
  end;

{ TStackRenderer }

function SymCallback_(hProcess: THandle; ActionCode: ULONG; CallbackData: ULONG64; UserContext: ULONG64): BOOL; stdcall;
begin
  Result := TStackRenderThread(DWORD(UserContext)).SymCallback(hProcess, ActionCode, CallbackData, UserContext);
end;

constructor TStackRenderer.Create(const ADbghelpPath, ASymbolPath: string; ASystemModules: TMMImages);
begin
  inherited Create;
  FSystemModules := ASystemModules.Clone;
  FThread := TStackRenderThread.Create(Self, ADbghelpPath, ASymbolPath);
end;

procedure TStackRenderer.DataResult(event_id: Int64; rows: TStackRows);
begin
  if Assigned(FOnData) then
    FOnData(Self, event_id, rows);
end;

destructor TStackRenderer.Destroy;
begin
  FSystemModules.Free;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    (FThread as TStackRenderThread).QueueProcessing(0, 0, nil, nil);
    FThread.Free;
  end;
  inherited Destroy;
end;

function TStackRenderer.Render(event_id: Int64; stack_in: TArrayOfByte; images_in: TMMImages): TStackRows;
var
  m0, ims: TMMImages;
  FFirstKernelModule: Integer;
begin
  SetLength(Result, 0);

  // Copy input data so we can process in our own thread

  ims := TMMImages.Create;

  m0 := images_in.Clone;
  try
    m0.OwnsObjects := False;
    ims.AddRange(m0);
  finally
    m0.Free;
  end;

  FFirstKernelModule := ims.Count;

  m0 := FSystemModules.Clone;
  try
    m0.OwnsObjects := False;
    ims.AddRange(m0);
  finally
    m0.Free;
  end;

  // Push this data into our thread for processing
  (FThread as TStackRenderThread).QueueProcessing(event_id, FFirstKernelModule, ims, stack_in);
end;

{ TStackRenderThread - Main Thread Context }

procedure TStackRenderThread.CleanupSymbols;
begin
  if FDbghelpLoaded then
  begin
    if not SymCleanup(hSymProcess) then
      RaiseLastOSError;
    UnloadDbghelp;
  end;
  FSymbolFiles.Free;
end;

type
  TString = class
    str: string;
    constructor Create(str: string);
  end;

constructor TStackRenderThread.Create(Owner: TStackRenderer; const ADbghelpPath, ASymbolPath: string);
begin
  FDbghelpPath := ADbghelpPath;
  FSymbolPath := ASymbolPath;
  FLock := TCriticalSection.Create;
  FOwner := Owner;
  FDataEvent := TEvent.Create(nil, False, False, '');
  FResultWnd := AllocateHwnd(DataResultProc);
  inherited Create(False);
end;

procedure TStackRenderThread.DataResultProc(var Message: TMessage);
var
  srr: TStackRenderResult;
  s: TString;
begin
  try
    case Message.Msg of
      WM_USER:
        begin
          srr := TStackRenderResult(Message.LParam);
          FOwner.DataResult(srr.event_id, srr.Rows);
          srr.Free;
        end;
      WM_USER+1:
        begin
          s := TString(Message.LParam);
          if Assigned(FOwner.FOnEvent) then
            FOwner.FOnEvent(FOwner, s.Str);
          s.Free;
        end;
      WM_USER+2:
        if Assigned(FOwner.FOnEvent) then
          FOwner.FOnEvent(FOwner, '');
    end;

    Message.Result := DefWindowProc(FResultWnd, Message.Msg, Message.wParam, Message.lParam);
  except
    ;
  end;
end;

destructor TStackRenderThread.Destroy;
begin
  FDataEvent.SetEvent;
  inherited Destroy;
  DeallocateHWnd(FResultWnd);
  FreeAndNil(FLock);
end;

procedure TStackRenderThread.QueueProcessing(
  event_id: Int64;
  firstKernelModule: Integer;
  ims: TMMImages;
  stack: TArrayOfByte);
var
  sri: TStackRenderInput;
begin
  // Add another stack to process. Wipes all other stacks
  sri := TStackRenderInput.Create(ims, firstKernelModule, stack, event_id);
  FLock.Enter;
  try
    FreeAndNil(FNext);
    FNext := sri;
  finally
    FLock.Leave;
  end;
  FDataEvent.SetEvent;
end;

{ TStackRenderThread - Execution Thread Context }

procedure TStackRenderThread.SetupSymbols;
begin
  FSymbolFiles := TDictionary<string,string>.Create;
  hSymProcess := 1; // 1 ensures it's a fake handle because it's not div by 4
  FDbghelpLoaded := LoadDbgHelp(FDbghelpPath);
  if FDbghelpLoaded then
  begin
    SymSetOptions(SYMOPT_UNDNAME or SYMOPT_DEFERRED_LOADS or SYMOPT_INCLUDE_32BIT_MODULES or SYMOPT_DEBUG);
    if not SymInitialize(hSymProcess, PChar(FSymbolPath), False) then
      RaiseLastOSError;
    SymRegisterCallback64(hSymProcess, SymCallback_, DWORD(Self));
  end;
end;

procedure TStackRenderThread.Execute;
var
  srr: TStackRenderResult;
begin
  SetupSymbols;
  try
    repeat
      if Terminated then
        Exit;

      FDataEvent.WaitFor(INFINITE);

      FLock.Enter;
      try
        if FNext = nil then
          Continue;

        FCurrent := FNext;
        FNext := nil;
      finally
        FLock.Leave;
      end;

      srr := nil;
      try
        if Terminated or (FCurrent = nil) or (FCurrent.event_id = 0) then
          Exit;
        srr := ProcessStack;

        PostMessage(FResultWnd, WM_USER+2, 0, 0); // clear status

      finally
        FreeAndNil(FCurrent);
      end;

      if Terminated then
      begin
        FreeAndNil(srr);
        Exit;
      end;

      if Assigned(srr) then
        PostMessage(FResultWnd, WM_USER, 0, LParam(srr));

    until False;
  finally
    CleanupSymbols;
  end;
end;

function TStackRenderThread.ProcessStack: TStackRenderResult;
var
  len: Integer;
  i: Integer;
  p: PUInt64;
  d: UInt64;
  s: PSymbolInfo;
  base: array of UInt64;
  im: TMMImage;
  j: Integer;
  r: TStackRow;
  foundfile: array[0..260] of char;
  SymbolFilename: string;
  FRows: TStackRows;
const
  MaxNameInfo = 100;
begin
  Result := nil;
  // At this point, we have a stack to work with

  len := Length(FCurrent.stack) div 8; // we have setup the stack as 8-byte aligned even on 32-bit systems
  SetLength(base, FCurrent.images.Count);
  FillChar(base[0], sizeof(UInt64) * FCurrent.images.Count, 0);

  p := @FCurrent.stack[0];
  for i := 0 to len - 1 do
  begin

    if (FNext <> nil) or Terminated then
      Exit;

    for j := 0 to FCurrent.images.Count - 1 do
    begin
      im := FCurrent.images[j];
      if (im.imagebase <= p^) and (p^ < im.ImageBase + im.imagesize) then
      begin
        if base[j] = 0 then
        begin
          // https://gregsplaceontheweb.wordpress.com/2015/08/15/how-to-download-windows-image-files-from-the-microsoft-symbol-server-using-c-and-dbghelp/
          if not FSymbolFiles.TryGetValue(im.Filename, SymbolFilename) then
          begin
            if SymFindFileInPath(hSymProcess, nil, PChar(ExtractFileName(im.filename)),
              Pointer(im.timedatestamp), im.imagesize, 0, SSRVOPT_DWORD, foundfile, nil, nil) then
            begin
              FSymbolFiles.Add(im.filename, foundfile);
              base[j] := SymLoadModuleExW(hSymProcess, 0, foundfile, nil, im.imagebase, 0, nil, 0);
            end
            else
            begin
              FSymbolFiles.Add(im.filename, '');
              base[j] := im.imagebase;
            end;
          end
          else
          begin
            if SymbolFilename <> ''
              then base[j] := SymLoadModuleExW(hSymProcess, 0, PChar(SymbolFilename), nil, im.imagebase, 0, nil, 0)
              else base[j] := im.imagebase;
          end;
        end;
        Break;
      end;
    end;
    Inc(p);
  end;

  // TODO: Note: we are not currently supporting images unloaded and reloaded at a new base address
  // Or where one image is unloaded and another is loaded at an overlapping address
  p := @FCurrent.stack[0];
  s := AllocMem(sizeof(TSymbolInfo) + MaxNameInfo);

  SetLength(FRows, len);

  for i := 0 to len - 1 do
  begin

    if (FNext <> nil) or Terminated then
      Exit;

    FillChar(s^, sizeof(TSymbolInfo) + MaxNameInfo, 0);
    s.SizeOfStruct := sizeof(TSymbolInfo);
    s.MaxNameLen := MaxNameInfo;
    im := nil;

    r.IsKernel := False;

    for j := 0 to FCurrent.images.Count - 1 do
    begin
      if (FCurrent.images[j].imagebase <= p^) and (p^ < FCurrent.images[j].ImageBase + FCurrent.images[j].imagesize) then
      begin
        im := FCurrent.images[j];
        r.IsKernel := j >= FCurrent.firstKernelModule;
        Break;
      end;
    end;

    r.Frame := i;
    if Assigned(im)
      then r.Module := ExtractFileName(im.filename)
      else r.Module := 'Unknown';

    if FDbghelpLoaded and SymFromAddr(hSymProcess, p^, @d, s) then
    begin
      if d > 0
        then r.Location := Format('%s + 0x%x', [s.Name, d])
        else r.Location := s.Name;
    end
    else
    begin
      if Assigned(im)
        then r.Location := Format('%s + 0x%x', [r.Module, p^ - im.imagebase])
        else r.Location := 'Unknown'

{$IFDEF STACK_DEBUGGING}
      r.Location := r.Location + Format(' [%d:%s]', [GetLastError, SysErrorMessage(GetLastError)]);
{$ENDIF}
    end;

    r.Address := p^;
    if Assigned(im)
      then r.Path := im.filename
      else r.Path := '';

    FRows[i] := r;
    Inc(p);
  end;

  Result := TStackRenderResult.Create(FCurrent.event_id, FRows);
end;

function TStackRenderThread.SymCallback(hProcess: THandle; ActionCode: ULONG; CallbackData: ULONG64; UserContext: ULONG64): BOOL;
var
  evt: PIMAGEHLP_CBA_EVENT;
  s: TString;
begin
  if ActionCode = CBA_EVENT then
  begin
    evt := PIMAGEHLP_CBA_EVENT(CallbackData);
    s := TString.Create(evt.desc);
    PostMessage(FResultWnd, WM_USER+1, 0, LParam(s));
    Result := True;
  end
  else
    Exit(False);
end;

{ TStackRenderInput }

constructor TStackRenderInput.Create(images: TMMImages; firstKernelModule: Integer; stack: TArrayOfByte;
  event_id: Int64);
begin
  inherited Create;
  Self.images := images;
  Self.firstKernelModule := firstKernelModule;
  Self.stack := stack;
  Self.event_id := event_id;
end;

destructor TStackRenderInput.Destroy;
begin
  FreeAndNil(images);
  inherited Destroy;
end;

{ TStackRenderResult }

constructor TStackRenderResult.Create(event_id: Int64; rows: TStackRows);
begin
  Self.event_id := event_id;
  Self.rows := rows;
end;

{ TString }

constructor TString.Create(str: string);
begin
  Self.str := str;
end;

end.
