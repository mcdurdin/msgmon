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

  TStackRenderer = class
  private
    hSymProcess: THandle;
    FDbghelpLoaded: Boolean;
    FSymbolFiles: TDictionary<string,string>;
    FSystemModules: TMMImages;
    FOnEvent: TStackRendererEvent;
    function SymCallback(hProcess: THandle; ActionCode: ULONG; CallbackData,
      UserContext: ULONG64): BOOL;
  protected
    procedure DataResult(event_id: Int64; rows: TStackRows);
  public
    constructor Create(const ADbghelpPath, ASymbolPath: string; ASystemModules: TMMImages);
    destructor Destroy; override;
    function Render(stack_in: TArrayOfByte; images_in: TMMImages): TStackRows;
    property DbghelpLoaded: Boolean read FDbghelpLoaded;
    property OnEvent: TStackRendererEvent read FOnEvent write FOnEvent;
  end;

implementation

uses
  MsgMon.Win.DbgHelp;

{ TStackRenderer }

function SymCallback_(hProcess: THandle; ActionCode: ULONG; CallbackData: ULONG64; UserContext: ULONG64): BOOL; stdcall;
begin
  Result := TStackRenderer(DWORD(UserContext)).SymCallback(hProcess, ActionCode, CallbackData, UserContext);
end;

constructor TStackRenderer.Create(const ADbghelpPath, ASymbolPath: string; ASystemModules: TMMImages);
begin
  inherited Create;
  FSymbolFiles := TDictionary<string,string>.Create;
  FSystemModules := ASystemModules.Clone;

  hSymProcess := 1; // 1 ensures it's a fake handle because it's not div by 4
  FDbghelpLoaded := LoadDbgHelp(ADbghelpPath);
  if FDbghelpLoaded then
  begin
    SymSetOptions(SYMOPT_UNDNAME or SYMOPT_DEFERRED_LOADS or SYMOPT_INCLUDE_32BIT_MODULES); // or SYMOPT_DEBUG);
    if not SymInitialize(hSymProcess, PChar(ASymbolPath), False) then
      RaiseLastOSError;
    SymRegisterCallback64(hSymProcess, SymCallback_, DWORD(Self));
  end;
end;

destructor TStackRenderer.Destroy;
begin
  if FDbghelpLoaded then
  begin
    if not SymCleanup(hSymProcess) then
      RaiseLastOSError;
    UnloadDbghelp;
  end;
  FSymbolFiles.Free;
  FSystemModules.Free;
  inherited Destroy;
end;

function TStackRenderer.Render(stack_in: TArrayOfByte; images_in: TMMImages): TStackRows;
var
  len: Integer;
  i: Integer;
  p: PUInt64;
  d: UInt64;
  s: PSymbolInfo;
  base: array of UInt64;
  stack: TArrayOfByte;
  m0, ims: TMMImages;
  im: TMMImage;
  j: Integer;
  r: TStackRow;
  FFirstKernelModule: Integer;
  foundfile: array[0..260] of char;
  SymbolFilename: string;
const
  MaxNameInfo = 100;
begin
  SetLength(Result, 0);

  // Copy input data so we can process in our own thread

  stack := stack_in;

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

  // TODO: Push this data into our thread for processing

  len := Length(stack) div 8; // we have setup the stack as 8-byte aligned even on 32-bit systems
  SetLength(base, ims.Count);
  FillChar(base[0], sizeof(UInt64) * ims.Count, 0);

  p := @stack[0];
  for i := 0 to len - 1 do
  begin
    for j := 0 to ims.Count - 1 do
      if (ims[j].imagebase <= p^) and (p^ < ims[j].ImageBase + ims[j].imagesize) then
      begin
        im := ims[j];
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
    Inc(p);
  end;

  // TODO: Note: we are not currently supporting images unloaded and reloaded at a new base address
  // Or where one image is unloaded and another is loaded at an overlapping address
  p := @stack[0];
  s := AllocMem(sizeof(TSymbolInfo) + MaxNameInfo);

  SetLength(Result, len);

  for i := 0 to len - 1 do
  begin
    FillChar(s^, sizeof(TSymbolInfo) + MaxNameInfo, 0);
    s.SizeOfStruct := sizeof(TSymbolInfo);
    s.MaxNameLen := MaxNameInfo;
    im := nil;

    r.IsKernel := False;

    for j := 0 to ims.Count - 1 do
      if (ims[j].imagebase <= p^) and (p^ < ims[j].ImageBase + ims[j].imagesize) then
      begin
        im := ims[j];
        r.IsKernel := j >= FFirstKernelModule;
        Break;
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
        then r.Location := Format('%s + 0x%x [%d:%s]', [r.Module, p^ - im.imagebase, GetLastError, SysErrorMessage(GetLastError)])
        else r.Location := Format('Unknown [%d:%s]', [GetLastError, SysErrorMessage(GetLastError)]);
    end;

    r.Address := p^;
    if Assigned(im)
      then r.Path := im.filename
      else r.Path := '';

    Result[i] := r;
    Inc(p);
  end;

  ims.Free;
end;

function TStackRenderer.SymCallback(hProcess: THandle; ActionCode: ULONG; CallbackData: ULONG64; UserContext: ULONG64): BOOL;
var
  evt: PIMAGEHLP_CBA_EVENT;
begin
  if ActionCode = CBA_EVENT then
  begin
    evt := PIMAGEHLP_CBA_EVENT(CallbackData);
    if Assigned(FOnEvent) then
      FOnEvent(Self, evt.desc);
    Result := True;
  end
  else
    Exit(False);
end;

type
  TStackRenderInput = class
  public
    images: TMMImages;
    stack: TArrayOfByte;
    event_id: Int64;
    constructor Create(images: TMMImages; stack: TArrayOfByte; event_id: Int64);
    destructor Destroy; override;
  end;

  TStackRenderResult = class
    event_id: Int64;
    rows: TStackRows;
    constructor Create(event_id: Int64; rows: TStackRows);
  end;

  TStackRenderThread = class(TThread)
  private
    FOwner: TStackRenderer;
    FResultWnd: THandle;
    FDataEvent: TEvent;
    FCurrent, FNext: TStackRenderInput;
    FLock: TCriticalSection;
    procedure DataResultProc(var Message: TMessage);
    function ProcessStack: TStackRenderResult;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TStackRenderer);
    destructor Destroy; override;
    procedure QueueProcessing(
      event_id: Int64;
      ims: TMMImages;
      stack: TArrayOfByte);
  end;

{ TStackRenderThread - Main Thread Context }

constructor TStackRenderThread.Create(Owner: TStackRenderer);
begin
  FLock := TCriticalSection.Create;
  FOwner := Owner;
  FDataEvent := TEvent.Create;
  FResultWnd := AllocateHwnd(DataResultProc);
  inherited Create(False);
end;

procedure TStackRenderThread.DataResultProc(var Message: TMessage);
var
  srr: TStackRenderResult;
begin
  try
    if Message.Msg = WM_USER then
    begin
      srr := TStackRenderResult(Message.LParam);
      FOwner.DataResult(srr.event_id, srr.Rows);
      srr.Free;
    end;

    Message.Result := DefWindowProc(FResultWnd, Message.Msg, Message.wParam, Message.lParam);
  except
    ;
  end;
end;

destructor TStackRenderThread.Destroy;
begin
  inherited Destroy;
  DeallocateHWnd(FResultWnd);
  FreeAndNil(FLock);
end;

procedure TStackRenderThread.QueueProcessing(
  input: TStackRenderInput);
begin
  // Add another stack to process. Wipes all other stacks
  FLock.Enter;
  try
    FreeAndNil(FNext);
    FNext := input;
  finally
    FLock.Leave;
  end;
  FDataEvent.SetEvent;
end;

{ TStackRenderThread - Execution Thread Context }

procedure TStackRenderThread.Execute;
var
  srr: TStackRenderResult;
begin
  repeat
    if Terminated then
      Exit;
    FDataEvent.WaitFor(INFINITE);
    if Terminated then
      Exit;
    FLock.Enter;
    try
      FreeAndNil(FCurrent);
      FCurrent := FNext;
      FNext := nil;
    finally
      FLock.Leave;
    end;

    srr := nil;
    try
      if Terminated then
        Exit;
      srr := ProcessStack;
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
end;

function TStackRenderThread.ProcessStack: TStackRenderResult;
var
  srr: TStackRenderResult;
begin
  // At this point, we have a stack to work with


  Result := TStackRenderResult.Create(FCurrent.event_id, FRows);
end;

{ TStackRenderInput }

constructor TStackRenderInput.Create(images: TMMImages; stack: TArrayOfByte;
  event_id: Int64);
begin
  inherited Create;
  Self.images := images;
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

end.
