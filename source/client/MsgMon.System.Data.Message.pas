unit MsgMon.System.Data.Message;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  TByteArray = TArray<Byte>;

  TMMMessage = class
  strict private
  public
    index: Integer;

    pid, tid: DWORD;
    hwndFocus,
    hwndActive,
    hwndCapture,
    hwndCaret,
    hwndMenuOwner,
    hwndMoveSize: DWORD;

    activeHKL: DWORD;

    hwnd,
    message: DWORD;
    wParam, lParam, lResult: UINT64;

    mode: DWORD;
    detail: TByteArray;
    stack: string;

    process: TMMProcess;
    window: TMMWindow;
    messageName: TMMMessageName;

    procedure Fill(processes: TMMProcessDictionary; windows: TMMWindowDictionary; messageNames: TMMMessageNameDictionary);
    constructor Create(AIndex: Integer;
      pid,
      tid,
      hwndFocus,
      hwndActive,
      hwndCapture,
      hwndCaret,
      hwndMenuOwner,
      hwndMoveSize,
      activeHKL,
      hwnd,
      message: Integer;
      wParam,
      lParam,
      lResult: Int64;
      mode: Integer;
      detail: string
//      const detail: Pointer;
//      detailLength: Integer
    );
  end;

type
  TMMMessages = class(TObjectList<TMMMessage>)
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TMsgMonMessage }

constructor TMMMessage.Create(AIndex: Integer;
  pid,
  tid,
  hwndFocus,
  hwndActive,
  hwndCapture,
  hwndCaret,
  hwndMenuOwner,
  hwndMoveSize,
  activeHKL,
  hwnd,
  message: Integer;
  wParam,
  lParam,
  lResult: Int64;
  mode: Integer;
  detail: string
//  const detail: Pointer;
//  detailLength: Integer
);
begin
  inherited Create;
  index := AIndex;

  Self.pid := pid;
  Self.tid := tid;

  // This is conceptually state information that doesn't belong per-message
  // We should move that into state notifications.
  // thread-level state:
  //   IsForegroundThread? Set to true when GetWindowThreadProcessId(GetForegroundWindow) = GetCurrentThreadId
  //   hwndFocus, hwndActive, hwndCapture, hwndCaret, hwndMenuOwner, hwndMoveSize (GetGUIThreadInfo(GetCurrentThreadId)
  //   activeHKL
  //   window list.
  Self.hwndFocus := hwndFocus;
  Self.hwndActive := hwndActive;
  Self.hwndCapture := hwndCapture;
  Self.hwndCaret := hwndCaret;
  Self.hwndMenuOwner := hwndMenuOwner;
  Self.hwndMoveSize := hwndMoveSize;
  Self.activeHKL := activeHKL;

  Self.hwnd := hwnd;
  Self.message := message;
  Self.wParam := wParam;
  Self.lParam := lParam;
  Self.lResult := lResult;
  Self.mode := mode;

  if Length(detail) > 0 then
  begin
    // detail is a hex string; convert to binary
    SetLength(Self.detail, Length(detail) div 2);
    HexToBin(PWideChar(detail), @Self.detail[0], Length(Self.detail));
  end;

//  if detailLength > 0 then
//  begin
//    SetLength(Self.detail, detailLength);
//    CopyMemory(@Self.detail[0], detail, detailLength);
//  end;
end;

procedure TMMMessage.Fill(processes: TMMProcessDictionary; windows: TMMWindowDictionary; messageNames: TMMMessageNameDictionary);
var
  ps: TMMProcesses;
  ws: TMMWindows;
begin
  if process <> nil then
    Exit;

  // Lookup data from context

  if processes.TryGetValue(pid, ps)
    then process := ps.FromBase(index)
    else process := nil;

  if windows.TryGetValue(hwnd, ws)
    then window := ws.FromBase(index)
    else window := nil;

  if not messageNames.TryGetValue(message, messageName)
    then messageName := nil;
end;

end.
