unit MsgMon.System.Data.Message;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
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
    detail: string;
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
      const detail: string
    );
  end;

type
  TMMMessages = class(TObjectList<TMMMessage>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

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
  const detail: string
);
begin
  inherited Create;
  index := AIndex;

  Self.pid := pid;
  Self.tid := tid;
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
  Self.detail := detail;
end;

procedure TMMMessage.Fill(processes: TMMProcessDictionary; windows: TMMWindowDictionary; messageNames: TMMMessageNameDictionary);
var
  name, value: string;
  valueInt: Int64;
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
