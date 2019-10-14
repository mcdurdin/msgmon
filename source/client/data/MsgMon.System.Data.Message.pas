unit MsgMon.System.Data.Message;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TByteArray = TArray<Byte>;

  TMMMessageContext = record
  private
    FProcesses: TMMProcessDictionary;
    FWindows: TMMWindowDictionary;
    FThreads: TMMThreadDictionary;
  public
    property Threads: TMMThreadDictionary read FThreads;
    property Processes: TMMProcessDictionary read FProcesses;
    property Windows: TMMWindowDictionary read FWindows;
  end;

  TMMMessage = class(TMMEvent)
  private
    FContext: TMMMessageContext;
  public
    index: Integer;

    hwnd,
    message: DWORD;
    wParam, lParam, lResult: UINT64;

    mode: DWORD;
    detail: TByteArray;
    stack: string;

    process: TMMProcess;
    thread: TMMThread;
    window: TMMWindow;
    messageName: TMMMessageName;

    procedure Fill(messageNames: TMMMessageNameDictionary; process: TMMProcess; thread: TMMThread; window: TMMWindow;
      processes: TMMProcessDictionary;
      threads: TMMThreadDictionary;
      windows: TMMWindowDictionary);
    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;

      index,
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
    destructor Destroy; override;

    property Context: TMMMessageContext read FContext;
  end;

type
  TMMMessages = class(TObjectList<TMMMessage>)
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TMsgMonMessage }

constructor TMMMessage.Create(
  timestamp: Int64;
  pid,
  tid: Integer;
  event_id: Int64;

  index: Integer;
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
  inherited Create(timestamp, pid, tid, event_id);

  Self.index := index;

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

destructor TMMMessage.Destroy;
begin
  FreeAndNil(process);
  FreeAndNil(thread);
  FreeAndNil(window);
  inherited Destroy;
end;

procedure TMMMessage.Fill(
  messageNames: TMMMessageNameDictionary;
  process: TMMProcess;
  thread: TMMThread;
  window: TMMWindow;

  processes: TMMProcessDictionary;
  threads: TMMThreadDictionary;
  windows: TMMWindowDictionary);
begin
  Self.process := process;
  Self.thread := thread;
  Self.window := window;

  if not Assigned(messageNames) or not messageNames.TryGetValue(message, messageName)
    then messageName := nil;

  FContext.FWindows := windows;
  FContext.FProcesses := processes;
  FContext.FThreads := threads;
end;

end.
