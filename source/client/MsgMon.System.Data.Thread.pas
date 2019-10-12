unit MsgMon.System.Data.Thread;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Window;

type
  TMMThread = class(TMMEvent)
  private
    //FWindows: TMMWindowDictionary;
  public
    isForegroundThread: Boolean;
    hwndFocus,
    hwndActive,
    hwndCapture,
    hwndCaret,
    hwndMenuOwner,
    hwndMoveSize: DWORD;
    activeHKL: DWORD;

    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;

      isForegroundThread: Integer;
      hwndFocus,
      hwndActive,
      hwndCapture,
      hwndCaret,
      hwndMenuOwner,
      hwndMoveSize,
      activeHKL: Integer
    );
  end;

type
  TMMThreads = class(TDictionary<Cardinal, TMMThread>)
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TMsgMonThread }

constructor TMMThread.Create(
  timestamp: Int64;
  pid,
  tid: Integer;
  event_id: Int64;

  isForegroundThread: Integer;
  hwndFocus,
  hwndActive,
  hwndCapture,
  hwndCaret,
  hwndMenuOwner,
  hwndMoveSize,
  activeHKL: Integer
);
begin
  inherited Create(timestamp, pid, tid, event_id);

  Self.isForegroundThread := not(not LONGBOOL(isForegroundThread)); // Is this strictly necessary?
  Self.hwndFocus := hwndFocus;
  Self.hwndActive := hwndActive;
  Self.hwndCapture := hwndCapture;
  Self.hwndCaret := hwndCaret;
  Self.hwndMenuOwner := hwndMenuOwner;
  Self.hwndMoveSize := hwndMoveSize;
  Self.activeHKL := activeHKL;
end;

{constructor TMMThread.Create(ATID, APID: Cardinal);
begin
  inherited Create;
  FTID := ATID;
  FPID := APID;
  FWindows := TMMWindowDictionary.Create;
end;

destructor TMMThread.Destroy;
begin
  FreeAndNil(FWindows);
  inherited Destroy;
end;}

end.
