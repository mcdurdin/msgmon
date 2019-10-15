unit MsgMon.System.Data.Thread;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Window;

type
  TMMThread = class(TMMEvent)
  private
  public
    tidOwner: Integer;  // TODO: rename or eliminate
    isForegroundThread: Boolean;
    hwndFocus,
    hwndActive,
    hwndCapture,
    hwndCaret,
    hwndMenuOwner,
    hwndMoveSize: DWORD;
    activeHKL: DWORD;
    threadDescription: string;

    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;
      const stack: string;

      tidOwner: Integer;
      const threadDescription: string;
      isForegroundThread: Integer;
      hwndFocus,
      hwndActive,
      hwndCapture,
      hwndCaret,
      hwndMenuOwner,
      hwndMoveSize,
      activeHKL: Integer
    );
    function Render(IncludeTID: Boolean): string;
  end;

type
  TMMThreads = class(TObjectList<TMMThread>)
  end;

  TMMThreadDictionary = class(TDictionary<Cardinal, TMMThread>)
  end;

implementation

uses
  System.Classes;

{ TMsgMonThread }

constructor TMMThread.Create(
  timestamp: Int64;
  pid,
  tid: Integer;
  event_id: Int64;
  const stack: string;

  tidOwner: Integer;
  const threadDescription: string;
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
  inherited Create(timestamp, pid, tid, event_id, stack);

  Self.tidOwner := tidOwner;
  Self.threadDescription := threadDescription;
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

function TMMThread.Render(IncludeTID: Boolean): string;
begin
  if threadDescription = '' then
    Result := IntToStr(tid)
  else
  begin
    Result := threadDescription;

    if IncludeTID then
      Result := Result + ' ['+IntToStr(tid)+']';
  end;
end;

end.
