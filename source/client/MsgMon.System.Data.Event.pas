unit MsgMon.System.Data.Event;

interface

type
  TMMEvent = class
  public
    timestamp: Int64;
    pid: Integer;
    tid: Integer;
    event_id: Int64;
    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64);
  end;

implementation

{ TMMEvent }

constructor TMMEvent.Create(timestamp: Int64; pid, tid: Integer;
  event_id: Int64);
begin
  inherited Create;
  Self.timestamp := timestamp;
  Self.pid := pid;
  Self.tid := tid;
  Self.event_id := event_id;
end;

end.
