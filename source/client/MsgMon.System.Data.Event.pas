unit MsgMon.System.Data.Event;

interface

uses
  System.Generics.Collections;

type
  TMMDataObject = class;

  TMMDataObjects = class(TObjectList<TMMDataObject>)
  end;

  TMMDataObject = class
  private
    FChildren: TMMDataObjects;
    FOwner: TMMDataObject;
    //TODO refactor to another unit
  public
    constructor Create;
    destructor Destroy; override;
    property Owner: TMMDataObject read FOwner write FOwner;
    property Children: TMMDataObjects read FChildren;
  end;

  TMMEvent = class(TMMDataObject)
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

uses
  System.SysUtils;

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

{ TMMDataObject }

constructor TMMDataObject.Create;
begin
  inherited Create;
  FOwner := nil;
  FChildren := TMMDataObjects.Create(False); // we do not assume ownership...
end;

destructor TMMDataObject.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

end.
