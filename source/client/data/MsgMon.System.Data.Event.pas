unit MsgMon.System.Data.Event;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Types;

type
  TArrayOfByte = TArray<Byte>;

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
    stack: TArrayOfByte;
    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;
      const stack: string);
  end;

implementation

uses
  System.Classes;

{ TMMEvent }

constructor TMMEvent.Create(timestamp: Int64; pid, tid: Integer;
  event_id: Int64;
  const stack: string);
begin
  inherited Create;
  Self.timestamp := timestamp;
  Self.pid := pid;
  Self.tid := tid;
  Self.event_id := event_id;

  if Length(stack) > 0 then
  begin
    // detail is a hex string; convert to binary
    SetLength(Self.stack, Length(stack) div 2);
    HexToBin(PWideChar(stack), @Self.stack[0], Length(Self.stack));
  end;
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
