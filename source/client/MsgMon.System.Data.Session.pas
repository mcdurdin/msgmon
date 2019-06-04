unit MsgMon.System.Data.Session;

interface

uses
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Column;

type
  TMMSession = class
  private
    FContext: TMMDataContext;
  public
    filters: TMMFilters;
    displayColumns: TMMColumns;
    constructor Create(AContext: TMMDataContext);
    destructor Destroy; override;
  end;

implementation

{ TMMSession }

constructor TMMSession.Create(AContext: TMMDataContext);
begin
  inherited Create;
  FContext := AContext;
  filters := TMMFilters.Create(AContext);
  displayColumns := TMMColumns.Create(AContext);
end;

destructor TMMSession.Destroy;
begin
  displayColumns.Free;
  filters.Free;
end;

end.
