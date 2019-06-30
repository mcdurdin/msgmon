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
    allColumns: TMMColumns;
    constructor Create(AContext: TMMDataContext);
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadDefault(ALastFilterDefinition, ALastColumnDefinition: string);
  end;

implementation

{ TMMSession }

constructor TMMSession.Create(AContext: TMMDataContext);
begin
  inherited Create;
  FContext := AContext;
  filters := TMMFilters.Create(AContext);
  displayColumns := TMMColumns.Create(AContext);
  allColumns := TMMColumns.Create(AContext);
end;

destructor TMMSession.Destroy;
begin
  displayColumns.Free;
  allColumns.Free;
  filters.Free;
end;

procedure TMMSession.LoadDefault(ALastFilterDefinition, ALastColumnDefinition: string);
begin
  allColumns.LoadAll;

  if (ALastColumnDefinition = '') or
      not displayColumns.LoadFromJSON(ALastColumnDefinition)
    then displayColumns.LoadDefault;

  if (ALastFilterDefinition = '') or
      not filters.LoadFromJSON(ALastFilterDefinition)
    then filters.LoadDefault;
end;

procedure TMMSession.LoadFromFile(const Filename: string);
begin

end;

procedure TMMSession.SaveToFile(const Filename: string);
begin

end;

end.
