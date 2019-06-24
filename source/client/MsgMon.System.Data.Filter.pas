unit MsgMon.System.Data.Filter;

interface

uses
  System.Generics.Collections,
  System.JSON,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context;

type
  TMMFilter = class
  public
    column: TMMColumn;
    relation: TMMFilterRelation;
    value: string;
    action: TMMFilterAction;
  end;

  TMMFilters = class(TObjectList<TMMFilter>)
  private
    FContext: TMMDataContext;
    FColumns: TMMColumns;
  public
    constructor Create(AContext: TMMDataContext);
    destructor Destroy; override;
    procedure LoadDefault;
    procedure LoadFromJSON(o: TJSONObject);
    procedure SaveToJSON(o: TJSONObject);
    property Columns: TMMColumns read FColumns;
  end;

const
  MMFilterRelationName: array[TMMFilterRelation] of string =
    ('is', 'is not', 'less than', 'more than', 'begins with', 'ends with', 'contains', 'excludes');

  MMFilterActionName: array[TMMFilterAction] of string =
    ('Include', 'Exclude');

implementation

uses
  MsgMon.System.Data.Message;

{ TMMFilters }

constructor TMMFilters.Create(AContext: TMMDataContext);
begin
  inherited Create;
  FContext := AContext;
  FColumns := TMMColumns.Create(AContext);
  FColumns.LoadAll;
end;

destructor TMMFilters.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

procedure TMMFilters.LoadDefault;
var
  f: TMMFilter;
begin
  // Exclude msgmon by default
  Clear;
  f := TMMFilter.Create;
  f.column := FColumns.FindClassName(TMMColumn_ProcessName.ClassName);
  Assert(Assigned(f.column));
  f.relation := frBeginsWith;
  f.value := 'msgmon';
  f.action := faExclude;
  Add(f);
end;

procedure TMMFilters.LoadFromJSON(o: TJSONObject);
begin

end;

procedure TMMFilters.SaveToJSON(o: TJSONObject);
begin

end;

end.
