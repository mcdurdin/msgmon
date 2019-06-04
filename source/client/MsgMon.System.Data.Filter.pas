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
    procedure Apply;
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

procedure TMMFilters.Apply;
var
  m: TMMMessage;
  f: TMMFilter;
  v: Boolean;
begin
  FContext.FilteredMessages.Clear;
  for m in FContext.Messages do
  begin
    m.Fill(FContext.Processes, FContext.Windows, FContext.MessageNames);

    v := True;
    for f in Self do
    begin
      v := f.column.Filter(m, f.relation, f.value, f.action);
      if not v then
        Break;
    end;
    if v then
      FContext.FilteredMessages.Add(m);
  end;
end;

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
