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
    function LoadFromJSON(definition: string): Boolean;
    procedure SaveToJSON(var definition: string);
    property Columns: TMMColumns read FColumns;
  end;

const
  MMFilterRelationName: array[TMMFilterRelation] of string =
    ('is', 'is not', 'less than', 'more than', 'begins with', 'ends with', 'contains', 'excludes');

  MMFilterActionName: array[TMMFilterAction] of string =
    ('Include', 'Exclude');

  MMFilterRelationID: array[TMMFilterRelation] of string =
    ('is', 'isNot', 'lessThan', 'moreThan', 'beginsWith', 'endsWith', 'contains', 'excludes');

  MMFilterActionID: array[TMMFilterAction] of string =
    ('include', 'exclude');

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

function TMMFilters.LoadFromJSON(definition: string): Boolean;
var
  o: TJSONObject;
  v: TJSONValue;
  i: Integer;
  c: TJSONArray;
  f: TMMFilter;
  r: TMMFilterRelation;
  a: TMMFilterAction;
begin
  Result := False;
  Clear;
  v := TJSONObject.ParseJSONValue(definition);
  try
    if not Assigned(v) or not (v is TJSONObject) then Exit;

    o := v as TJSONObject;
    v := o.Values['columns'];
    if not Assigned(v) or not (v is TJSONArray) then Exit;
    c := v as TJSONArray;
    for i := 0 to c.Count - 1 do
    begin
      if not (c.Items[i] is TJSONObject) then Exit;

      o := c.Items[i] as TJSONObject;

      f := TMMFilter.Create;
      f.column := FColumns.FindClassName(o.Values['className'].Value);
      if not Assigned(f.column) then
      begin
        f.Free;
        Exit;
      end;

      f.relation := frIs;
      for r := Low(TMMFilterRelation) to High(TMMFilterRelation) do
        if o.Values['relation'].Value = MMFilterRelationID[r] then
        begin
          f.relation := r;
          Break;
        end;

      f.value := o.Values['value'].Value;
      f.action := faInclude;
      for a := Low(TMMFilterAction) to High(TMMFilterAction) do
        if o.Values['action'].Value = MMFilterActionID[a] then
        begin
          f.action := a;
          Break;
        end;

      Add(f);
    end;

    Result := True;
  finally
    v.Free;

    if not Result then
      // If it failed to load, then reset the filter
      LoadDefault;
  end;
end;

procedure TMMFilters.SaveToJSON(var definition: string);
var
  o: TJSONObject;
  jf: TJSONObject;
  c: TJSONArray;
  f: TMMFilter;
begin
  // Export filters to a json object
  o := TJSONObject.Create;
  try
    c := TJSONArray.Create;
    o.AddPair('columns', c);
    for f in Self do
    begin
      jf := TJSONObject.Create;
      jf.AddPair('column', f.column.ClassName);
      jf.AddPair('relation', MMFilterRelationID[f.relation]);
      jf.AddPair('value', f.value);
      jf.AddPair('action', MMFilterActionID[f.action]);
      c.Add(jf);
    end;

    definition := o.ToString;
  finally
    o.Free;
  end;
end;

end.
