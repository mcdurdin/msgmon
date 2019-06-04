unit MsgMon.System.Data.Filter;

interface

uses
  System.Generics.Collections,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context;

type
  TMMFilter = class
  public
    column: TMMColumn;
    relation: TMMFilterRelation;
    valueStr: string;
    valueInt: Integer;
    action: TMMFilterAction;
  end;

  TMMFilters = class(TObjectList<TMMFilter>)
  private
    FContext: TMsgMonContext;
    FColumns: TMMColumns;
  public
    constructor Create(AContext: TMsgMonContext);
    destructor Destroy; override;
    procedure Apply;
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
  m: TMsgMonMessage;
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
      v := f.column.Filter(m, f.relation, f.valueStr, f.action);
      if not v then
        Break;
    end;
    if v then
      FContext.FilteredMessages.Add(m);
  end;
end;

constructor TMMFilters.Create(AContext: TMsgMonContext);
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

end.
