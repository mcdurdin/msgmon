unit MsgMon.System.Data.Session;

interface

uses
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Column,
  MsgMon.System.Data.Search;

type
  TMMSession = class
  private
    FContext: TMMGlobalContext;
  public
    highlights, filters: TMMFilters;
    searches: TMMSearches;
    displayColumns: TMMColumns;
    allColumns: TMMColumns;
    constructor Create(AContext: TMMGlobalContext);
    destructor Destroy; override;
    procedure LoadDefault(ALastFilterDefinition, ALastHighlightDefinition, ALastColumnDefinition, ALastSearchDefinition: string);
  end;

implementation

{ TMMSession }

constructor TMMSession.Create(AContext: TMMGlobalContext);
begin
  inherited Create;
  FContext := AContext;
  filters := TMMFilters.Create(AContext, ftFilter);
  highlights := TMMFilters.Create(AContext, ftHighlight);
  displayColumns := TMMColumns.Create(AContext);
  allColumns := TMMColumns.Create(AContext);
  searches := TMMSearches.Create;
end;

destructor TMMSession.Destroy;
begin
  displayColumns.Free;
  allColumns.Free;
  filters.Free;
  highlights.Free;
  searches.Free;
end;

procedure TMMSession.LoadDefault(ALastFilterDefinition, ALastHighlightDefinition, ALastColumnDefinition, ALastSearchDefinition: string);
begin
  allColumns.LoadAll;

  if (ALastColumnDefinition = '') or
      not displayColumns.LoadFromJSON(ALastColumnDefinition)
    then displayColumns.LoadDefault;

  if (ALastFilterDefinition = '') or
      not filters.LoadFromJSON(ALastFilterDefinition)
    then filters.LoadDefault;

  if (ALastHighlightDefinition = '') or
      not highlights.LoadFromJSON(ALastHighlightDefinition)
    then highlights.LoadDefault;

  if (ALastSearchDefinition = '') or
      not searches.LoadFromJSON(ALastSearchDefinition)
    then searches.LoadDefault;
end;

end.
