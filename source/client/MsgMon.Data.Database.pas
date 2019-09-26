unit MsgMon.Data.Database;

interface

uses
  System.Classes,
  System.SysUtils,

  SQLite3,
  SQLite3Utils,
  SQLite3Wrap,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Window;

type
  TMMFilterType = (ftFilter = 1, ftHighlight = 2);

  TMMDatabase = class
  private
    FFilename: string;
    db: TSQLite3Database;

    FContext: TMMDataContext;
    FSession: TMMSession;

    FTotalRowCount: Integer;
    FFilteredRowCount: Integer;
    stmtMessage: TSQLite3Statement;
    procedure Load;
    procedure SaveFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadColumns;
    procedure SaveColumns;
  public
    constructor Create(const AFilename, ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition: string);
    destructor Destroy; override;

    procedure ApplyFilter;
    procedure InitializeFilter(filters: TMMFilters);
    function DoesFilterMatchMessage(filters: TMMFilters; m: TMMMessage): Boolean;

    function LoadMessageRow(index: Integer): TMMMessage;
    function FindText(text: string; Row: Integer; FindDown: Boolean): Integer;

    property TotalRowCount: Integer read FTotalRowCount;
    property FilteredRowCount: Integer read FFilteredRowCount;
    property Session: TMMSession read FSession;
    property Context: TMMDataContext read FContext;
    property Filename: string read FFilename;
  end;

implementation

{ TMMDatabase }

constructor TMMDatabase.Create(const AFilename, ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition: string);
begin
  inherited Create;
  FFilename := AFilename;
  FContext := TMMDataContext.Create;
  FSession := TMMSession.Create(FContext);
  session.LoadDefault(ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition);

  Load;
end;

destructor TMMDatabase.Destroy;
begin
  SaveColumns;
  FreeAndNil(db);
  FreeAndNil(FSession);
  FreeAndNil(FContext);
  inherited Destroy;
end;

procedure TMMDatabase.Load;
var
  w: TMMWindow;
  p: TMMProcess;
  ws: TMMWindows;
  ps: TMMProcesses;
  stmt: TSQLite3Statement;
  i: Integer;
begin
  Assert(FileExists(FFilename));

  db := TSQLite3Database.Create;
  db.Open(FFilename);

  db.Execute('CREATE TABLE IF NOT EXISTS FilterKey (filter_id INT, filter_row INT, row INT)');
  db.Execute('CREATE INDEX IF NOT EXISTS ix_FilterKey_filterid ON FilterKey (filter_id, filter_row)');
  db.Execute('CREATE TABLE IF NOT EXISTS Filter (filter_id INT, definition TEXT)');
  db.Execute('CREATE TABLE IF NOT EXISTS Settings (id TEXT, value TEXT)');

  stmt := TSQLite3Statement.Create(db, 'SELECT COUNT(*) FROM Message');
  try
    stmt.Step;
    FTotalRowCount := stmt.ColumnInt(0);
  finally
    stmt.Free;
  end;

  FContext.Clear;

  // Load windows

  {stmt := TSQLite3Statement.Create(db, 'SELECT * FROM Window');
  try
    Assert(stmt.ColumnCount = 8);
    Assert(stmt.ColumnName(0) = 'row');
    Assert(stmt.ColumnName(1) = 'hwnd');
    Assert(stmt.ColumnName(2) = 'pid');
    Assert(stmt.ColumnName(3) = 'tid');
    Assert(stmt.ColumnName(4) = 'hwndOwner');
    Assert(stmt.ColumnName(5) = 'hwndParent');
    Assert(stmt.ColumnName(6) = 'className');
    Assert(stmt.ColumnName(7) = 'realClassName');

    while stmt.Step <> SQLITE_DONE do
    begin
      for i := 0 to stmt.ColumnCount - 1 do
      begin
        w := TMMWindow.Create(
          stmt.ColumnInt(1),
          stmt.ColumnInt(2),
          stmt.ColumnInt(3),
          stmt.ColumnInt(4),
          stmt.ColumnInt(5),
          stmt.ColumnText(6),
          stmt.ColumnText(7)
          , 0 // TODO: add base offset to database
        );
        if not FContext.Windows.TryGetValue(stmt.ColumnInt(1), ws) then
        begin
          ws := TMMWindows.Create;
          FContext.Windows.Add(stmt.ColumnInt(1), ws);
        end;
        ws.Add(w);
      end;
    end;
  finally
    stmt.Free;
  end;}

  {stmt := TSQLite3Statement.Create(db, 'SELECT pid FROM Event GROUP BY pid');
  try
    Assert(stmt.ColumnCount = 5);
    Assert(stmt.ColumnName(0) = 'row');
    Assert(stmt.ColumnName(1) = 'pid');
    Assert(stmt.ColumnName(2) = 'platform');
    Assert(stmt.ColumnName(3) = 'process');
    Assert(stmt.ColumnName(4) = 'commandLine');

    while stmt.Step <> SQLITE_DONE do
    begin
      for i := 0 to stmt.ColumnCount - 1 do
      begin
        p := TMMProcess.Create(
          s
          stmt.ColumnInt(1),
          stmt.ColumnInt(2),
          stmt.ColumnText(3),
          stmt.ColumnText(4)
          , 0 // TODO: add base offset to database
        );
        if not FContext.Processes.TryGetValue(stmt.ColumnInt(1), ps) then
        begin
          ps := TMMProcesses.Create;
          FContext.Processes.Add(stmt.ColumnInt(1), ps);
        end;
        ps.Add(p);
      end;
    end;
  finally
    stmt.Free;
  end;}

  LoadFilter(session.filters, ftFilter);
  LoadFilter(session.highlights, ftHighlight);
  LoadColumns;

  // Load Messages<!>: This must match LoadMessageRow

  stmtMessage := TSQLite3Statement.Create(db,
    'SELECT Event.timestamp, Event.pid, Event.tid, Message.*, FilterKey.filter_row '+
    'FROM FilterKey '+
    'INNER JOIN Message on FilterKey.row = Message.row '+
    'INNER JOIN Event on Message.event_id = Event.event_id '+
    'WHERE filter_id = ? AND filter_row = ?');
  Assert(stmtMessage.ColumnCount = 13);
  Assert(stmtMessage.ColumnName(0) = 'timestamp');
  Assert(stmtMessage.ColumnName(1) = 'pid');
  Assert(stmtMessage.ColumnName(2) = 'tid');
  Assert(stmtMessage.ColumnName(3) = 'event_id');
  Assert(stmtMessage.ColumnName(4) = 'row');
  Assert(stmtMessage.ColumnName(5) = 'hwnd');
  Assert(stmtMessage.ColumnName(6) = 'message');
  Assert(stmtMessage.ColumnName(7) = 'wParam');
  Assert(stmtMessage.ColumnName(8) = 'lParam');
  Assert(stmtMessage.ColumnName(9) = 'lResult');
  Assert(stmtMessage.ColumnName(10) = 'mode');
  Assert(stmtMessage.ColumnName(11) = 'detail');
  Assert(stmtMessage.ColumnName(12) = 'filter_row');

//  ApplyFilter;

  FContext.PrepareTrees;
end;

procedure TMMDatabase.InitializeFilter(filters: TMMFilters);
var
  f: TMMFilter;
begin
  // Filtering for includes: if we have 'include' filters for a column,
  // then we exclude anything that doesn't match. If no 'include' filter
  // exists for a given column, then it is included by default.

  for f in filters do
    f.column.vIncludeDefault := True;

  for f in filters do
    if f.action = faInclude then f.column.vIncludeDefault := False;
end;

function TMMDatabase.DoesFilterMatchMessage(filters: TMMFilters; m: TMMMessage): Boolean;
var
  f: TMMFilter;
  vInclude, vExclude: Boolean;
begin
  vInclude := True;
  vExclude := False;

  for f in filters do
    f.column.vInclude := f.column.vIncludeDefault;

  for f in filters do
  begin
    if f.column.Filter(m, f.relation, f.value) then
      if f.action = faInclude then
        f.column.vInclude := True
      else
      begin
        vExclude := True;
        Break;
      end;
  end;

  for f in filters do
    vInclude := vInclude and f.column.vInclude;

  Result := vInclude and not vExclude;
end;

function TMMDatabase.FindText(text: string; Row: Integer; FindDown: Boolean): Integer;
var
  s: string;
  m: TMMMessage;
  col: TMMColumn;
begin
  // We search through the visible columns in the view. Otherwise it is too confusing?

  // TODO: for now, we do a naive scan. We may later do this via SQL.
  while (Row >= 0) and (Row < FFilteredRowCount) do
  begin
    m := LoadMessageRow(Row);
    try
      for col in FSession.displayColumns do
      begin
        s := col.Render(m);
        if Pos(text, s) > 0 then
          Exit(Row);
      end;
    finally
      m.Free;
    end;
    if FindDown
      then Inc(Row)
      else Dec(Row);
  end;
  Exit(-1);
end;

procedure TMMDatabase.ApplyFilter;
var
  m: TMMMessage;
  row: Integer;
  stmt: TSQLite3Statement;
begin
  row := 0;

  InitializeFilter(session.filters);

  //

  db.BeginTransaction;
  try
    SaveFilter(session.filters, ftFilter);
    db.Execute('DELETE FROM FilterKey WHERE filter_id = '+Ord(ftFilter).ToString);
    stmt := TSQLite3Statement.Create(db,
      'SELECT Event.timestamp, Event.pid, Event.tid, Message.* '+
      'FROM  Message '+
      'INNER JOIN Event on Message.event_id = Event.event_id ');
    try
      while stmt.Step <> SQLITE_DONE do
      begin
        m := TMMMessage.Create(
          stmtMessage.ColumnInt64(0),        //timestamp
          stmtMessage.ColumnInt(1),          //'pid');
          stmtMessage.ColumnInt(2),          //'tid');
          stmtMessage.ColumnInt64(3),        //event_id
          stmtMessage.ColumnInt(4),          //'row'); // source row, not target row
          stmtMessage.ColumnInt(5),         // 'hwnd');
          stmtMessage.ColumnInt(6),         // 'message');
          stmtMessage.ColumnInt64(7),       // 'wParam');
          stmtMessage.ColumnInt64(8),       // 'lParam');
          stmtMessage.ColumnInt64(9),       // 'lResult');
          stmtMessage.ColumnInt(10),         // 'mode');
          stmtMessage.ColumnText(11)
        );
        try
    //      stmt.Reset;

        //  m := context.FilteredMessages[Item.Index];
          m.Fill; //(FContext.Processes, FContext.Windows, FContext.MessageNames);

          if DoesFilterMatchMessage(session.filters, m) then
          begin                                                                                   // TODO rename index to row
            db.Execute('INSERT INTO FilterKey (filter_id, filter_row, row) VALUES ('+Ord(ftFilter).ToString+', '+IntToStr(row)+', '+IntToStr(m.index)+')');
            Inc(row);
          end;
        finally
          m.Free;
        end;
      end;
    finally
      stmt.Free;
    end;
  finally
    db.Commit;
  end;

  FFilteredRowCount := row;
end;

function TMMDatabase.LoadMessageRow(index: Integer): TMMMessage;
var
  m: TMMMessage;
begin
  stmtMessage.BindInt(1, Ord(ftFilter)); // filter_id
  stmtMessage.BindInt(2, index); // filter_row

  if stmtMessage.Step <> SQLITE_ROW then
  begin
    raise Exception.Create('Invalid');
  end;

  Assert(stmtMessage.ColumnName(0) = 'timestamp');
  Assert(stmtMessage.ColumnName(1) = 'pid');
  Assert(stmtMessage.ColumnName(2) = 'tid');
  Assert(stmtMessage.ColumnName(3) = 'event_id');
  Assert(stmtMessage.ColumnName(4) = 'row');

  m := TMMMessage.Create(
    stmtMessage.ColumnInt64(0),        //timestamp
    stmtMessage.ColumnInt(1),          //'pid');
    stmtMessage.ColumnInt(2),          //'tid');
    stmtMessage.ColumnInt64(3),        //event_id
    stmtMessage.ColumnInt(4),          //'row'); // source row, not target row
    stmtMessage.ColumnInt(5),         // 'hwnd');
    stmtMessage.ColumnInt(6),         // 'message');
    stmtMessage.ColumnInt64(7),       // 'wParam');
    stmtMessage.ColumnInt64(8),       // 'lParam');
    stmtMessage.ColumnInt64(9),       // 'lResult');
    stmtMessage.ColumnInt(10),         // 'mode');
    stmtMessage.ColumnText(11)
//    stmtMessage.ColumnBlob(16),        // 'detail');
//    stmtMessage.ColumnBytes(16)
  );

  stmtMessage.Reset;

//  m := context.FilteredMessages[Item.Index];
  m.Fill; //(FContext.Processes, FContext.Windows, FContext.MessageNames);
  Result := m;
end;

procedure TMMDatabase.SaveFilter(filters: TMMFilters; filter_id: TMMFilterType);
var
  stmt: TSQLite3Statement;
  definition: string;
const
  sql_SaveFilter =
    'INSERT INTO Filter (filter_id, definition) SELECT ?, ?';
begin
  filters.SaveToJSON(definition);
  db.Execute('DELETE FROM Filter WHERE filter_id = '+Ord(filter_id).ToString);
  stmt := TSQLite3Statement.Create(db, sql_SaveFilter);
  try
    stmt.BindInt(1, Ord(filter_id));
    stmt.BindText(2, definition);
    stmt.Step;
  finally
    stmt.Free;
  end;
end;

procedure TMMDatabase.LoadFilter(filters: TMMFilters; filter_id: TMMFilterType);
var
  stmt: TSQLite3Statement;
const
  sql_LoadFilter =
    'SELECT filter_id, definition FROM Filter WHERE filter_id = ?';
begin
  stmt := TSQLite3Statement.Create(db, sql_LoadFilter);
  try
    stmt.BindInt(1, Ord(filter_id));
    if stmt.Step <> SQLITE_DONE then
    begin
      if not filters.LoadFromJSON(stmt.ColumnText(1)) then
        filters.LoadDefault;
      ApplyFilter;
    end;
  finally
    stmt.Free;
  end;
end;

procedure TMMDatabase.SaveColumns;
var
  stmt: TSQLite3Statement;
  definition: string;
const
  sql_SaveColumns =
    'INSERT INTO Settings (id, value) SELECT ''columns'', ?';
begin
  session.displayColumns.SaveToJSON(definition);
  db.Execute('DELETE FROM Settings WHERE id = ''columns''');
  stmt := TSQLite3Statement.Create(db, sql_SaveColumns);
  try
    stmt.BindText(1, definition);
    stmt.Step;
  finally
    stmt.Free;
  end;
end;

procedure TMMDatabase.LoadColumns;
var
  stmt: TSQLite3Statement;
const
  sql_LoadColumns =
    'SELECT value FROM Settings where id=''columns''';
begin
  stmt := TSQLite3Statement.Create(db, sql_LoadColumns);
  try
    if stmt.Step <> SQLITE_DONE then
    begin
      if not session.displayColumns.LoadFromJSON(stmt.ColumnText(0)) then
        session.displayColumns.LoadDefault;
    end;
  finally
    stmt.Free;
  end;
end;

end.
