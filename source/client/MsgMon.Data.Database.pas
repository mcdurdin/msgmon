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
  MsgMon.System.Data.Thread,
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
    stmtWindowBase: TSQLite3Statement;
    stmtProcessBase: TSQLite3Statement;
    stmtThreadBase: TSQLite3Statement;
    procedure Load;
    procedure SaveFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadColumns;
    procedure SaveColumns;
    function DoLoadMessageRow(stmt: TSQLite3Statement): TMMMessage;
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

const
  EVENT_CX = 5;
  MESSAGE_CX = 9;
  WINDOW_CX = 9;
  THREAD_CX = 10;
  PROCESS_CX = 6;

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
//  ws: TMMWindows;
//  ps: TMMProcesses;
  stmt: TSQLite3Statement;
//  i: Integer;
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

  stmtWindowBase := TSQLite3Statement.Create(db,
    'select '+
    '  w.*, e.* '+
    'from '+
    '  Window w inner join '+
    '  Event e on w.event_id = e.event_id '+
    'where '+
    '  w.event_id < ? and '+
    '  w.hwnd = ? '+
    'limit 1'
  );

  Assert(stmtWindowBase.ColumnCount = WINDOW_CX + EVENT_CX);

  stmtThreadBase := TSQLite3Statement.Create(db,
    'select '+
    '  t.*, e.* '+
    'from '+
    '  Thread t inner join '+
    '  Event e on t.event_id = e.event_id '+
    'where '+
    '  t.event_id < ? and '+
    '  e.tid = ? '+ // TODO: Add tid to thread table?
    'limit 1'
  );

  Assert(stmtThreadBase.ColumnCount = THREAD_CX + EVENT_CX);

  stmtProcessBase := TSQLite3Statement.Create(db,
    'select '+
    '  p.*, e.* '+
    'from '+
    '  Process p inner join '+
    '  Event e on p.event_id = e.event_id '+
    'where '+
    '  p.event_id < ? and '+
    '  p.pid = ? '+
    'limit 1'
  );

  Assert(stmtProcessBase.ColumnCount = PROCESS_CX + EVENT_CX);

  LoadFilter(session.filters, ftFilter);
  LoadFilter(session.highlights, ftHighlight);
  LoadColumns;

  // Load Messages<!>: This must match LoadMessageRow


  stmtMessage := TSQLite3Statement.Create(db,
    'select '+
    '  m.*, e.*, fk.filter_row '+
    'from '+
    '  FilterKey fk inner join '+
    '  Message m on fk.row = m.Row inner join '+
    '  Event e on m.event_id = e.event_id '+
    'where '+
    '  fk.filter_id = ? and '+
    '  fk.filter_row = ? '+
    'order by '+
    '  m.event_id asc');


    {'SELECT Event.timestamp, Event.pid, Event.tid, Message.*, FilterKey.filter_row '+
    'FROM FilterKey '+
    'INNER JOIN Message on FilterKey.row = Message.row '+
    'INNER JOIN Event on Message.event_id = Event.event_id '+
    'WHERE filter_id = ? AND filter_row = ?');}

  Assert(stmtMessage.ColumnCount =
    MESSAGE_CX + EVENT_CX +
    1); // filter_row


  {TODO: add assertions
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
  Assert(stmtMessage.ColumnName(12) = 'filter_row');}

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
      m.thread.Free;
      m.process.Free;
      m.window.Free;
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

    // TODO: Eliminate unnecessary Event columns
      'select '+
      '  m.*, e.* '+
      'from '+
      '  Message m inner join '+
      '  Event e on m.event_id = e.event_id '+ // left join '+
//      '  (select w0.*, e0.* from Window w0 inner join Event e0 on w0.event_id = e0.event_id order by w0.event_id desc) w on w.event_id < m.event_id and w.hwnd = m.hwnd left join '+
//      '  (select p0.*, e0.* from Process p0 inner join Event e0 on p0.event_id = e0.event_id order by p0.event_id desc) p on p.event_id < m.event_id and p.pid = e.pid left join '+
//      '  (select t0.*, e0.* from Thread t0 inner join Event e0 on t0.event_id = e0.event_id order by t0.event_id desc) t on t.event_id < m.event_id and t.tid = e.tid '+
      'order by '+
      '  m.event_id asc');
{
      'SELECT Event.timestamp, Event.pid, Event.tid, Message.* '+
      'FROM  Message '+
      'INNER JOIN Event on Message.event_id = Event.event_id ');
}
    try
      Assert(stmt.ColumnCount =
        MESSAGE_CX + EVENT_CX);
//        WINDOW_CX + EVENT_CX +
//        PROCESS_CX + EVENT_CX +
//        THREAD_CX + EVENT_CX);

      while stmt.Step <> SQLITE_DONE do
      begin
        m := DoLoadMessageRow(stmt);
        try
          if DoesFilterMatchMessage(session.filters, m) then
          begin                                                                                   // TODO rename index to row
            db.Execute('INSERT INTO FilterKey (filter_id, filter_row, row) VALUES ('+Ord(ftFilter).ToString+', '+IntToStr(row)+', '+IntToStr(m.index)+')');
            Inc(row);
          end;
        finally
          m.thread.Free;
          m.process.Free;
          m.window.Free;
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

function TMMDatabase.DoLoadMessageRow(stmt: TSQLite3Statement): TMMMessage;
var
  w: TMMWindow;
  p: TMMProcess;
  t: TMMThread;
begin
  w := nil;
  p := nil;
  t := nil;
{
    'SELECT Event.timestamp, Event.pid, Event.tid, Message.* '+
    'FROM  Message '+
    'INNER JOIN Event on Message.event_id = Event.event_id ');
}
  Result := TMMMessage.Create(
    stmt.ColumnInt64(MESSAGE_CX + 1),        //timestamp
    stmt.ColumnInt(MESSAGE_CX + 2),          //'pid');
    stmt.ColumnInt(MESSAGE_CX + 3),          //'tid');
    stmt.ColumnInt64(MESSAGE_CX + 0),        //event_id

    stmt.ColumnInt(1),          //'row'); // source row, not target row
    stmt.ColumnInt(2),         // 'hwnd');
    stmt.ColumnInt(3),         // 'message');
    stmt.ColumnInt64(4),       // 'wParam');
    stmt.ColumnInt64(5),       // 'lParam');
    stmt.ColumnInt64(6),       // 'lResult');
    stmt.ColumnInt(7),         // 'mode');
    stmt.ColumnText(8)         // 'detail');
  );

  // Do Window, Process and Thread loads

  stmtWindowBase.BindInt(1, Result.event_id);
  stmtWindowBase.BindInt(2, Result.hwnd);
  if stmtWindowBase.Step = SQLITE_ROW then
  begin
    w := TMMWindow.Create(
      stmtWindowBase.ColumnInt64(WINDOW_CX + 1),        //timestamp
      stmtWindowBase.ColumnInt(WINDOW_CX + 2),          //'pid');
      stmtWindowBase.ColumnInt(WINDOW_CX + 3),          //'tid');
      stmtWindowBase.ColumnInt64(WINDOW_CX + 0),        //event_id

      stmtWindowBase.ColumnInt(2), // hwnd
      stmtWindowBase.ColumnInt(3), // ownerPid
      stmtWindowBase.ColumnInt(4), // ownerTid
      stmtWindowBase.ColumnInt(5), // hwndOwner
      stmtWindowBase.ColumnInt(6), // hwndParent
      stmtWindowBase.ColumnText(7), // className
      stmtWindowBase.ColumnText(8)  // realClassName
    );
  end;
  stmtWindowBase.Reset;

  stmtThreadBase.BindInt(1, Result.event_id);
  stmtThreadBase.BindInt(2, Result.tid);
  if stmtThreadBase.Step = SQLITE_ROW then
  begin
    t := TMMThread.Create(
      stmtThreadBase.ColumnInt64(THREAD_CX + 1),        //timestamp
      stmtThreadBase.ColumnInt(THREAD_CX + 2),          //'pid');
      stmtThreadBase.ColumnInt(THREAD_CX + 3),          //'tid');
      stmtThreadBase.ColumnInt64(THREAD_CX + 0),        //event_id

      stmtThreadBase.ColumnInt(2), // isForegroundThread
      stmtThreadBase.ColumnInt(3), // hwndFocus
      stmtThreadBase.ColumnInt(4), // hwndActive
      stmtThreadBase.ColumnInt(5), // hwndCapture
      stmtThreadBase.ColumnInt(6), // hwndCaret
      stmtThreadBase.ColumnInt(7), // hwndMenuOwner
      stmtThreadBase.ColumnInt(8), // hwndMoveSize
      stmtThreadBase.ColumnInt(9) // hwndMoveSize
    );
  end;
  stmtThreadBase.Reset;

  stmtProcessBase.BindInt(1, Result.event_id);
  stmtProcessBase.BindInt(2, Result.pid);
  if stmtProcessBase.Step = SQLITE_ROW then
  begin
    p := TMMProcess.Create(
      stmtProcessBase.ColumnInt64(PROCESS_CX + 1),        //timestamp
      stmtProcessBase.ColumnInt(PROCESS_CX + 2),          //'pid');
      stmtProcessBase.ColumnInt(PROCESS_CX + 3),          //'tid');
      stmtProcessBase.ColumnInt64(PROCESS_CX + 0),        //event_id

      stmtProcessBase.ColumnInt(2), // ownerPid
      stmtProcessBase.ColumnInt(3), // platform_
      stmtProcessBase.ColumnText(4), // processPath
      stmtProcessBase.ColumnText(5)  // commandLine
    );
  end;
  stmtProcessBase.Reset;

  Result.Fill(p, t, w);
end;

function TMMDatabase.LoadMessageRow(index: Integer): TMMMessage;
begin
  stmtMessage.BindInt(1, Ord(ftFilter)); // filter_id
  stmtMessage.BindInt(2, index); // filter_row

  if stmtMessage.Step <> SQLITE_ROW then
  begin
    raise Exception.Create('Invalid');
  end;

  Result := DoLoadMessageRow(stmtMessage);

  stmtMessage.Reset;
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
