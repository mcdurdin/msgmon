unit MsgMon.Data.Database;

interface

uses
  System.Classes,
  System.SysUtils,

  SQLite3,
  SQLite3Utils,
  SQLite3Wrap,

  MsgMon.System.ProgressManager,
  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Image,
  MsgMon.System.Data.Window;

type
  TMMFilterType = (ftFilter = 1, ftHighlight = 2);

  TMMDatabase = class
  private
    FFilename: string;
    FReady: Boolean;
    db: TSQLite3Database;

    FContext: TMMGlobalContext;
    FSession: TMMSession;

    FTotalRowCount: Integer;
    FFilteredRowCount: Integer;
    stmtMessage: TSQLite3Statement;
    stmtWindowBase: TSQLite3Statement;
    stmtProcessBase: TSQLite3Statement;
    stmtThreadBase: TSQLite3Statement;
    FHasStackTraces: Boolean;
    procedure Load;
    procedure SaveFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadFilter(filters: TMMFilters; filter_id: TMMFilterType);
    procedure LoadColumns;
    procedure SaveColumns;
    function DoLoadMessageRow(stmt: TSQLite3Statement; loadContext: Boolean): TMMMessage;
    function LoadWindowRow(stmt: TSQLite3Statement): TMMWindow;
    function LoadThreadRow(stmt: TSQLite3Statement): TMMThread;
    function LoadProcessRow(stmt: TSQLite3Statement): TMMProcess;
  public
    constructor Create(const AFilename, ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition: string);
    destructor Destroy; override;

    procedure ApplyFilter(Sender: IProgressUI);
    procedure InitializeFilter(filters: TMMFilters);
    function DoesFilterMatchMessage(filters: TMMFilters; m: TMMMessage): Boolean;

    function LoadMessageRow(index: Integer; loadContext: Boolean): TMMMessage;

    function LoadWindows(event_id: Int64): TMMWindowDictionary;
    function LoadProcesses(event_id: Int64): TMMProcessDictionary;
    function LoadThreads(event_id: Int64): TMMThreadDictionary;

    function LoadImages(pid: Integer): TMMImages;

    function FindText(progress: IProgressUI; text: string; Row: Integer; FindDown: Boolean): Integer;

    property HasStackTraces: Boolean read FHasStackTraces;
    property TotalRowCount: Integer read FTotalRowCount;
    property FilteredRowCount: Integer read FFilteredRowCount;
    property Session: TMMSession read FSession;
    property Context: TMMGlobalContext read FContext;
    property Filename: string read FFilename;
    property Ready: Boolean read FReady;
  end;

implementation

const
  // TODO: Refactor these into const arrays of fields
  EVENT_CX = 6;
  MESSAGE_CX = 9;
  WINDOW_CX = 9;
  THREAD_CX = 12;
  PROCESS_CX = 6;

{ TMMDatabase }

constructor TMMDatabase.Create(const AFilename, ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition: string);
begin
  inherited Create;
  FFilename := AFilename;
  FContext := TMMGlobalContext.Create;
  FSession := TMMSession.Create(FContext);
  session.LoadDefault(ALastFilterDefinition, ALastHighlightDefinition, ALastColumnsDefinition, ALastSearchDefinition);

  Load;
  FReady := True;
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
  stmt: TSQLite3Statement;
begin
  Assert(FileExists(FFilename));

  db := TSQLite3Database.Create;
  db.Open(FFilename);

  db.Execute('CREATE TABLE IF NOT EXISTS FilterKey (filter_id INT, filter_row INT, row INT)');
  db.Execute('CREATE INDEX IF NOT EXISTS ix_FilterKey_filterid ON FilterKey (filter_id, filter_row)');
  db.Execute('CREATE TABLE IF NOT EXISTS Filter (filter_id INT, definition TEXT)');
  db.Execute('CREATE TABLE IF NOT EXISTS Settings (id TEXT, value TEXT)');

  db.Execute('CREATE INDEX IF NOT EXISTS ix_Process_Event ON Process (pid, event_id)');
  db.Execute('CREATE INDEX IF NOT EXISTS ix_Thread_Event ON Thread (tid, event_id)');
  db.Execute('CREATE INDEX IF NOT EXISTS ix_Window_Event ON Window (hwnd, event_id)');

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
    '  t.tid = ? '+
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

  stmt := TSQLite3Statement.Create(db, 'SELECT COUNT(*) FROM Image');
  try
    stmt.Step;
    FHasStackTraces := stmt.ColumnInt(0) > 0;
  finally
    stmt.Free;
  end;
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

function TMMDatabase.FindText(progress: IProgressUI; text: string; Row: Integer; FindDown: Boolean): Integer;
var
  s: string;
  m: TMMMessage;
  col: TMMColumn;
  i: Integer;
begin
  // We search through the visible columns in the view. Otherwise it is too confusing?
  Result := -1;

  if Assigned(progress) then
  begin
    progress.Title := 'Searching';
    progress.Message := 'Finding '''+text+'''';
    progress.CanCancel := True;
    if FindDown
      then progress.Max := FFilteredRowCount - Row
      else progress.Max := Row;
  end;

  i := 0;

  // We do a flat scan because we are searching on the
  // transformed text presented by the renderer. This means
  // we cannot benefit from using SQL. This will probably be
  // okay, perf-wise.
  while (Row >= 0) and (Row < FFilteredRowCount) do
  begin
    m := LoadMessageRow(Row, False);
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

    Inc(i);
    if Assigned(progress) and ((i mod 100) = 0) then
    begin
      progress.Position := i;
      progress.Yield;
      if progress.Cancelled then
        Exit;
    end;
  end;
  Exit;
end;

procedure TMMDatabase.ApplyFilter(Sender: IProgressUI);
var
  m: TMMMessage;
  row: Integer;
  stmt: TSQLite3Statement;
  i: Integer;
begin
  row := 0;

  FReady := False;

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

    try
      Assert(stmt.ColumnCount =
        MESSAGE_CX + EVENT_CX);

      if Assigned(Sender) then
      begin
        Sender.Title := 'Applying filter';
        Sender.Message := 'Scanning '+IntToStr(FTotalRowCount)+' messages';
        Sender.Max := FTotalRowCount;
        Sender.CanCancel := True;
      end;
      i := 0;

      while stmt.Step <> SQLITE_DONE do
      begin
        m := DoLoadMessageRow(stmt, False);
        try
          if DoesFilterMatchMessage(session.filters, m) then
          begin                                                                                   // TODO rename index to row
            db.Execute('INSERT INTO FilterKey (filter_id, filter_row, row) VALUES ('+Ord(ftFilter).ToString+', '+IntToStr(row)+', '+IntToStr(m.index)+')');
            Inc(row);
          end;
        finally
          m.Free;
        end;
        Inc(i);
        if Assigned(Sender) and ((i mod 100) = 0) then
        begin
          Sender.Position := i;
          Sender.Yield;
          if Sender.Cancelled then
            // Note, this will mess up the filter!
            Exit;
        end;
      end;
    finally
      stmt.Free;
    end;
  finally
    db.Commit;
    FReady := True;
  end;

  FFilteredRowCount := row;
end;

function TMMDatabase.DoLoadMessageRow(stmt: TSQLite3Statement; loadContext: Boolean): TMMMessage;
var
  w: TMMWindow;
  p: TMMProcess;
  t: TMMThread;
  windows: TMMWindowDictionary;
  threads: TMMThreadDictionary;
  processes: TMMProcessDictionary;
begin
  w := nil;
  p := nil;
  t := nil;

  Result := TMMMessage.Create(
    stmt.ColumnInt64(MESSAGE_CX + 1),        //timestamp
    stmt.ColumnInt(MESSAGE_CX + 2),          //'pid');
    stmt.ColumnInt(MESSAGE_CX + 3),          //'tid');
    stmt.ColumnInt64(MESSAGE_CX + 0),        //event_id
    // type is 4
    stmt.ColumnText(MESSAGE_CX + 5),         //stack

    stmt.ColumnInt(1),          //'row'); // source row, not target row
    stmt.ColumnInt(2),         // 'hwnd');
    stmt.ColumnInt(3),         // 'message');
    stmt.ColumnInt64(4),       // 'wParam');
    stmt.ColumnInt64(5),       // 'lParam');
    stmt.ColumnInt64(6),       // 'lResult');
    stmt.ColumnInt(7),         // 'mode');
    stmt.ColumnText(8)         // 'detail');
  );

  //
  // Laod Window, Process and Thread data for the current message
  //

  stmtWindowBase.BindInt(1, Result.event_id);
  stmtWindowBase.BindInt(2, Result.hwnd);
  if stmtWindowBase.Step = SQLITE_ROW then
    w := LoadWindowRow(stmtWindowBase);
  stmtWindowBase.Reset;

  stmtThreadBase.BindInt(1, Result.event_id);
  stmtThreadBase.BindInt(2, Result.tid);
  if stmtThreadBase.Step = SQLITE_ROW then
    t := LoadThreadRow(stmtThreadBase);
  stmtThreadBase.Reset;

  stmtProcessBase.BindInt(1, Result.event_id);
  stmtProcessBase.BindInt(2, Result.pid);
  if stmtProcessBase.Step = SQLITE_ROW then
    p := LoadProcessRow(stmtProcessBase);
  stmtProcessBase.Reset;

  //
  // Load complete Window / Process / Thread context, if required
  //

  if loadContext then
  begin
    processes := LoadProcesses(Result.event_id);
    threads := LoadThreads(Result.event_id);
    windows := LoadWindows(Result.event_id);
  end
  else
  begin
    processes := TMMProcessDictionary.Create;
    threads := TMMThreadDictionary.Create;
    windows := TMMWindowDictionary.Create;
  end;
  Result.Fill(Context.MessageNames, p, t, w, processes, threads, windows);
end;

function TMMDatabase.LoadWindowRow(stmt: TSQLite3Statement): TMMWindow;
begin
  Result := TMMWindow.Create(
    stmt.ColumnInt64(WINDOW_CX + 1),        //timestamp
    stmt.ColumnInt(WINDOW_CX + 2),          //'pid');
    stmt.ColumnInt(WINDOW_CX + 3),          //'tid');
    stmt.ColumnInt64(WINDOW_CX + 0),        //event_id
    stmt.ColumnText(WINDOW_CX + 5),         //stack

    stmt.ColumnInt(2), // hwnd
    stmt.ColumnInt(3), // ownerPid
    stmt.ColumnInt(4), // ownerTid
    stmt.ColumnInt(5), // hwndOwner
    stmt.ColumnInt(6), // hwndParent
    stmt.ColumnText(7), // className
    stmt.ColumnText(8)  // realClassName
  );
end;

function TMMDatabase.LoadThreadRow(stmt: TSQLite3Statement): TMMThread;
begin
  Result := TMMThread.Create(
    stmt.ColumnInt64(THREAD_CX + 1),        //timestamp
    stmt.ColumnInt(THREAD_CX + 2),          //'pid');
    stmt.ColumnInt(THREAD_CX + 3),          //'tid');
    stmt.ColumnInt64(THREAD_CX + 0),        //event_id
    stmt.ColumnText(THREAD_CX + 5),         //stack

    stmt.ColumnInt(2), // tid
    stmt.ColumnText(3), // threadDescription
    stmt.ColumnInt(4), // isForegroundThread
    stmt.ColumnInt(5), // hwndFocus
    stmt.ColumnInt(6), // hwndActive
    stmt.ColumnInt(7), // hwndCapture
    stmt.ColumnInt(8), // hwndCaret
    stmt.ColumnInt(9), // hwndMenuOwner
    stmt.ColumnInt(10), // hwndMoveSize
    stmt.ColumnInt(11) // hwndMoveSize
  );
end;

function TMMDatabase.LoadProcessRow(stmt: TSQLite3Statement): TMMProcess;
begin
  Result := TMMProcess.Create(
    stmt.ColumnInt64(PROCESS_CX + 1),        //timestamp
    stmt.ColumnInt(PROCESS_CX + 2),          //'pid');
    stmt.ColumnInt(PROCESS_CX + 3),          //'tid');
    stmt.ColumnInt64(PROCESS_CX + 0),        //event_id
    stmt.ColumnText(PROCESS_CX + 5),         //stack

    stmt.ColumnInt(2), // ownerPid
    stmt.ColumnInt(3), // platform_
    stmt.ColumnText(4), // processPath
    stmt.ColumnText(5)  // commandLine
  );
end;

function TMMDatabase.LoadMessageRow(index: Integer; loadContext: Boolean): TMMMessage;
begin
  stmtMessage.BindInt(1, Ord(ftFilter)); // filter_id
  stmtMessage.BindInt(2, index); // filter_row

  if stmtMessage.Step <> SQLITE_ROW then
  begin
    raise Exception.Create('Invalid');
  end;

  Result := DoLoadMessageRow(stmtMessage, loadContext);

  stmtMessage.Reset;
end;

function TMMDatabase.LoadProcesses(event_id: Int64): TMMProcessDictionary;
var
  stmt: TSQLite3Statement;
  p: TMMProcess;
begin
  Result := TMMProcessDictionary.Create;

  // https://stackoverflow.com/questions/7745609/sql-select-only-rows-with-max-value-on-a-column
  stmt := TSQLite3Statement.Create(db,
    'select '+
    '  p.*, e.* '+
    'from '+
    '  process p inner join '+
    '  ('+
    '    select '+
    '      p0.pid, max(p0.event_id) max_event_id '+
    '    from '+
    '      process p0 '+
    '    where '+
    '      p0.event_id < ? '+
    '    group by '+
    '      p0.pid '+
    '  ) p1 on p.event_id = p1.max_event_id inner join '+
    '  event e on p.event_id = e.event_id'
  );
  try
    Assert(stmt.ColumnCount = PROCESS_CX + EVENT_CX);
    stmt.BindInt64(1, event_id);
    while stmt.Step = SQLITE_ROW do
    begin
      p := LoadProcessRow(stmt);
      Result.Add(p.pid, p);
    end;
  finally
    stmt.Free;
  end;
end;

function TMMDatabase.LoadThreads(event_id: Int64): TMMThreadDictionary;
var
  stmt: TSQLite3Statement;
  t: TMMThread;
begin
  Result := TMMThreadDictionary.Create;

  // https://stackoverflow.com/questions/7745609/sql-select-only-rows-with-max-value-on-a-column
  stmt := TSQLite3Statement.Create(db,
    'select '+
    '  t.*, e.* '+
    'from '+
    '  thread t inner join '+
    '  ('+
    '    select '+
    '      t0.tid, max(t0.event_id) max_event_id '+
    '    from '+
    '      thread t0 '+
    '    where '+
    '      t0.event_id < ? '+
    '    group by '+
    '      t0.tid '+
    '  ) t1 on t.event_id = t1.max_event_id inner join '+
    '  event e on t.event_id = e.event_id'
  );
  try
    Assert(stmt.ColumnCount = THREAD_CX + EVENT_CX);
    stmt.BindInt64(1, event_id);
    while stmt.Step = SQLITE_ROW do
    begin
      t := LoadThreadRow(stmt);
      Result.Add(t.tid, t);
    end;
  finally
    stmt.Free;
  end;
end;

function TMMDatabase.LoadWindows(event_id: Int64): TMMWindowDictionary;
var
  stmt: TSQLite3Statement;
  w: TMMWindow;
begin
  Result := TMMWindowDictionary.Create;

  stmt := TSQLite3Statement.Create(db,
    'select '+
    '  w.*, e.* '+
    'from '+
    '  window w inner join '+
    '  ('+
    '    select '+
    '      w0.hwnd, max(w0.event_id) max_event_id '+
    '    from '+
    '      window w0 '+
    '    where '+
    '      w0.event_id < ? '+
    '    group by '+
    '      w0.hwnd '+
    '  ) w1 on w.event_id = w1.max_event_id inner join '+
    '  event e on w.event_id = e.event_id'
  );
  try
    Assert(stmt.ColumnCount = WINDOW_CX + EVENT_CX);
    stmt.BindInt64(1, event_id);
    while stmt.Step = SQLITE_ROW do
    begin
      w := LoadWindowRow(stmt);
      Result.Add(w.hwnd, w);
    end;
  finally
    stmt.Free;
  end;
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
      ApplyFilter(nil);
    end;
  finally
    stmt.Free;
  end;
end;

function TMMDatabase.LoadImages(pid: Integer): TMMImages;
var
  stmt: TSQLite3Statement;
const
  sql_LoadImages =
    'SELECT filename, pid, checksum, timedatestamp, imagesize, imagebase FROM Image WHERE pid = ?';
begin
  Result := TMMImages.Create;
  stmt := TSQLite3Statement.Create(db, sql_LoadImages);
  try
    stmt.BindInt(1, pid);
    while stmt.Step <> SQLITE_DONE do
    begin
      Result.Add(TMMImage.Create(
        stmt.ColumnText(0),
        stmt.ColumnInt(1),
        stmt.ColumnInt(2),
        stmt.ColumnInt(3),
        stmt.ColumnInt64(4),
        stmt.ColumnInt64(5)
      ));
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
