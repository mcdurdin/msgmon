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
    procedure SaveFilter;
    procedure LoadFilter;
    procedure LoadColumns;
    procedure SaveColumns;
  public
    constructor Create(const AFilename, ALastFilterDefinition, ALastColumnsDefinition: string);
    destructor Destroy; override;
    procedure ApplyFilter;
    function LoadMessageRow(index: Integer): TMMMessage;

    property TotalRowCount: Integer read FTotalRowCount;
    property FilteredRowCount: Integer read FFilteredRowCount;
    property Session: TMMSession read FSession;
    property Context: TMMDataContext read FContext;
    property Filename: string read FFilename;
  end;

implementation

{ TMMDatabase }

constructor TMMDatabase.Create(const AFilename, ALastFilterDefinition, ALastColumnsDefinition: string);
begin
  inherited Create;
  FFilename := AFilename;
  FContext := TMMDataContext.Create;
  FSession := TMMSession.Create(context);
  session.LoadDefault(ALastFilterDefinition, ALastColumnsDefinition);

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

  stmt := TSQLite3Statement.Create(db, 'SELECT COUNT(*) FROM Message');
  try
    stmt.Step;
    FTotalRowCount := stmt.ColumnInt(0);
  finally
    stmt.Free;
  end;

  context.Clear;

  // Load windows

  stmt := TSQLite3Statement.Create(db, 'SELECT * FROM Window');
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
        if not context.Windows.TryGetValue(stmt.ColumnInt(1), ws) then
        begin
          ws := TMMWindows.Create;
          context.Windows.Add(stmt.ColumnInt(1), ws);
        end;
        ws.Add(w);
      end;
    end;
  finally
    stmt.Free;
  end;

  stmt := TSQLite3Statement.Create(db, 'SELECT * FROM Process');
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
          stmt.ColumnInt(1),
          stmt.ColumnInt(2),
          stmt.ColumnText(3),
          stmt.ColumnText(4)
          , 0 // TODO: add base offset to database
        );
        if not context.Processes.TryGetValue(stmt.ColumnInt(1), ps) then
        begin
          ps := TMMProcesses.Create;
          context.Processes.Add(stmt.ColumnInt(1), ps);
        end;
        ps.Add(p);
      end;
    end;
  finally
    stmt.Free;
  end;

  db.Execute('CREATE TABLE IF NOT EXISTS FilterKey (filter_id INT, filter_row INT, row INT)');
  db.Execute('CREATE INDEX IF NOT EXISTS ix_FilterKey_filterid ON FilterKey (filter_id, filter_row)');
  db.Execute('CREATE TABLE IF NOT EXISTS Filter (filter_id INT, definition TEXT)');
  db.Execute('CREATE TABLE IF NOT EXISTS Settings (id TEXT, value TEXT)');
  LoadFilter;
  LoadColumns;

  // Load Messages<!>

  stmtMessage := TSQLite3Statement.Create(db, 'SELECT Message.*, FilterKey.filter_row FROM FilterKey INNER JOIN Message on FilterKey.row = Message.row WHERE filter_id = ? AND filter_row = ?');
  Assert(stmtMessage.ColumnCount = 18);
  Assert(stmtMessage.ColumnName(0) = 'row');
  Assert(stmtMessage.ColumnName(1) = 'pid');
  Assert(stmtMessage.ColumnName(2) = 'tid');
  Assert(stmtMessage.ColumnName(3) = 'hwndFocus');
  Assert(stmtMessage.ColumnName(4) = 'hwndActive');
  Assert(stmtMessage.ColumnName(5) = 'hwndCapture');
  Assert(stmtMessage.ColumnName(6) = 'hwndCaret');
  Assert(stmtMessage.ColumnName(7) = 'hwndMenuOwner');
  Assert(stmtMessage.ColumnName(8) = 'hwndMoveSize');
  Assert(stmtMessage.ColumnName(9) = 'activeHKL');
  Assert(stmtMessage.ColumnName(10) = 'hwnd');
  Assert(stmtMessage.ColumnName(11) = 'message');
  Assert(stmtMessage.ColumnName(12) = 'wParam');
  Assert(stmtMessage.ColumnName(13) = 'lParam');
  Assert(stmtMessage.ColumnName(14) = 'lResult');
  Assert(stmtMessage.ColumnName(15) = 'mode');
  Assert(stmtMessage.ColumnName(16) = 'detail');
  Assert(stmtMessage.ColumnName(17) = 'filter_row');

//  ApplyFilter;
end;

procedure TMMDatabase.ApplyFilter;
var
  m: TMMMessage;
  f: TMMFilter;
  v: Boolean;
  row: Integer;
  stmt: TSQLite3Statement;
begin
  row := 0;
  db.BeginTransaction;
  try
    SaveFilter;
    db.Execute('DELETE FROM FilterKey WHERE filter_id = 1');
    stmt := TSQLite3Statement.Create(db, 'SELECT Message.* FROM Message');
    try
      while stmt.Step <> SQLITE_DONE do
      begin
        m := TMMMessage.Create(
          stmt.ColumnInt(0),          //'row');
          stmt.ColumnInt(1),          //'pid');
          stmt.ColumnInt(2),          //'tid');
          stmt.ColumnInt(3),          //'hwndFocus');
          stmt.ColumnInt(4),          //'hwndActive');
          stmt.ColumnInt(5),          //'hwndCapture');
          stmt.ColumnInt(6),          //'hwndCaret');
          stmt.ColumnInt(7),          //'hwndMenuOwner');
          stmt.ColumnInt(8),          //'hwndMoveSize');
          stmt.ColumnInt(9),          //'activeHKL');
          stmt.ColumnInt(10),         // 'hwnd');
          stmt.ColumnInt(11),         // 'message');
          stmt.ColumnInt64(12),       // 'wParam');
          stmt.ColumnInt64(13),       // 'lParam');
          stmt.ColumnInt64(14),       // 'lResult');
          stmt.ColumnInt(15),         // 'mode');
          stmt.ColumnBlob(16),        // 'detail');
          stmt.ColumnBytes(16)        // sizeof(detail)
        );
        try
    //      stmt.Reset;

        //  m := context.FilteredMessages[Item.Index];
          m.Fill(context.Processes, context.Windows, context.MessageNames);

          v := True;
          for f in session.filters do
          begin
            v := f.column.Filter(m, f.relation, f.value, f.action);
            if not v then
              Break;
          end;
          if v then
          begin                                                                                   // TODO rename index to row
            db.Execute('INSERT INTO FilterKey (filter_id, filter_row, row) VALUES (1, '+IntToStr(row)+', '+IntToStr(m.index)+')');
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
  stmtMessage.BindInt(1, 1); // filter_id
  stmtMessage.BindInt(2, index); // filter_row

  if stmtMessage.Step <> SQLITE_ROW then
  begin
    raise Exception.Create('Invalid');
  end;

  m := TMMMessage.Create(
    stmtMessage.ColumnInt(0),          //'row'); // source row, not target row
    stmtMessage.ColumnInt(1),          //'pid');
    stmtMessage.ColumnInt(2),          //'tid');
    stmtMessage.ColumnInt(3),          //'hwndFocus');
    stmtMessage.ColumnInt(4),          //'hwndActive');
    stmtMessage.ColumnInt(5),          //'hwndCapture');
    stmtMessage.ColumnInt(6),          //'hwndCaret');
    stmtMessage.ColumnInt(7),          //'hwndMenuOwner');
    stmtMessage.ColumnInt(8),          //'hwndMoveSize');
    stmtMessage.ColumnInt(9),          //'activeHKL');
    stmtMessage.ColumnInt(10),         // 'hwnd');
    stmtMessage.ColumnInt(11),         // 'message');
    stmtMessage.ColumnInt64(12),       // 'wParam');
    stmtMessage.ColumnInt64(13),       // 'lParam');
    stmtMessage.ColumnInt64(14),       // 'lResult');
    stmtMessage.ColumnInt(15),         // 'mode');
    stmtMessage.ColumnBlob(16),        // 'detail');
    stmtMessage.ColumnBytes(16)
  );

  stmtMessage.Reset;

//  m := context.FilteredMessages[Item.Index];
  m.Fill(context.Processes, context.Windows, context.MessageNames);
  Result := m;
end;

procedure TMMDatabase.SaveFilter;
var
  stmt: TSQLite3Statement;
  definition: string;
const
  sql_SaveFilter =
    'INSERT INTO Filter (filter_id, definition) SELECT 1, ?';
begin
  session.filters.SaveToJSON(definition);
  db.Execute('DELETE FROM Filter WHERE filter_id = 1');
  stmt := TSQLite3Statement.Create(db, sql_SaveFilter);
  try
    stmt.BindText(1, definition);
    stmt.Step;
  finally
    stmt.Free;
  end;
end;

procedure TMMDatabase.LoadFilter;
var
  stmt: TSQLite3Statement;
const
  sql_LoadFilter =
    'SELECT filter_id, definition FROM Filter WHERE filter_id = 1';
begin
  stmt := TSQLite3Statement.Create(db, sql_LoadFilter);
  try
    if stmt.Step <> SQLITE_DONE then
    begin
      if not session.filters.LoadFromJSON(stmt.ColumnText(1)) then
        session.filters.LoadDefault;
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
