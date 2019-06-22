unit mm;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  SQLite3, SQLite3Wrap,
  JwaEventTracing, JwaEvntCons, JwaEventDefs, JwaWmistr;

type
  TMMTraceController = class
  private
//    FSessionHandle: TRACEHANDLE;
//    pSessionProperties: PEVENT_TRACE_PROPERTIES;
//    procedure StartTrace;
//    procedure EndTrace;
  public
    procedure Execute;
  end;

  TMMTraceReader = class //(TThread)
  private
    DB: TSQLite3Database;
    Stmt: TSQLite3Statement;
    hTrace: TRACEHANDLE;
    n: Int64;
    st: LONGLONG;
    et: LONGLONG;
    pInfo: PTRACE_EVENT_INFO;
    infoBufferSize: DWORD;
    procedure CreateDatabase;
    procedure LoadTrace;
    procedure ProcessRecord(EventRecord: PEVENT_RECORD);
  protected
    procedure Execute; //override;
  end;

procedure Run;

implementation

//const
//  LibraryName = 'msgmon.capture.x86.dll';
//
//function BeginLog: BOOL; stdcall; external LibraryName;
//function EndLog: BOOL; stdcall; external LibraryName;

{$DEFINE RUNX64}

type
  TEVENT_FILTER_DESCRIPTOR = record
    Ptr: ULONGLONG;
    Size: ULONG;
    Type_: ULONG;
  end;

  PEVENT_FILTER_DESCRIPTOR = ^TEVENT_FILTER_DESCRIPTOR;

  TENABLE_TRACE_PARAMETERS = record
    Version: ULONG;
    EnableProperty: ULONG;
    ControlFlags: ULONG;
    SourceId: TGUID;
    EnableFilterDesc: PEVENT_FILTER_DESCRIPTOR;
    FilterDescCount: ULONG;
  end;

  PENABLE_TRACE_PARAMETERS = ^TENABLE_TRACE_PARAMETERS;

function EnableTraceEx2(
    {__in} TraceHandle : TRACEHANDLE;
    {__in} ProviderId : PGUID;
    {__in} ControlCode : ULONG;
    {__in} Level : UCHAR;
    {__in} MatchAnyKeyword : ULONGLONG;
    {__in} MatchAllKeyword : ULONGLONG;
    {__in} Timeout : ULONG;
    {__in_opt} EnableParameters: PENABLE_TRACE_PARAMETERS) : ULONG; stdcall; external advapi32 name 'EnableTraceEx2';

const
  ENABLE_TRACE_PARAMETERS_VERSION_2 = 2;
  EVENT_ENABLE_PROPERTY_STACK_TRACE = 4;
  EVENT_CONTROL_CODE_ENABLE_PROVIDER = 1;
  EVENT_CONTROL_CODE_DISABLE_PROVIDER = 0;

  MsgMonProviderGuid: TGUID = '{082E6CC6-239C-4B96-9475-159AA241B4AB}';


var FERC: TMMTRaceReader;

function StringBufferSize(const s: string): Integer;
begin
  Result := (Length(s) + 1) * sizeof(WCHAR);
end;

procedure ERC(EventRecord : PEVENT_RECORD); stdcall;
begin
  FERC.ProcessRecord(EventRecord);
end;

procedure TMMTraceReader.ProcessRecord(EventRecord : PEVENT_RECORD);
var
  v: Int64;
begin
  if EventRecord.EventHeader.ProviderId <> MsgMonProviderGuid then
  begin
    Exit;
  end;

  Inc(n); if (n mod 10000) = 0 then
  begin
    // This may not be necessary for offline trace but will be for
    // realtime trace, in the future
    DB.Commit;
    DB.BeginTransaction;
    writeln(n);
  end;

  TdhGetEventInformation(EventRecord, 0, nil, pInfo, @bufferSize);
  try
    Stmt.BindInt(1, EventRecord.EventHeader.ProcessId);
    Stmt.BindInt(2, EventRecord.EventHeader.ThreadId);
    //

    // Time to read some properties...
    Stmt.StepAndReset;
  except
    ; //TODO handle

  end;

  GetEventInformation(EventRecord, Info);
//end;
//  EventRecord.

end;

procedure TMMTraceReader.CreateDatabase;
begin
  if FileExists('c:\temp\msgmon.db') then
    DeleteFile('c:\temp\msgmon.db');

  DB := TSQLite3Database.Create;
  try
//    DB.Open(':memory:'); //c:\temp\msgmon.db');
    DB.Open('c:\temp\msgmon.db', SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_NOMUTEX or SQLITE_OPEN_SHAREDCACHE);

    // Create table "messages"
    DB.Execute('CREATE TABLE messages (' +
               ' PID INT,'+
               ' TID INT,'+
               ' hwndFocus INT,'+
               ' hwndActive INT,'+
               ' hwndCapture INT,'+
               ' hwndCaret INT,'+
               ' hwndMenuOwner INT,'+
               ' hwndMoveSize INT,'+
               ' activeHKL INT,'+
               ' hwnd INT,'+
               ' message INT,'+
               ' wParam INT64,'+
               ' lParam INT64,'+
               ' lResult INT64,'+
               ' mode INT,'+
               ' detail TEXT)');

    DB.BeginTransaction;
    // Fill the table with artists
    Stmt := DB.Prepare(
      'INSERT INTO messages'+
      '(PID, TID, hwndFocus, hwndActive, hwndCapture, hwndCaret, hwndMenuOwner, hwndMoveSize, '+
      'activeHKL, hwnd, message, wParam, lParam, lResult, mode, detail) '+
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?, '+
              '?, ?, ?, ?, ?, ?, ?, ?)');
  finally

  end;
end;

procedure TMMTraceReader.Execute;
var
  err: ULONG;
begin
  CreateDatabase;
  try
    LoadTrace;
    err := ProcessTrace(@hTrace, 1, nil, nil);
    if err <> ERROR_SUCCESS then
      RaiseLastOSError(err);
    CloseTrace(hTrace);
    DB.Commit;
  finally
    Stmt.Free;
    DB.Free;
  end;
end;

procedure TMMTraceReader.LoadTrace;
var
  logFile: EVENT_TRACE_LOGFILE;
begin
  FERC := Self;
  FillChar(logFile, sizeof(EVENT_TRACE_LOGFILE), 0);
  if FileExists(ParamStr(1)) then
  begin
    logFile.LogFileName := PWideChar(ParamStr(1));
    logFile.LogFileMode.ProcessTraceMode := PROCESS_TRACE_MODE_EVENT_RECORD;
  end
  else
  begin
    logFile.LoggerName := 'MsgMon_Session';
    logFile.LogFileMode.ProcessTraceMode := PROCESS_TRACE_MODE_EVENT_RECORD or PROCESS_TRACE_MODE_REAL_TIME;
  end;
  logFile.EventCallback.EventRecordCallback := ERC;
  hTrace := OpenTrace(logFile);
  if hTrace = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  et := logFile.LogfileHeader.EndTime.QuadPart;
  st := logFile.LogfileHeader.StartTime.QuadPart;
  n := 0;
end;

{ TMMTraceController }

procedure TMMTraceController.Execute;
var
  r: TMMTraceReader;
begin
  r := TMMTraceReader.Create; //(True);
  // Skip the threading for now
  r.Execute;
//  r.WaitFor;
  r.Free;
end;

procedure Run;
begin
  with TMMTraceController.Create do
  try
    Execute;
  finally
    Free;
  end;
end;

end.
