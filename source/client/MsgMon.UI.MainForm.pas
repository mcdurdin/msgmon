unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, Vcl.ToolWin,
  JwaEventTracing, JwaEvntCons, JwaEventDefs, JwaWmistr, System.Generics.Collections,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Window,
  Vcl.Themes,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.ActnMenus, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  SQLite3, SQLite3Wrap, Vcl.Grids, Vcl.ComCtrls;

type
  TMMMainForm = class(TForm)
    statusBar: TStatusBar;
    menu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    N1: TMenuItem;
    mnuFileCaptureEvents: TMenuItem;
    N2: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditFindHighlight: TMenuItem;
    mnuEditFindBookmark: TMenuItem;
    N3: TMenuItem;
    mnuEditAutoScroll: TMenuItem;
    N4: TMenuItem;
    mnuEditClearDisplay: TMenuItem;
    mnuMessage: TMenuItem;
    mnuFilter: TMenuItem;
    mnuFilterFilter: TMenuItem;
    mnuFilterResetFilter: TMenuItem;
    mnuFilterLoad: TMenuItem;
    mnuFilterSave: TMenuItem;
    mnuFilterOrganize: TMenuItem;
    N5: TMenuItem;
    mnuFilterDropFilteredEvents: TMenuItem;
    N6: TMenuItem;
    mnuFilterHighlight: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    progress: TProgressBar;
    panDetail: TPanel;
    splitterDetail: TSplitter;
    mnuMessageViewDetailPane: TMenuItem;
    PageControl1: TPageControl;
    tabMessageDetail: TTabSheet;
    editParentWindow: TEdit;
    lblParentWindow: TLabel;
    editOwnerWindow: TEdit;
    lblOwnerWindow: TLabel;
    lblMessageDetail: TLabel;
    memoMessageDetail: TMemo;
    TabSheet2: TTabSheet;
    memoCallStack: TMemo;
    mnuItem: TPopupMenu;
    mnuPopupFilterInclude: TMenuItem;
    mnuPopupFilterExclude: TMenuItem;
    N7: TMenuItem;
    mnuPopupFilterEdit: TMenuItem;
    mnuPopupCopy: TMenuItem;
    gridMessages: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdFlushLibrariesClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileCaptureEventsClick(Sender: TObject);
    procedure mnuEditClearDisplayClick(Sender: TObject);
    procedure mnuFilterFilterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuFilterResetFilterClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuMessageClick(Sender: TObject);
    procedure mnuFileClick(Sender: TObject);
    procedure mnuMessageViewDetailPaneClick(Sender: TObject);
    procedure mnuPopupFilterIncludeClick(Sender: TObject);
    procedure mnuPopupFilterExcludeClick(Sender: TObject);
    procedure mnuPopupCopyClick(Sender: TObject);
    procedure mnuPopupFilterEditClick(Sender: TObject);
    procedure mnuItemPopup(Sender: TObject);
    procedure gridMessagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure gridMessagesClick(Sender: TObject);
  private
    db: TSQLite3Database;

    context: TMMDataContext;
    session: TMMSession;

    x64Thread: Cardinal;

    FRowCount: Integer;

    // Trace controller
    FTracing: Boolean;
    FSessionHandle: TRACEHANDLE;
    pSessionProperties: PEVENT_TRACE_PROPERTIES;
    PopupContextText: string;
    PopupContextCol: Integer;
    stmtMessage: TSQLite3Statement;
    LastIndex: Integer;
    currentMessage: TMMMessage;
//    LastCaption: string;
//    LastSubItems: TStringList;
    procedure Controller_StartTrace;
    procedure Controller_StopTrace;
    procedure EnableDisableTrace;
    procedure PrepData;
    procedure LoadData;
    procedure FlushLibrary;
    procedure BeginLogProcesses;
    procedure EndLogProcesses;
    procedure PrepareView;
    procedure ApplyFilter;
    procedure WMUser(var Message: TMessage); message WM_USER;
    procedure UpdateMessageDetail(data: TMMMessage);
    function CreateFilterFromPopup: TMMFilter;
    function LoadMessageRow(index: Integer): Boolean;
  end;

var
  MMMainForm: TMMMainForm;


const
  LOGSESSION_NAME = 'MsgMon_Session';
  LOGSESSION_DB_FILENAME = 'c:\temp\msgmon.db';
  LOGSESSION_FILENAME = 'c:\temp\msgmon.etl';
  LOGSESSION_SESSION_FILENAME = 'c:\temp\msgmon.col';

implementation

{$R *.dfm}

uses
  Vcl.Clipbrd,
  Winapi.ActiveX,
  Winapi.Tlhelp32,
  MsgMon.UI.FilterForm,
  MsgMon.System.ExecProcess;

const
  LibraryName = 'msgmon.capture.x86.dll';

function BeginLog: BOOL; stdcall; external LibraryName;
function EndLog: BOOL; stdcall; external LibraryName;

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

procedure TMMMainForm.cmdFlushLibrariesClick(Sender: TObject);
begin
  FlushLibrary;
end;

procedure TMMMainForm.BeginLogProcesses;
{$IFDEF RUNX64}
var
  app: string;
  si: TStartupInfo;
  pi: TProcessInformation;
{$ENDIF}
begin
{$IFDEF RunX64}
  app := 'msgmon.x64host.exe';

  FillChar(si, SizeOf(TStartupInfo), 0);
  FillChar(pi, Sizeof(TProcessInformation), 0);

  si.cb := SizeOf(TStartupInfo);
  if not CreateProcess(PChar(app), nil, nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
  begin
    ShowMessage('Unable to start x64 process');
    Exit;
  end;

  CloseHandle(pi.hProcess);
  x64Thread := pi.hThread;
{$ENDIF}

  BeginLog;
end;

procedure TMMMainForm.EndLogProcesses;
var
  h: THandle;
begin
  EndLog;

{$IFDEF RunX64}
  h := FindWindow('Tmsgmonx64Host', nil);
  PostMessage(h, WM_USER+100, 0, 0);
  WaitForSingleObject(x64Thread, INFINITE);
  CloseHandle(x64Thread);
{$ENDIF}

  FlushLibrary;
end;

procedure TMMMainForm.EnableDisableTrace;
var
  params: TENABLE_TRACE_PARAMETERS;
  status: ULONG;
begin
  if FTracing then
  begin
    BeginLogProcesses;

    Controller_StartTrace;

//    cmdStartStopTrace.Caption := '&Stop Tracing';
    FillChar(params, Sizeof(params), 0);
    params.Version := ENABLE_TRACE_PARAMETERS_VERSION_2;
    params.EnableProperty := EVENT_ENABLE_PROPERTY_STACK_TRACE;

    status := EnableTraceEx2(
      FSessionHandle,
      @MsgMonProviderGuid,
      EVENT_CONTROL_CODE_ENABLE_PROVIDER,
      TRACE_LEVEL_INFORMATION,
      0,
      0,
      0,
      @params);

    if ERROR_SUCCESS <> status then
      RaiseLastOSError(status, 'EnableTraceEx2');
  end
  else
  begin
//    cmdStartStopTrace.Caption := '&Start Tracing';

    EndLogProcesses;
    Controller_StopTrace;

    PrepData;
    LoadData;

{    status := EnableTraceEx2(
      FSessionHandle,
      @MsgMonProviderGuid,
      EVENT_CONTROL_CODE_DISABLE_PROVIDER,
      TRACE_LEVEL_INFORMATION,
      0,
      0,
      0,
      nil);

    if ERROR_SUCCESS <> status then
      RaiseLastOSError(status, 'EnableTraceEx2');
 }
//    StopTrace(FSessionHandle, PWideChar(LOGSESSION_NAME),
  end;
end;

procedure TMMMainForm.FormCreate(Sender: TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  LastIndex := -1;
//  LastSubItems := TStringList.Create;

  context := TMMDataContext.Create;
  session := TMMSession.Create(context);

  if FileExists(LOGSESSION_SESSION_FILENAME)
    then session.LoadFromFile(LOGSESSION_SESSION_FILENAME)
    else session.LoadDefault;

  PrepareView;
end;

procedure TMMMainForm.FormDestroy(Sender: TObject);
begin
  if FTracing then
  begin
    FTracing := not FTracing;
    EnableDisableTrace;
  end;

  CoUninitialize;
end;

procedure TMMMainForm.FormResize(Sender: TObject);
begin
  PrepareView;
end;

procedure TMMMainForm.FormShow(Sender: TObject);
begin
  // Delay after shown
  PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TMMMainForm.gridMessagesClick(Sender: TObject);
var
  index: Integer;
begin
  index := gridMessages.Row - 1;
  if not LoadMessageRow(index) then
    Exit;

  UpdateMessageDetail(currentMessage);
end;

procedure TMMMainForm.gridMessagesDrawCell(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  index: Integer;
  t: string;
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    Exit;

  if ARow = 0 then
  begin
    t := session.displayColumns[ACol].Caption;
  end
  else
  begin
    index := ARow - 1;
    if not LoadMessageRow(index) then Exit;
    t := session.displayColumns[ACol].Render(currentMessage);
  end;

  if StyleServices.Enabled then
  begin
    ARect.Left := ARect.Left + 4;
    gridMessages.Canvas.TextRect(ARect, ARect.Left+2,
      ARect.Top+((ARect.Height - gridMessages.Canvas.TextHeight(t)) div 2), t)
  end
  else
    gridMessages.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, t);
end;

function TMMMainForm.LoadMessageRow(index: Integer): Boolean;
var
  m: TMMMessage;
begin
  if index = LastIndex then Exit(True);

  stmtMessage.BindInt(1, index);
  if stmtMessage.Step <> SQLITE_ROW then
  begin
    raise Exception.Create('Invalid');
  end;

  m := TMMMessage.Create(
    stmtMessage.ColumnInt(0),          //'row');
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
    stmtMessage.ColumnText(16)         // 'detail');
  );

  stmtMessage.Reset;

//  m := context.FilteredMessages[Item.Index];
  m.Fill(context.Processes, context.Windows, context.MessageNames);

  FreeAndNil(currentMessage);
  currentMessage := m;
  LastIndex := index;
  Result := True;
end;

procedure TMMMainForm.WMUser(var Message: TMessage);
begin
  progress.Visible := True;
  LoadData;
  progress.Visible := False;
end;

procedure TMMMainForm.FlushLibrary;
var
  h: THandle;
  te: TThreadEntry32;
  hd, hdcurrent: HDESK;
begin
  // A better way of doing may be to make each loaded process have a thread
  // waiting on this process handle. Then after the process exits, the thread
  // does a PostThreadMessage and then FreeLibraryAndExitThread
  // e.g. https://stackoverflow.com/a/25597741/1836776

  hdcurrent := GetThreadDesktop(GetCurrentThreadID);

  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    FillChar(te, sizeof(te), 0);
    te.dwSize := sizeof(te);
    if Thread32First(h, te) then
    begin
      repeat
        if te.dwSize >= 12 then // see https://devblogs.microsoft.com/oldnewthing/20060223-14/?p=32173
        begin
          hd := GetThreadDesktop(te.th32ThreadID);
          if (hd <> 0) and (hd = hdcurrent) then
          begin
            if not PostThreadMessage(te.th32ThreadID, WM_NULL, 0, 0) then
            begin
              OutputDebugString(Pchar(format('Unable to post to %d::%d -- (%d) %s', [te.th32OwnerProcessID, te.th32ThreadID, GetLastError, SysErrorMessage(GetLastError)])));
            end;
          end;
        end;
      until not Thread32Next(h, te);
    end;
    CloseHandle(h);
  end;
end;

{ Grid control and display }

procedure TMMMainForm.PrepareView;
var
  c: TMMColumn;
  i, ColumnWidths: Integer;
begin
//  grid
//  lvMessages.Items.BeginUpdate;
  try
//    lvMessages.Columns.Clear;
    ColumnWidths := 0;

    gridMessages.ColCount := session.displayColumns.Count;

    for c in session.displayColumns do
      if c.Width >= 0 then
        ColumnWidths := ColumnWidths + c.Width;

    i := 0;
    for c in session.displayColumns do
    begin
      if c.Width < 0
        then gridMessages.ColWidths[i] := gridMessages.ClientWidth - ColumnWidths
        else gridMessages.ColWidths[i] := c.Width;
      Inc(i);
    end;
  finally
//    gridMessages.EndU
//    lvMessages.Items.EndUpdate;
  end;
end;

procedure TMMMainForm.UpdateMessageDetail(data: TMMMessage);
var
  ws: TMMWindows;
  owner, parent, w: TMMWindow;
begin
  editOwnerWindow.Text := '';
  editParentWindow.Text := '';
  memoMessageDetail.Text := '';
  memoCallStack.Text := '';

  if panDetail.Visible and Assigned(data) then
  begin
    owner := nil;
    w := nil;
    parent := nil;

    if context.Windows.TryGetValue(data.hwnd, ws) then
      w := ws.FromBase(data.index);

    if Assigned(w) then
    begin
      if (w.hwndOwner <> 0) and context.Windows.TryGetValue(w.hwndOwner, ws) then
        owner := ws.FromBase(data.index);

      if (w.hwndParent <> 0) and context.Windows.TryGetValue(w.hwndParent, ws) then
        parent := ws.FromBase(data.index);
    end;

    if Assigned(owner) then
      editOwnerWindow.Text := owner.Render(True);

    if Assigned(parent) then
      editParentWindow.Text := parent.Render(True);

    memoMessageDetail.Text := data.detail;
    memoCallStack.Text := data.stack;
  end;
end;

procedure TMMMainForm.mnuEditClearDisplayClick(Sender: TObject);
begin
  context.Clear;
  ApplyFilter;
  // context.MessageNames.Clear;
end;

procedure TMMMainForm.mnuFileCaptureEventsClick(Sender: TObject);
begin
  FTracing := not FTracing;
  EnableDisableTrace;
end;

procedure TMMMainForm.mnuFileClick(Sender: TObject);
begin
  mnuFileCaptureEvents.Checked := FTracing;
end;

procedure TMMMainForm.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMMMainForm.mnuFileOpenClick(Sender: TObject);
begin
//  if dlgOpen.Execute then
//  begin
//    LoadData(dlgOpen.Filename);
//  end;
end;

procedure TMMMainForm.ApplyFilter;
var
  index: Integer;
  i: Integer;
begin
  // Remember selected item
{
  s := lvMessages.Selected;
  if Assigned(s) and (s.Index >= 0) and (s.index < context.FilteredMessages.Count)
    then index := context.FilteredMessages[s.Index].index
    else index := -1;

  // Apply the filter
  session.filters.Apply;  // applies to current context
  lvMessages.Items.Count := context.FilteredMessages.Count;
  lvMessages.Invalidate;

  // Select previously selected item or nearest
  for i := 0 to context.FilteredMessages.Count - 1 do
  begin
    if context.FilteredMessages[i].index >= index then
    begin
      lvMessages.Selected := lvMessages.Items[i];
      Break;
    end;
  end;

  // Refresh status
  statusBar.Panels[0].Text := 'Showing '+IntToStr(context.FilteredMessages.Count)+' of total '+IntToStr(context.Messages.Count)+' messages';
  }
  gridMessages.RowCount := FRowCount + 1;
//  lvMessages.Items.Count := FRowCount;
end;

procedure TMMMainForm.mnuFilterFilterClick(Sender: TObject);
begin
  with TMMFilterForm.Create(Self, Session) do
  try
    if ShowModal = mrOk then
    begin
      Self.ApplyFilter;
    end;
  finally
    Free;
  end;
end;

procedure TMMMainForm.mnuFilterResetFilterClick(Sender: TObject);
begin
  session.filters.Clear;
  ApplyFilter;
end;

procedure TMMMainForm.mnuHelpAboutClick(Sender: TObject);
begin
  ShowMessage('Message Monitor v0.1');
end;

procedure TMMMainForm.mnuMessageClick(Sender: TObject);
begin
  mnuMessageViewDetailPane.Checked := panDetail.Visible;
end;

procedure TMMMainForm.mnuMessageViewDetailPaneClick(Sender: TObject);
begin
  panDetail.Visible := not panDetail.Visible;
  if panDetail.Height < 48 then
    panDetail.Height := 48;
  splitterDetail.Visible := panDetail.Visible;
  panDetail.Top := 0; // Force detail pane above status bar
  splitterDetail.Top := 0; // Enforce splitter above detail panel
end;

function StringBufferSize(const s: string): Integer;
begin
  Result := (Length(s) + 1) * sizeof(WCHAR);
end;

procedure TMMMainForm.Controller_StartTrace;
var
  BufferSize: ULONG;
  status: ULONG;
begin
  // Allocate memory for the session properties. The memory must
  // be large enough to include the log file name and session name,
  // which get appended to the end of the session properties structure.

  BufferSize := sizeof(EVENT_TRACE_PROPERTIES) + StringBufferSize(LOGSESSION_FILENAME) + StringBufferSize(LOGSESSION_NAME);
  pSessionProperties := PEVENT_TRACE_PROPERTIES(AllocMem(BufferSize));

  // Set the session properties. You only append the log file name
  // to the properties structure; the StartTrace function appends
  // the session name for you.

  pSessionProperties.Wnode.BufferSize := BufferSize;
  pSessionProperties.Wnode.Flags := WNODE_FLAG_TRACED_GUID;
  pSessionProperties.Wnode.ClientContext := 1; //QPC clock resolution
  pSessionProperties.Wnode.Guid := MsgMonProviderGuid;
  pSessionProperties.LogFileMode := EVENT_TRACE_FILE_MODE_SEQUENTIAL; //EVENT_TRACE_REAL_TIME_MODE; //
  pSessionProperties.MaximumFileSize := 256;  // 256 MB
  pSessionProperties.LoggerNameOffset := sizeof(EVENT_TRACE_PROPERTIES);
  pSessionProperties.LogFileNameOffset := sizeof(EVENT_TRACE_PROPERTIES) + sizeof(LOGSESSION_NAME);
//  StrPCopy(PWideChar(PByte(pSessionProperties) + pSessionProperties.LoggerNameOffset), LOGSESSION_NAME);
  StrPCopy(PWideChar(PByte(pSessionProperties) + pSessionProperties.LogFileNameOffset), LOGSESSION_FILENAME);

  status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  if ERROR_ALREADY_EXISTS = status then
  begin
    // The trace was already started, perhaps it did not close down cleanly
    // We'll stop it and restart it
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)));

    status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  end;

  if ERROR_SUCCESS <> status then
    RaiseLastOSError(status, 'StartTrace');

  {if not TExecProcess.WaitForProcess(
      'xperf_run.bat -on LOADER+PROC_THREAD -start "'+LOGSESSION_NAME+'" -on MsgMon:::''stack'' -f "'+LOGSESSION_1_FILENAME+'"',
      GetCurrentDir) then
    RaiseLastOSError;}
end;

procedure TMMMainForm.Controller_StopTrace;
var
  status: ULONG;
begin
  if FSessionHandle <> 0 then
  begin
    if FTracing then
    begin
      FTracing := False;
      EnableDisableTrace;
    end;

    // We use ControlTraceW because JwaEventTracing has a typo for ControlTrace
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)+' '+SysErrorMessage(status)));

    FSessionHandle := 0;
  end;

  if pSessionProperties <> nil then
  begin
    FreeMem(pSessionProperties);
    pSessionProperties := nil;
  end;

  {if not TExecProcess.WaitForProcess(
      'xperf.exe -stop "'+LOGSESSION_NAME+'" -stop -d "'+LOGSESSION_FILENAME+'"',
      GetCurrentDir) then
    RaiseLastOSError;}
end;

procedure TMMMainForm.PrepData;
begin
  if FileExists(LOGSESSION_DB_FILENAME) then
    DeleteFile(LOGSESSION_DB_FILENAME);

  if not TExecProcess.WaitForProcess('msgmon.recorder.exe "'+LOGSESSION_FILENAME+'"', GetCurrentDir) then
    RaiseLastOSError;
end;

procedure TMMMainForm.LoadData;
var
  w: TMMWindow;
  p: TMMProcess;
  ws: TMMWindows;
  ps: TMMProcesses;
  stmt: TSQLite3Statement;
  i: Integer;
begin
  if not FileExists(LOGSESSION_DB_FILENAME) then
    Exit;

  statusbar.panels[0].Text := 'Opening database';
  statusbar.Update;

  db := TSQLite3Database.Create;
  db.Open(LOGSESSION_DB_FILENAME, SQLITE_OPEN_READONLY);

  stmt := TSQLite3Statement.Create(db, 'SELECT COUNT(*) FROM Message');
  try
    stmt.Step;
    FRowCount := stmt.ColumnInt(0);
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
//        context.Windows.Add(stmt.ColumnInt(0), ws);
//        ws.Add(w);
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
//        context.Windows.Add(stmt.ColumnInt(0), ws);
//        ws.Add(w);
      end;
    end;
  finally
    stmt.Free;
  end;

  // Load Messages<!>

  stmtMessage := TSQLite3Statement.Create(db, 'SELECT * FROM Message WHERE row = ?');
  Assert(stmtMessage.ColumnCount = 17);
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

  ApplyFilter;
  statusbar.panels[0].Text := '';
  statusbar.Update;
end;

//
// Context menu
//

procedure TMMMainForm.mnuItemPopup(Sender: TObject);
var
  pt: TPoint;
  acol: Integer;
  arow: Integer;
begin
  mnuPopupFilterInclude.Enabled := False;
  mnuPopupFilterExclude.Enabled := False;
  mnuPopupCopy.Enabled := False;
  mnuPopupFilterEdit.Enabled := False;
  PopupContextText := '';

  pt := gridMessages.ScreenToClient(Mouse.CursorPos);
  gridMessages.MouseToCell(pt.X, pt.Y, acol, arow);

  if (ACol < 0) or (ARow < 1) then
    Exit;

  gridMessages.Row := ARow;

  if not LoadMessageRow(ARow-1) then
    Exit;

  PopupContextText := session.displayColumns[ACol].Render(currentMessage);

  mnuPopupFilterInclude.Caption := '&Include '''+PopupContextText+'''';
  mnuPopupFilterInclude.Enabled := True;
  mnuPopupFilterExclude.Caption := 'Ex&clude '''+PopupContextText+'''';
  mnuPopupFilterExclude.Enabled := True;
  mnuPopupCopy.Caption := '&Copy '''+PopupContextText+'''';
  mnuPopupCopy.Enabled := True;
  mnuPopupFilterEdit.Caption := '&Edit Filter '''+PopupContextText+'''...';
  mnuPopupFilterEdit.Enabled := True;
end;

procedure TMMMainForm.mnuPopupCopyClick(Sender: TObject);
begin
  Clipboard.AsText := PopupContextText;
end;

function TMMMainForm.CreateFilterFromPopup: TMMFilter;
begin
  Result := TMMFilter.Create;
  Result.column := session.filters.Columns.FindClassName(session.displayColumns[PopupContextCol].ClassName);
  Assert(Assigned(Result.column));
  Result.relation := frIs;
  Result.value := PopupContextText;
  Result.action := faInclude;
end;

procedure TMMMainForm.mnuPopupFilterEditClick(Sender: TObject);
var
  f: TMMFilter;
begin
  f := CreateFilterFromPopup;
  with TMMFilterForm.Create(Self, session) do
  try
    cbColumn.ItemIndex := cbColumn.Items.IndexOfObject(f.column);
    cbRelation.ItemIndex := 0; // frIs
    cbValue.Text := f.value;
    cbAction.ItemIndex := 0; // faInclude
    if ShowModal = mrOk then
      ApplyFilter;
  finally
    Free;
    f.Free;
  end;
end;

procedure TMMMainForm.mnuPopupFilterExcludeClick(Sender: TObject);
var
  f: TMMFilter;
begin
  f := CreateFilterFromPopup;
  f.action := faExclude;
  session.filters.Add(f);
  ApplyFilter;
end;

procedure TMMMainForm.mnuPopupFilterIncludeClick(Sender: TObject);
var
  f: TMMFilter;
begin
  f := CreateFilterFromPopup;
  session.filters.Add(f);
  ApplyFilter;
end;

end.
