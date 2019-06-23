unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, Vcl.ToolWin,
  Vcl.ComCtrls, JwaEventTracing, JwaEvntCons, JwaEventDefs, JwaWmistr, System.Generics.Collections,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Window,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.ActnMenus, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  SQLite3, SQLite3Wrap;

type
  TMMMainForm = class(TForm)
    CoolBar1: TCoolBar;
    cmdStartStopTrace: TButton;
    cmdClear: TButton;
    lvMessages: TListView;
    statusBar: TStatusBar;
    cmdFlushLibraries: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvMessagesData(Sender: TObject; Item: TListItem);
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
    procedure lvMessagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mnuPopupFilterIncludeClick(Sender: TObject);
    procedure mnuPopupFilterExcludeClick(Sender: TObject);
    procedure mnuPopupCopyClick(Sender: TObject);
    procedure mnuPopupFilterEditClick(Sender: TObject);
    procedure mnuItemPopup(Sender: TObject);
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

    cmdStartStopTrace.Caption := '&Stop Tracing';
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
    cmdStartStopTrace.Caption := '&Start Tracing';

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
  lvc: TListColumn;
  ColumnWidths: Integer;
begin
  lvMessages.Items.BeginUpdate;
  try
    lvMessages.Columns.Clear;
    ColumnWidths := 0;

    for c in session.displayColumns do
      if c.Width >= 0 then
        ColumnWidths := ColumnWidths + c.Width;

    for c in session.displayColumns do
    begin
      lvc := lvMessages.Columns.Add;
      lvc.Caption := c.Caption;
      lvc.AutoSize := False;
      if c.Width < 0
        then lvc.Width := lvMessages.ClientWidth - ColumnWidths
        else lvc.Width := c.Width;
    end;
  finally
    lvMessages.Items.EndUpdate;
  end;
end;

procedure TMMMainForm.lvMessagesData(Sender: TObject; Item: TListItem);
var
  m: TMMMessage;
  i: Integer;
begin
  if (Item.Index < 0) or (Item.Index >= context.FilteredMessages.Count) then
    Exit;

  m := context.FilteredMessages[Item.Index];
//  m.Fill(context.Processes, context.Windows, context.MessageNames);

  if session.displayColumns.Count = 0 then
    Exit;

//  lvMessages.Items.BeginUpdate;
//  Item.SubItems.BeginUpdate;
  try
    Item.Caption := session.displayColumns[0].Render(m);
    for i := 1 to session.displayColumns.Count - 1 do
      Item.SubItems.Add(session.displayColumns[i].Render(m));
  finally
//    lvMessages.Items.EndUpdate;
  end;
end;

procedure TMMMainForm.lvMessagesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Item) and (Item.Index >= 0) and (Item.Index < context.FilteredMessages.Count)
    then UpdateMessageDetail(context.FilteredMessages[Item.Index])
    else UpdateMessageDetail(nil);
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
  s: TListItem;
  index: Integer;
  i: Integer;
begin
  // Remember selected item
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
  m: TMMMessage;
  w: TMMWindow;
  eventName: string;
//  p: TMMProcess;
  ws: TMMWindows;
//  ps: TMMProcesses;
  pos: Integer;
  stmt: TSQLite3Statement;
  i: Integer;
begin
  if not FileExists(LOGSESSION_DB_FILENAME) then
    Exit;

  statusbar.panels[0].Text := 'Opening database';
  statusbar.Update;

  db := TSQLite3Database.Create;
  db.Open(LOGSESSION_DB_FILENAME, SQLITE_OPEN_READONLY);

  stmt := TSQLite3Statement.Create(db, 'SELECT COUNT(*) FROM Messages');
  try
    stmt.Step;
    FRowCount := stmt.ColumnInt64(0);
  finally
    stmt.Free;
  end;

  context.Clear;

  // Load windows

  stmt := TSQLite3Statement.Create(db, 'SELECT * FROM Window');
  try
    Assert(stmt.ColumnCount = 7);
    Assert(stmt.ColumnName(0) = 'hwnd');
    Assert(stmt.ColumnName(1) = 'pid');
    Assert(stmt.ColumnName(2) = 'tid');
    Assert(stmt.ColumnName(3) = 'hwndOwner');
    Assert(stmt.ColumnName(4) = 'hwndParent');
    Assert(stmt.ColumnName(5) = 'className');
    Assert(stmt.ColumnName(6) = 'realClassName');

    while stmt.Step <> SQLITE_DONE do
    begin
      for i := 0 to stmt.ColumnCount - 1 do
      begin
        w := TMMWindow.Create(
          stmt.ColumnInt(0),
          stmt.ColumnInt(1),
          stmt.ColumnInt(2),
          stmt.ColumnInt(3),
          stmt.ColumnInt(4),
          stmt.ColumnText(5),
          stmt.ColumnText(6)
          , 0 // TODO: add base offset to database
        );
        if not context.Windows.TryGetValue(stmt.ColumnInt(0), ws) then
        begin
          ws := TMMWindows.Create;
          context.Windows.Add(stmt.ColumnInt(0), ws);
        end;
        ws.Add(w);
//        context.Windows.Add(stmt.ColumnInt(0), ws);
//        ws.Add(w);
      end;
    end;
  finally
    stmt.Free;
  end;

  // TODO: Load processes

  // Load Messages<!>

  stmt := TSQLite3Statement.Create(db, 'SELECT * FROM Message');
  try
    Assert(stmt.ColumnCount = 16);
    Assert(stmt.ColumnName(0) = 'pid');
    Assert(stmt.ColumnName(1) = 'tid');
    Assert(stmt.ColumnName(2) = 'hwndFocus');
    Assert(stmt.ColumnName(3) = 'hwndActive');
    Assert(stmt.ColumnName(4) = 'hwndCapture');
    Assert(stmt.ColumnName(5) = 'hwndCaret');
    Assert(stmt.ColumnName(6) = 'hwndMenuOwner');
    Assert(stmt.ColumnName(7) = 'hwndMoveSize');
    Assert(stmt.ColumnName(8) = 'activeHKL');
    Assert(stmt.ColumnName(9) = 'hwnd');
    Assert(stmt.ColumnName(10) = 'message');
    Assert(stmt.ColumnName(11) = 'wParam');
    Assert(stmt.ColumnName(12) = 'lParam');
    Assert(stmt.ColumnName(13) = 'lResult');
    Assert(stmt.ColumnName(14) = 'mode');
    Assert(stmt.ColumnName(15) = 'detail');

    while stmt.Step <> SQLITE_DONE do
    begin
      for i := 0 to stmt.ColumnCount - 1 do
      begin
        m := TMMMessage.Create(
          i,
          stmt.ColumnInt(0),
          stmt.ColumnInt(1),
          stmt.ColumnInt(2),
          stmt.ColumnInt(3),
          stmt.ColumnInt(4),
          stmt.ColumnInt(5),
          stmt.ColumnInt(6),
          stmt.ColumnInt(7),
          stmt.ColumnInt(8),
          stmt.ColumnInt(9),
          stmt.ColumnInt(10),
          stmt.ColumnInt64(11),
          stmt.ColumnInt64(12),
          stmt.ColumnInt64(13),
          stmt.ColumnInt(14),
          stmt.ColumnText(15)
        );
        context.Messages.Add(m);
//        context.Windows.Add(stmt.ColumnInt(0), ws);
//        ws.Add(w);
      end;
    end;
  finally
    stmt.Free;
  end;

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
  ht: TListItem;
  i: Integer;
  w: Integer;
begin
  mnuPopupFilterInclude.Enabled := False;
  mnuPopupFilterExclude.Enabled := False;
  mnuPopupCopy.Enabled := False;
  mnuPopupFilterEdit.Enabled := False;
  PopupContextText := '';

  pt := lvMessages.ScreenToClient(Mouse.CursorPos);
  ht := lvMessages.GetItemAt(pt.X, pt.Y);
  if not Assigned(ht) then
    Exit;

  PopupContextCol := -1; w := 0;
  for i := 0 to lvMessages.Columns.Count - 1 do
  begin
    w := w + lvMessages.Columns[i].Width;
    if pt.X < w then
    begin
      PopupContextCol := i;
      Break;
    end;
  end;

  if PopupContextCol < 0 then
    Exit;

  if PopupContextCol = 0 then
    PopupContextText := ht.Caption
  else if PopupContextCol - 1 < ht.SubItems.Count then
    PopupContextText := ht.SubItems[PopupContextCol - 1]
  else
    Exit;

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
