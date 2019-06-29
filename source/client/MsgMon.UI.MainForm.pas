unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, Vcl.ToolWin,
  System.SyncObjs,
  System.Generics.Collections,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Context,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Window,
  MsgMon.Data.Database,
  MsgMon.System.ExecConsoleProcess,
  Vcl.Themes,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.ActnMenus, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.Grids, Vcl.ComCtrls;

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
    TabSheet1: TTabSheet;
    memoLog: TMemo;
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
    db: TMMDatabase;
    FTraceEvent: TEvent;

    // Trace controller
    FTracing: Boolean;
    PopupContextText: string;
    PopupContextCol: Integer;
    LastIndex: Integer;
    currentMessage: TMMMessage;
    FTraceProcess: TRunConsoleApp;
    FLogStoreProcess: TRunConsoleApp;
    FTraceProcessx64: TRunConsoleApp;
    fIsWow64: LongBool;

    procedure EnableDisableTrace;
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
    procedure BeginLogCaptureProcess;
    procedure BeginLogStoreProcess;
    function GetTraceEventName: string;
    procedure BeginLogCaptureX64Process;
  end;

var
  MMMainForm: TMMMainForm;


const
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

procedure TMMMainForm.cmdFlushLibrariesClick(Sender: TObject);
begin
  FlushLibrary;
end;

function TMMMainForm.GetTraceEventName: string;
begin
  Result := 'msgmon.'+IntToStr(GetCurrentProcessId)+'.trace';
end;

procedure TMMMainForm.BeginLogProcesses;
var
  FTraceEventName: string;
begin
  FreeAndNil(db);

  FTraceEventName := PChar(GetTraceEventName);
  FTraceEvent := TEvent.Create(nil, True, False, PChar(FTraceEventName), False);

  BeginLogCaptureProcess;

  if fIsWow64 then
    BeginLogCaptureX64Process;

  BeginLogStoreProcess;
end;

procedure TMMMainForm.BeginLogCaptureProcess;
var
  path, app, cmdline: string;
  logfile: string;
begin
  path := ExtractFileDir(ParamStr(0));
  app := path+'\msgmon.recorder.exe';
  cmdline := '"'+app+'" capture -e '+GetTraceEventName;
  logfile := path+'\msgmon.recorder.capture.log';
  FTraceProcess := TRunConsoleApp.Run(app, cmdline, path, logfile);
end;

procedure TMMMainForm.BeginLogCaptureX64Process;
var
  path, app, cmdline: string;
  logfile: string;
begin
  path := ExtractFileDir(ParamStr(0));
  app := path+'\msgmon.recorder.x64.exe';
  cmdline := '"'+app+'" capture -e '+GetTraceEventName;
  logfile := path+'\msgmon.recorder.x64.capture.log';
  FTraceProcessx64 := TRunConsoleApp.Run(app, cmdline, path, logfile);
end;

procedure TMMMainForm.BeginLogStoreProcess;
var
  path, app, cmdline: string;
  logfile: string;
begin
  path := ExtractFileDir(ParamStr(0));
  app := path+'\msgmon.recorder.exe';
  cmdline := '"'+app+'" store -f -d "'+LOGSESSION_DB_FILENAME+'"';
  logfile := path+'\msgmon.recorder.store.log';

  FLogStoreProcess := TRunConsoleApp.Run(app, cmdline, path, logfile);
end;

procedure TMMMainForm.EndLogProcesses;
  procedure ReportProcessError(app, msg: string; code: Integer = 0);
  begin
    msg := app + ' ' + msg;
    if code <> 0 then
      msg := msg + '. The error ('+IntToStr(code)+') was '+SysErrorMessage(GetLastError);
    statusBar.Text := msg;
  end;

  procedure ReportProcessResult(p: TRunConsoleApp);
  var
    a: string;
  begin
    a := ExtractFileName(p.App);
    if not p.RunResult then
      ReportProcessError(a, 'failed to start', p.LastError)
    else if p.ExitCode <> 0 then
      ReportProcessError(a, 'failed with exit code '+IntToStr(p.ExitCode));

    memoLog.Text := memoLog.Text +
      p.CommandLine + #13#10 +
      p.LogText + #13#10 +
      #13#10;
  end;
begin
  Assert(Assigned(FTraceEvent));
  Assert(Assigned(FTraceProcess));
  Assert(Assigned(FTraceProcessx64));
  Assert(Assigned(FLogStoreProcess));

  FTraceEvent.SetEvent;

  { Wait for the x86 capture first; it also controls the trace }

  FTraceProcess.WaitFor;
  ReportProcessResult(FTraceProcess);
  FreeAndNil(FTraceProcess);

  { Now cleanup after the x64 capture }

  if Assigned(FTraceProcessx64) then
  begin
    FTraceProcessx64.WaitFor;
    ReportProcessResult(FTraceProcessx64);
    FreeAndNil(FTraceProcessx64);
  end;

  { And finally, the log store process will shut down once the trace events finish draining }

  FLogStoreProcess.WaitFor;
  ReportProcessResult(FLogStoreProcess);
  FreeAndNil(FLogStoreProcess);

  FreeAndNil(FTraceEvent);

  FlushLibrary;
end;

procedure TMMMainForm.EnableDisableTrace;
begin
  if FTracing then
  begin
    BeginLogProcesses;
  end
  else
  begin
    EndLogProcesses;
    LoadData;
  end;
end;

procedure TMMMainForm.FormCreate(Sender: TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  if not IsWow64Process(GetCurrentProcess, fIsWow64) then
    RaiseLastOSError;

  LastIndex := -1;
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
  if Assigned(db) then
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
  if not Assigned(db) then
    Exit;

  if (ARow < 0) or (ARow >= db.FilteredRowCount) then
    Exit;

  if ARow = 0 then
  begin
    t := db.session.displayColumns[ACol].Caption;
  end
  else
  begin
    index := ARow - 1;
    if not LoadMessageRow(index) then Exit;
    t := db.session.displayColumns[ACol].Render(currentMessage);
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
  if not Assigned(db) then
    Exit(False);

  if index = LastIndex then
    Exit(True);

  m := db.LoadMessageRow(index);
  if not Assigned(m) then
    Exit(False);

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
  if not Assigned(db) then
    Exit;

  ColumnWidths := 0;

  gridMessages.ColCount := db.session.displayColumns.Count;

  for c in db.session.displayColumns do
    if c.Width >= 0 then
      ColumnWidths := ColumnWidths + c.Width;

  i := 0;
  for c in db.session.displayColumns do
  begin
    if c.Width < 0
      then gridMessages.ColWidths[i] := gridMessages.ClientWidth - ColumnWidths
      else gridMessages.ColWidths[i] := c.Width;
    Inc(i);
  end;
end;

procedure TMMMainForm.UpdateMessageDetail(data: TMMMessage);
var
  ws: TMMWindows;
  owner, parent, w: TMMWindow;
begin
  if not Assigned(db) then
    Exit;

  editOwnerWindow.Text := '';
  editParentWindow.Text := '';
  memoMessageDetail.Text := '';
  memoCallStack.Text := '';

  if panDetail.Visible and Assigned(data) then
  begin
    owner := nil;
    w := nil;
    parent := nil;

    if db.context.Windows.TryGetValue(data.hwnd, ws) then
      w := ws.FromBase(data.index);

    if Assigned(w) then
    begin
      if (w.hwndOwner <> 0) and db.context.Windows.TryGetValue(w.hwndOwner, ws) then
        owner := ws.FromBase(data.index);

      if (w.hwndParent <> 0) and db.context.Windows.TryGetValue(w.hwndParent, ws) then
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
  if not Assigned(db) then
    Exit;
  db.context.Clear;
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
begin
  if not Assigned(db) then
    Exit;
  // TODO: Remember selected item by index

  db.ApplyFilter;

  // Refresh status
  statusBar.Panels[0].Text := 'Showing '+IntToStr(db.FilteredRowCount)+' of total '+IntToStr(db.TotalRowCount)+' messages';
  gridMessages.RowCount := db.FilteredRowCount + 1;
  gridMessages.Invalidate;
end;

procedure TMMMainForm.mnuFilterFilterClick(Sender: TObject);
begin
  if not Assigned(db) then
    Exit;

  with TMMFilterForm.Create(Self, db.session) do
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
  if not Assigned(db) then
    Exit;
  db.session.filters.Clear;
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

procedure TMMMainForm.LoadData;
begin
  if not FileExists(LOGSESSION_DB_FILENAME) then
    Exit;

  statusbar.panels[0].Text := 'Opening database';
  statusbar.Update;

  db := TMMDatabase.Create(LOGSESSION_DB_FILENAME);

  statusbar.panels[0].Text := '';
  statusbar.Update;

  PrepareView;
  ApplyFilter;
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
  if not Assigned(db) then
    Exit;
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

  PopupContextCol := ACol;
  PopupContextText := db.session.displayColumns[ACol].Render(currentMessage);

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
  if not Assigned(db) then
    Exit;

  Result.column := db.session.filters.Columns.FindClassName(db.session.displayColumns[PopupContextCol].ClassName);
  Assert(Assigned(Result.column));
  Result.relation := frIs;
  Result.value := PopupContextText;
  Result.action := faInclude;
end;

procedure TMMMainForm.mnuPopupFilterEditClick(Sender: TObject);
var
  f: TMMFilter;
begin
  if not Assigned(db) then
    Exit;
  f := CreateFilterFromPopup;
  with TMMFilterForm.Create(Self, db.session) do
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
  if not Assigned(db) then
    Exit;
  f := CreateFilterFromPopup;
  f.action := faExclude;
  db.session.filters.Add(f);
  ApplyFilter;
end;

procedure TMMMainForm.mnuPopupFilterIncludeClick(Sender: TObject);
var
  f: TMMFilter;
begin
  if not Assigned(db) then
    Exit;
  f := CreateFilterFromPopup;
  db.session.filters.Add(f);
  ApplyFilter;
end;

end.
