unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, Vcl.ToolWin,
  System.SyncObjs,
  System.IOUtils,
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
  MsgMon.UI.DetailRenderToGrid,
  MsgMon.UI.WindowTreeFrame,
  Vcl.Themes,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.ActnMenus, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.Grids, Vcl.ComCtrls;

type
  // This overrides TDrawGrid at runtime but allows us to use the normal design-time version
  // because for some silly reason no col resize event is published. By the way, we use
  // a draw grid because it is ridiculously faster to display than a virtual list view, at
  // least with Delphi's implementation
  TDrawGrid = class(Vcl.Grids.TDrawGrid)
  private
    FOnColWidthsChanged: TNotifyEvent;
  protected
    procedure ColWidthsChanged; override;
  public
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
  end;

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
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    mnuMessageSelectColumns: TMenuItem;
    mnuPopupCopyRows: TMenuItem;
    panWindowTree: TPanel;
    Splitter1: TSplitter;
    gridMessageDetails: TStringGrid;
    tmrUpdateWindowTree: TTimer;
    panTop: TPanel;
    panHighlight1: TPanel;
    panHighlight2: TPanel;
    panHighlight3: TPanel;
    panHighlight4: TPanel;
    Shape1: TShape;
    panToolbar: TPanel;
    N8: TMenuItem;
    mnuMessageFind: TMenuItem;
    mnuMessageFindPrevious: TMenuItem;
    mnuMessageFindNext: TMenuItem;
    dlgFind: TFindDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuMessageSelectColumnsClick(Sender: TObject);
    procedure gridMessagesColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure mnuPopupCopyRowsClick(Sender: TObject);
    procedure gridMessageDetailsClick(Sender: TObject);
    procedure gridMessageDetailsDblClick(Sender: TObject);
    procedure tmrUpdateWindowTreeTimer(Sender: TObject);
    procedure mnuFilterHighlightClick(Sender: TObject);
    procedure gridMessagesDblClick(Sender: TObject);
    procedure gridMessageDetailsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure panHighlightDblClick(Sender: TObject);
    procedure mnuMessageFindClick(Sender: TObject);
    procedure mnuMessageFindPreviousClick(Sender: TObject);
    procedure mnuMessageFindNextClick(Sender: TObject);
    procedure dlgFindFind(Sender: TObject);
  private
  private
    db: TMMDatabase;
    FFilename: string;
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
    FIsTempDatabase: Boolean;
    FLastColumnsDefinition, FLastHighlightDefinition, FLastFilterDefinition: string;
    FPreparingView: Boolean;
    FWindowTreeFrame: TMMWindowTreeFrame;

    FHighlights: THighlightInfoArray;
    FActiveHighlight: Integer;

    procedure gridMessagesColWidthsChanged(Sender: TObject);
    procedure BeginLogProcesses;
    procedure EndLogProcesses;
    procedure PrepareView;
    procedure ApplyFilter;
    procedure WMUser(var Message: TMessage); message WM_USER;
    procedure UpdateMessageDetail(data: TMMMessage);
    function CreateFilterFromPopup: TMMFilter;
    function LoadMessageRow(index: Integer): Boolean;
    procedure BeginLogCaptureProcess;
    procedure BeginLogStoreProcess(shouldAppend: Boolean);
    function GetTraceEventName: string;
    procedure BeginLogCaptureX64Process;
    procedure LoadLastTrace;
    function LoadDatabase(const AFilename: string): Boolean;
    procedure CloseDatabase;
    procedure UpdateStatusBar(const simpleMessage: string = '');
    function SaveDatabase(const AFilename: string): Boolean;
    procedure DisableTrace;
    procedure EnableTrace;
    procedure CreateNewTempDatabase;
    function UpdateWindowTree: Boolean;
    procedure FindRecordByIndex(value: Integer);
    procedure FindHighlightText(FindDown: Boolean);
    procedure SetActiveHighlight(t: string; UpdateForSearch: Boolean);
  end;

var
  MMMainForm: TMMMainForm;

implementation

{$R *.dfm}

uses
  System.Win.Registry,
  Vcl.Clipbrd,
  Winapi.ActiveX,

  MsgMon.UI.FilterForm,
  MsgMon.UI.DisplayColumnForm,
  MsgMon.System.Data.MessageDetail,
  MsgMon.System.ExecProcess,
  MsgMon.System.Util;

const
  SRegKey_MsgMon = 'Software\MsgMon';
  SRegValue_LastTraceFilename = 'LastTraceFilename';
  SRegValue_Filter = 'Filter';
  SRegValue_Highlight = 'Highlight';
  SRegValue_Columns = 'Columns';

procedure TMMMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  TDetailGridController.Resize(gridMessageDetails);

  SetLength(FHighlights, 4);
  FHighlights[0].Control := panHighlight1;
  FHighlights[1].Control := panHighlight2;
  FHighlights[2].Control := panHighlight3;
  FHighlights[3].Control := panHighlight4;
  for i := Low(FHighlights) to High(FHighlights) do
    FHighlights[i].Color := FHighlights[i].Control.Font.Color;

  gridMessages.OnColWidthsChanged := gridMessagesColWidthsChanged;
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  if not IsWow64Process(GetCurrentProcess, fIsWow64) then
    RaiseLastOSError;

  statusBar.Top := Height; // Force statusBar to be at bottom of window

  FWindowTreeFrame := TMMWindowTreeFrame.Create(Self);
  FWindowTreeFrame.Parent := panWindowTree;
  FWindowTreeFrame.Visible := True;

//  CaptureWindows;

  UpdateStatusBar;

  LastIndex := -1;
end;

procedure TMMMainForm.FormDestroy(Sender: TObject);
var
  r: TRegistry;
  filterDefinition: string;
  columnsDefinition: string;
  highlightDefinition: string;
begin
  if FTracing then
  begin
    FTracing := False;
    DisableTrace;
  end;

  if Assigned(db) then
  begin
    db.Session.filters.SaveToJSON(filterDefinition);
    db.Session.highlights.SaveToJSON(highlightDefinition);
    db.Session.displayColumns.SaveToJSON(columnsDefinition);
  end;

  CloseDatabase;

  r := TRegistry.Create;
  try
    if FIsTempDatabase and FileExists(FFilename) then
      DeleteFile(FFilename);

    if r.OpenKey(SRegKey_MsgMon, True) then
    begin
      if FIsTempDatabase then
      begin
        if r.ValueExists(SRegValue_LastTraceFilename) then
          r.DeleteValue(SRegValue_LastTraceFilename);
      end
      else
        r.WriteString(SRegValue_LastTraceFilename, FFilename);

      if filterDefinition <> '' then
        r.WriteString(SRegValue_Filter, filterDefinition);
      if columnsDefinition <> '' then
        r.WriteString(SRegValue_Columns, columnsDefinition);
      if highlightDefinition <> '' then
        r.WriteString(SRegValue_Highlight, highlightDefinition);
    end;
  finally
    r.Free;
  end;

  CoUninitialize;
end;

procedure TMMMainForm.FormResize(Sender: TObject);
begin
  if Assigned(db) then
    PrepareView;

  TDetailGridController.Resize(gridMessageDetails);
end;

procedure TMMMainForm.FormShow(Sender: TObject);
begin
  // Delay after shown
  PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TMMMainForm.WMUser(var Message: TMessage);
begin
  LoadLastTrace;
end;

//
// Trace control
//

procedure TMMMainForm.EnableTrace;
begin
  BeginLogProcesses;
  UpdateStatusBar('Recording trace to '+FFilename);
end;

procedure TMMMainForm.DisableTrace;
begin
  UpdateStatusBar('Stopping trace, please wait.');
  EndLogProcesses;
  LoadDatabase(FFilename);
end;

procedure TMMMainForm.dlgFindFind(Sender: TObject);
begin
  SetActiveHighlight(dlgFind.FindText, True);
  FindHighlightText(frDown in dlgFind.Options);
end;

function TMMMainForm.GetTraceEventName: string;
begin
  Result := 'msgmon.'+IntToStr(GetCurrentProcessId)+'.trace';
end;

procedure TMMMainForm.BeginLogProcesses;
var
  FTraceEventName: string;
  shouldAppend: Boolean;
begin
  shouldAppend := Assigned(db);

  CloseDatabase;

  FTraceEventName := PChar(GetTraceEventName);
  FTraceEvent := TEvent.Create(nil, True, False, PChar(FTraceEventName), False);

  BeginLogCaptureProcess;

  if fIsWow64 then
    BeginLogCaptureX64Process;

  BeginLogStoreProcess(shouldAppend);
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

procedure TMMMainForm.BeginLogStoreProcess(shouldAppend: Boolean);
var
  path, app, cmdline: string;
  logfile: string;
begin
  path := ExtractFileDir(ParamStr(0));
  app := path+'\msgmon.recorder.exe';
  cmdline := '"'+app+'" store -d "'+FFilename+'"';
  if shouldAppend then
    cmdline := cmdline + ' -f';
  logfile := path+'\msgmon.recorder.store.log';

//  ShowMessage(cmdline);
  FLogStoreProcess := TRunConsoleApp.Run(app, cmdline, path, logfile);
end;

procedure TMMMainForm.EndLogProcesses;
  function PIDPrefix(PID: Integer): string;
  begin
    Result := '[' + IntToStr(PID) + '] ';
  end;

  procedure ReportProcessError(PID: Integer; app, msg: string; code: Integer = 0);
  begin
    msg := PIDPrefix(PID) + app + ' ' + msg;
    if code <> 0 then
      msg := msg + '. The error ('+IntToStr(code)+') was '+SysErrorMessage(GetLastError);

    UpdateStatusBar(msg);
    memoLog.Lines.Add(msg);
  end;

  procedure ReportProcessResult(p: TRunConsoleApp);
  var
    a: string;
  begin
    memoLog.Text := memoLog.Text +
      PIDPrefix(p.PID) +
      p.CommandLine + #13#10;

    a := ExtractFileName(p.App);
    if not p.RunResult then
      ReportProcessError(p.PID, a, 'failed to start', p.LastError)
    else if p.ExitCode <> 0 then
      ReportProcessError(p.PID, a, 'failed with exit code '+IntToStr(p.ExitCode));

    memoLog.Text := memoLog.Text +
      p.LogText + #13#10 +
      #13#10;
  end;
begin
  Assert(Assigned(FTraceEvent));

  FTraceEvent.SetEvent;

  { Wait for the x86 capture first; it also controls the trace }

  if Assigned(FTraceProcess) then
  begin
    FTraceProcess.WaitFor;
    ReportProcessResult(FTraceProcess);
    FreeAndNil(FTraceProcess);
  end;

  { Now cleanup after the x64 capture }

  if Assigned(FTraceProcessx64) then
  begin
    FTraceProcessx64.WaitFor;
    ReportProcessResult(FTraceProcessx64);
    FreeAndNil(FTraceProcessx64);
  end;

  { And finally, the log store process will shut down once the trace events finish draining }

  if Assigned(FLogStoreProcess) then
  begin
    FLogStoreProcess.WaitFor;
    ReportProcessResult(FLogStoreProcess);
    FreeAndNil(FLogStoreProcess);
  end;

  FreeAndNil(FTraceEvent);

  TMMUtil.FlushLibrary;
end;

//
// Event handlers
//

procedure TMMMainForm.gridMessageDetailsClick(Sender: TObject);
begin
  tmrUpdateWindowTree.Enabled := False;
  tmrUpdateWindowTree.Enabled := True;
end;

function TMMMainForm.UpdateWindowTree: Boolean;
var
  v: Integer;
begin
  case TDetailGridController.GetClickContext(gridMessageDetails, v) of
    mdrHwnd: FWindowTreeFrame.ShowWindowInfo(v);
    mdrPID: FWindowTreeFrame.ShowProcessInfo(v);
    mdrTID: FWindowTreeFrame.ShowThreadInfo(v);
    else Exit(False);
  end;
  Result := TRue;
end;

procedure TMMMainForm.gridMessageDetailsDblClick(Sender: TObject);
begin
  if UpdateWindowTree then
    FWindowTreeFrame.SetFocus;
end;

procedure TMMMainForm.gridMessageDetailsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  TDetailGridController.DrawCellText(gridMessageDetails.Canvas, Rect, gridMessageDetails.Cells[ACol, ARow], FHighlights, ACol > 0);
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

procedure TMMMainForm.gridMessagesColumnMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
begin
  db.Session.displayColumns.Move(FromIndex, ToIndex);
end;

procedure TMMMainForm.gridMessagesColWidthsChanged(Sender: TObject);
var
  i: Integer;
begin
  if FPreparingView then
    // OnColWidthsChanged is triggered from code updates to column width
    Exit;
  for i := 0 to db.Session.displayColumns.Count - 1 do
    db.Session.displayColumns[i].Width := gridMessages.ColWidths[i];
end;

procedure TMMMainForm.gridMessagesDblClick(Sender: TObject);
var
  ACol, ARow: Integer;
  pt: TPoint;
  t: string;
begin
  pt := gridMessages.ScreenToClient(Mouse.CursorPos);
  gridMessages.MouseToCell(pt.X, pt.Y, ACol, ARow);
  if ACol < 0 then
    Exit;

  t := db.session.displayColumns[ACol].Render(currentMessage);

  SetActiveHighlight(t, False);
end;

procedure TMMMainForm.SetActiveHighlight(t: string; UpdateForSearch: Boolean);
begin
  if UpdateForSearch then
    FHighlights[FActiveHighlight].Text := t
  else
    if t = FHighlights[FActiveHighlight].Text
      then FHighlights[FActiveHighlight].Text := ''
      else FHighlights[FActiveHighlight].Text := t;

  gridMessages.Invalidate;
  gridMessageDetails.Invalidate;
  FWindowTreeFrame.Highlights := FHighlights;
  UpdateStatusBar;
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

  db.InitializeFilter(db.Session.highlights); // TODO: Refactor this for when changes are made to the filter
  if (ARow > 0) and (db.Session.highlights.Count > 0) and db.DoesFilterMatchMessage(db.Session.highlights, currentMessage) then
  begin
    gridMessages.Canvas.Brush.Color := RGB($FF, $C0, $c0);
//    gridMessages.Canvas.Font.Color := clWhite;
  end;

  TDetailGridController.DrawCellText(gridMessages.Canvas, ARect, t, FHighlights, ARow > 0);
end;

procedure TMMMainForm.mnuEditClearDisplayClick(Sender: TObject);
begin
  FreeAndNil(db);
  CreateNewTempDatabase;
  gridMessages.RowCount := 2;
  gridMessages.FixedRows := 1;
  gridMessages.Invalidate;
end;

procedure TMMMainForm.mnuFileCaptureEventsClick(Sender: TObject);
begin
  FTracing := not FTracing;
  if FTracing then
  begin
    EnableTrace;
  end
  else
  begin
    DisableTrace;
  end;
end;

procedure TMMMainForm.mnuFileClick(Sender: TObject);
begin
  mnuFileCaptureEvents.Checked := FTracing;
  mnuFileOpen.Enabled := not FTracing;
  mnuFileSave.Enabled := Assigned(db);
end;

procedure TMMMainForm.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMMMainForm.mnuFileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    if LoadDatabase(dlgOpen.Filename) then
      FIsTempDatabase := False;
  end;
end;

procedure TMMMainForm.mnuFileSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    SaveDatabase(dlgSave.FileName);
  end;
end;

procedure TMMMainForm.mnuFilterFilterClick(Sender: TObject);
begin
  if not Assigned(db) then
    Exit;

  with TMMFilterForm.Create(Self, 'Message Monitor Filter', db.session.filters) do
  try
    if ShowModal = mrOk then
    begin
      Self.ApplyFilter;
    end;
  finally
    Free;
  end;
end;

procedure TMMMainForm.mnuFilterHighlightClick(Sender: TObject);
begin
  if not Assigned(db) then
    Exit;

  with TMMFilterForm.Create(Self, 'Message Monitor Highlight', db.session.highlights) do
  try
    if ShowModal = mrOk then
    begin
      gridMessages.Invalidate;
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
  ShowMessage('Message Monitor v0.2');
end;

procedure TMMMainForm.mnuMessageClick(Sender: TObject);
begin
  mnuMessageViewDetailPane.Checked := panDetail.Visible;
  mnuMessageFindPrevious.Enabled := FHighlights[FActiveHighlight].Text <> '';
  mnuMessageFindNext.Enabled := FHighlights[FActiveHighlight].Text <> '';
end;

procedure TMMMainForm.mnuMessageFindClick(Sender: TObject);
begin
  dlgFind.Execute;
end;

procedure TMMMainForm.mnuMessageFindNextClick(Sender: TObject);
begin
  FindHighlightText(True);
end;

procedure TMMMainForm.mnuMessageFindPreviousClick(Sender: TObject);
begin
  FindHighlightText(False);
end;

procedure TMMMainForm.FindHighlightText(FindDown: Boolean);
var
  row: Integer;
begin
  if FHighlights[FActiveHighlight].Text = '' then
    Exit;

  if FindDown then
  begin
    row := gridMessages.Row;
    if row >= gridMessages.RowCount then
      Exit;
  end
  else
  begin
    row := gridMessages.Row - 2;
    if row <= 0 then
      Exit;
  end;

  row := db.FindText(FHighlights[FActiveHighlight].Text, row, FindDown);
  if row >= 0 then
  begin
    gridMessages.Row := row + 1;
    gridMessagesClick(gridMessages);
  end;
end;

procedure TMMMainForm.mnuMessageSelectColumnsClick(Sender: TObject);
begin
  if not Assigned(db) then
    Exit;

  with TMMDisplayColumnsForm.Create(Self, db.session) do
  try
    if ShowModal = mrOk then
    begin
      Self.PrepareView;
    end;
  finally
    Free;
  end;
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

//
// Data control and display
//

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

procedure TMMMainForm.PrepareView;
var
  c: TMMColumn;
  i, ColumnWidths: Integer;
begin
  if not Assigned(db) then
    Exit;

  FPreparingView := True;
  try
    ColumnWidths := 0;

    gridMessages.ColCount := db.session.displayColumns.Count;

    for c in db.session.displayColumns do
      if c.Width >= 0 then
        ColumnWidths := ColumnWidths + c.Width + 1;

    i := 0;
    for c in db.session.displayColumns do
    begin
      if c.Width < 0 then
      begin
        if ColumnWidths > gridMessages.ClientWidth - 128
          then gridMessages.ColWidths[i] := 128
          else gridMessages.ColWidths[i] := gridMessages.ClientWidth - ColumnWidths
      end
      else
        gridMessages.ColWidths[i] := c.Width;
      Inc(i);
    end;
  finally
    FPreparingView := False;
  end;
end;

procedure TMMMainForm.UpdateMessageDetail(data: TMMMessage);
var
  d: TMessageDetails;
begin
  gridMessageDetails.RowCount := 1;
  memoCallStack.Text := '';

  if not Assigned(db) then
    Exit;

  if panDetail.Visible and Assigned(data) then
  begin
    d := TDetailRenderer.RenderMessage(db.Context, data, True);
    TDetailGridController.Render(d, gridMessageDetails);
    memoCallStack.Text := data.stack;

    //if mnuAutomaticallyShowDetails.Checked then
    begin
      tmrUpdateWindowTree.Enabled := False;
      tmrUpdateWindowTree.Enabled := True;
    end;
  end;
end;

procedure TMMMainForm.FindRecordByIndex(value: Integer);
var
  i: Integer;
  m: TMMMessage;
begin
  // TODO: Ouch: scanning ... but good enough for now
  for i := 0 to db.FilteredRowCount - 1 do
  begin
    m := db.LoadMessageRow(i);
    try
      if m.index >= value then
      begin
        gridMessages.Row := i + 1;
        Exit;
      end;
    finally
      m.Free;
    end;
  end;
end;

procedure TMMMainForm.ApplyFilter;
var
  FIndex: Integer;
begin
  if not Assigned(db) then
    Exit;

  // Remember selected item by index
  if (gridMessages.Row > 0) and (db.FilteredRowCount > 0) and LoadMessageRow(gridMessages.Row-1)
      then FIndex := currentMessage.index
      else FIndex := -1;

  db.ApplyFilter;

  // Refresh status
  UpdateStatusBar;
  gridMessages.RowCount := db.FilteredRowCount + 1;
  if gridMessages.RowCount > 1 then
    gridMessages.FixedRows := 1;
  gridMessages.Invalidate;

  if (FIndex >= 0) and (gridMessages.RowCount > 1) then
    FindRecordByIndex(FIndex);
end;

//
// Database file management
//

procedure TMMMainForm.LoadLastTrace;
var
  r: TRegistry;
  filename: string;
begin
  r := TRegistry.Create;
  if r.OpenKeyReadOnly(SRegKey_MsgMon) then
  begin
    if r.ValueExists(SRegValue_LastTraceFilename) then
      filename := r.ReadString(SRegValue_LastTraceFilename);
    if r.ValueExists(SRegValue_Filter) then
      FLastFilterDefinition := r.ReadString(SRegValue_Filter);
    if r.ValueExists(SRegValue_Highlight) then
      FLastHighlightDefinition := r.ReadString(SRegValue_Highlight);
    if r.ValueExists(SRegValue_Columns) then
      FLastColumnsDefinition := r.ReadString(SRegValue_Columns);
  end;

  if (filename <> '') and FileExists(filename) then
  begin
    if LoadDatabase(filename) then
    begin
      FIsTempDatabase := False;
      Exit;
    end;
  end;

  // File was not found or could not be loaded, so start fresh
  CreateNewTempDatabase;
end;

procedure TMMMainForm.CreateNewTempDatabase;
begin
  FIsTempDatabase := True;  // We'll delete on close of program
  FFilename := TPath.Combine(TPath.GetTempPath, 'msgmon.' + IntToStr(GetCurrentProcessId) + '.db');
  if FileExists(FFilename) then
    DeleteFile(FFilename);
  UpdateStatusBar;
end;

function TMMMainForm.LoadDatabase(const AFilename: string): Boolean;
begin
  if not FileExists(AFilename) then
    Exit(False);

  UpdateStatusBar('Opening database '+ExtractFileName(AFilename));

  try
    try
      db := TMMDatabase.Create(AFilename, FLastFilterDefinition, FLastHighlightDefinition, FLastColumnsDefinition);
      FFilename := AFilename;
    except
      on E:Exception do
      begin
        memoLog.Lines.Add('Error '+E.ClassName+' loading database '+AFilename+': '+E.Message);
        Exit(False);
      end;
    end;
  finally
    UpdateStatusBar;
  end;

  PrepareView;
  ApplyFilter;
  FWindowTreeFrame.SetDatabase(db);
  gridMessagesClick(gridMessages);

  Result := True;
end;

function TMMMainForm.SaveDatabase(const AFilename: string): Boolean;
begin
  Assert(Assigned(db));
  CloseDatabase;
  Result := MoveFile(PChar(FFilename), PChar(AFilename));
  if not Result then
  begin
    ShowMessage('Unable to save database to '+AFilename+': '+SysErrorMessage(GetLastError));
    Result := LoadDatabase(FFilename); // Load old file
  end
  else
  begin
    Result := LoadDatabase(AFilename); // Load renamed file
    FIsTempDatabase := False;
  end;
end;

procedure TMMMainForm.tmrUpdateWindowTreeTimer(Sender: TObject);
begin
  UpdateWindowTree;
  tmrUpdateWindowTree.Enabled := False;
end;

procedure TMMMainForm.CloseDatabase;
begin
  FWindowTreeFrame.CloseDatabase;
  FreeAndNil(db);
  UpdateStatusBar;
end;

//
// Status bar
//

procedure TMMMainForm.UpdateStatusBar(const simpleMessage: string);
var
  h: THighlightInfo;
begin
  if simpleMessage <> '' then
  begin
    statusBar.SimplePanel := True;
    statusBar.SimpleText := simpleMessage;
  end
  else
    statusBar.SimplePanel := False;

  if Assigned(db) then
  begin
    statusBar.Panels[0].Text := ExtractFileName(db.Filename);
    statusBar.Panels[3].Text := 'Showing '+IntToStr(db.FilteredRowCount)+' of total '+IntToStr(db.TotalRowCount)+' messages';
  end
  else
  begin
    statusBar.Panels[0].Text := 'No file loaded.';
    statusBar.Panels[3].Text := '';
  end;

  for h in FHighlights do
  begin
    if h.Text = ''
      then h.Control.Caption := '(no highlighted text)'
      else h.Control.Caption := h.Text;
    if h.Control = FHighlights[FActiveHighlight].Control
      then h.Control.Font.Style := [fsBold]
      else h.Control.Font.Style := [];
    Canvas.Font := h.Control.Font;
    h.Control.Width := Canvas.TextWidth(h.Control.Caption) + 8;
  end;

  statusbar.Update;
end;

//
// Context menu
//

procedure TMMMainForm.mnuPopupCopyRowsClick(Sender: TObject);
var
  s, t: string;
  row: Integer;
  col: Integer;
begin
  s := db.session.displayColumns[0].Caption;
  for col := 1 to gridMessages.ColCount - 1 do
  begin
    t := db.session.displayColumns[col].Caption;
    s := s + #9 + t;
  end;

  s := s + #13#10;

  for row := gridMessages.Selection.Top to gridMessages.Selection.Bottom do
  begin
    if not LoadMessageRow(row-1) then Exit;
    t := db.session.displayColumns[0].Render(currentMessage);
    s := s + t;
    for col := 1 to gridMessages.ColCount - 1 do
    begin
      t := db.session.displayColumns[col].Render(currentMessage);
      s := s + #9 + t;
    end;
    s := s + #13#10;
  end;

  Clipboard.AsText := s;
end;

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

  if (ARow < gridMessages.Selection.Top) or
      (ARow > gridMessages.Selection.Bottom) then
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

  if gridMessages.Selection.Top = gridMessages.Selection.Bottom then
    mnuPopupCopyRows.Caption := 'Copy selected &row'
  else
    mnuPopupCopyRows.Caption := 'Copy '+IntToStr(gridMessages.Selection.Bottom - gridMessages.Selection.Top)+
      ' selected &rows';
end;

procedure TMMMainForm.mnuPopupCopyClick(Sender: TObject);
begin
  Clipboard.AsText := PopupContextText;
end;

procedure TMMMainForm.mnuPopupFilterEditClick(Sender: TObject);
var
  f: TMMFilter;
begin
  if not Assigned(db) then
    Exit;
  f := CreateFilterFromPopup;
  with TMMFilterForm.Create(Self, 'Message Monitor Filter', db.session.filters) do
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

procedure TMMMainForm.panHighlightDblClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(FHighLights) do
    if FHighlights[i].Control = Sender then
      FActiveHighlight := i;

  UpdateStatusBar;
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

{ TDrawGrid }

procedure TDrawGrid.ColWidthsChanged;
begin
  inherited;
  if Assigned(FOnColWidthsChanged) then
    FOnColWidthsChanged(Self);
end;

end.
