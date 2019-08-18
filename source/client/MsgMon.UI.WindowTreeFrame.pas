unit MsgMon.UI.WindowTreeFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  System.Generics.Collections,

  MsgMon.Data.Database,
  MsgMon.UI.DetailRenderToGrid,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window, Vcl.Grids, Vcl.ExtCtrls;

type
  TTreeView = class(Vcl.ComCtrls.TTreeView)
  protected
    procedure CreateWnd; override;
  end;

  TMMWindowTreeFrame = class(TForm)
    tvWindows: TTreeView;
    Splitter1: TSplitter;
    gridDetails: TStringGrid;
    procedure tvWindowsChange(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure gridDetailsDblClick(Sender: TObject);
    procedure gridDetailsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure tvWindowsAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure tvWindowsDblClick(Sender: TObject);
  private
    db: TMMDatabase;
    FHighlights: THighlightInfoArray;
    procedure RefreshTree;
    procedure ShowItemDetails(node: TTreeNode);
    procedure ShowProcessDetails(p: TMMProcess);
    procedure ShowThreadDetails(t: TMMThread);
    procedure ShowWindowDetails(w: TMMWindow);
    function ShowInfo(d: Pointer): Boolean;
    procedure SetHighlights(const Value: THighlightInfoArray);
  public
    { Public declarations }
    procedure SetDatabase(Adb: TMMDatabase);
    procedure CloseDatabase;

    function ShowWindowInfo(wnd: TMMWindow): Boolean; overload;
    function ShowWindowInfo(hwnd: THandle): Boolean; overload;
    function ShowProcessInfo(PID: Integer): Boolean; overload;
    function ShowProcessInfo(process: TMMProcess): Boolean; overload;
    function ShowThreadInfo(TID: Integer): Boolean; overload;
    function ShowThreadInfo(thread: TMMThread): Boolean; overload;
    property Highlights: THighlightInfoArray read FHighlights write SetHighlights;
  end;

implementation

uses
  Winapi.UxTheme,
  MsgMon.System.Data.MessageDetail;

{$R *.dfm}

procedure TMMWindowTreeFrame.CloseDatabase;
begin
  db := nil;
end;

procedure TMMWindowTreeFrame.FormResize(Sender: TObject);
begin
  TDetailGridController.Resize(gridDetails);
end;

procedure TMMWindowTreeFrame.gridDetailsDblClick(Sender: TObject);
var
  v: Integer;
begin
  case TDetailGridController.GetClickContext(gridDetails, v) of
    mdrHwnd: ShowWindowInfo(v);
    mdrPID: ShowProcessInfo(v);
    mdrTID: ShowThreadInfo(v);
    else Exit;
  end;

  tvWindows.SetFocus;
end;

procedure TMMWindowTreeFrame.gridDetailsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  TDetailGridController.DrawCellText(gridDetails.Canvas, Rect, gridDetails.Cells[ACol, ARow], FHighlights, ACol > 0);
end;

procedure TMMWindowTreeFrame.RefreshTree;
  procedure AddWindow(p: TTreeNode; w: TMMWindow);
  var
    wnode: TTreeNode;
    w0: TMMWindow;
  begin
    // TODO: Use column renderer for window
    wnode := tvWindows.Items.AddChild(p, IntToHex(w.hwnd, 8) + ' ' + w.ClassName);
    wnode.Data := w;
    for w0 in w.ChildWindows do
    begin
      AddWindow(wnode, w0);
    end;
  end;
var
  pp: TPair<DWORD, TMMProcesses>;
  tp: TPair<DWORD, TMMThread>;
  pnode, tnode: TTreeNode;
  wp: TPair<DWORD, TMMWindows>;
  w: TMMWindow;
begin
  tvWindows.Items.BeginUpdate;
  try
    tvWindows.Items.Clear;

    // For each process, get the windows
    for pp in db.Context.Processes do
    begin
      // TODO: Use column renderer for PID?
      pnode := tvWindows.Items.AddObject(nil, IntToStr(pp.Value[0].PID) + ' ' + pp.Value[0].processName, pp.Value[0]);
      for tp in pp.Value[0].Threads do
      begin
        // TODO: Use column renderer for TID?
        tnode := tvWindows.Items.AddChildObject(pnode, IntToStr(tp.Value.TID), tp.Value);
        for wp in tp.Value.Windows do
        begin
          w := wp.Value[0];
          if w.hwndParent = 0 then
          begin
            AddWindow(tnode, w);
          end;
        end;
      end;
      //ps.Value

//      tvn := TTreeNode.Create
    end;

    tvWindows.FullExpand;
  finally
    tvWindows.Items.EndUpdate;
  end;
end;

procedure TMMWindowTreeFrame.SetDatabase(Adb: TMMDatabase);
begin
  db := Adb;
  RefreshTree;
end;

procedure TMMWindowTreeFrame.SetHighlights(const Value: THighlightInfoArray);
begin
  FHighlights := Value;
  gridDetails.Invalidate;
  tvWindows.Invalidate;
end;

procedure TMMWindowTreeFrame.ShowItemDetails(node: TTreeNode);
var
  o: TObject;
begin
  o := TObject(node.Data);
  if o = nil then
  begin
    gridDetails.RowCount := 1;
    gridDetails.Cells[0,0] := '';
    gridDetails.Cells[1,0] := '';
    gridDetails.Cells[2,0] := '';
  end
  else if o is TMMProcess then
    ShowProcessDetails(o as TMMProcess)
  else if o is TMMThread then
    ShowThreadDetails(o as TMMThread)
  else if o is TMMWindow then
    ShowWindowDetails(o as TMMWindow);
end;

procedure TMMWindowTreeFrame.ShowProcessDetails(p: TMMProcess);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderProcess(db.Context, p);
  TDetailGridController.Render(d, gridDetails);
end;

procedure TMMWindowTreeFrame.ShowThreadDetails(t: TMMThread);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderThread(db.Context, t);
  TDetailGridController.Render(d, gridDetails);
end;

procedure TMMWindowTreeFrame.ShowWindowDetails(w: TMMWindow);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderWindow(db.Context, w);
  TDetailGridController.Render(d, gridDetails);
end;

function TMMWindowTreeFrame.ShowProcessInfo(PID: Integer): Boolean;
var
  ps: TMMProcesses;
begin
  Result := False;

  if not Assigned(db) or
      not db.Context.Processes.TryGetValue(PID, ps) or
      (ps.Count = 0) then
    Exit;

  Result := ShowProcessInfo(ps[0]);
end;

function TMMWindowTreeFrame.ShowThreadInfo(TID: Integer): Boolean;
var
  t: TMMThread;
  pp: TPair<DWORD, TMMProcesses>;
begin
  Result := False;

  if not Assigned(db) then
    Exit;

  for pp in db.Context.Processes do
    if pp.Value[0].Threads.TryGetValue(TID, t) then
      Exit(ShowThreadInfo(t));
end;

function TMMWindowTreeFrame.ShowWindowInfo(hwnd: THandle): Boolean;
var
  ws: TMMWindows;
begin
  Result := False;

  if not Assigned(db) or
      not db.Context.Windows.TryGetValue(hwnd, ws) or
      (ws.Count = 0) then
    Exit;

  Result := ShowWindowInfo(ws[0]);
end;

function TMMWindowTreeFrame.ShowWindowInfo(wnd: TMMWindow): Boolean;
begin
  Result := ShowInfo(wnd);
end;

function TMMWindowTreeFrame.ShowProcessInfo(process: TMMProcess): Boolean;
begin
  Result := ShowInfo(process);
end;

function TMMWindowTreeFrame.ShowThreadInfo(thread: TMMThread): Boolean;
begin
  Result := ShowInfo(thread);
end;

function TMMWindowTreeFrame.ShowInfo(d: Pointer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to tvWindows.Items.Count - 1 do
    if tvWindows.Items[i].Data = d then
    begin
      tvWindows.Select(tvWindows.Items[i]);
      Exit(True);
    end;
end;

procedure TMMWindowTreeFrame.tvWindowsAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  ARect: TRect;
begin
  if Stage = cdPostPaint then
  begin
    ARect := Node.DisplayRect(True);
    if cdsSelected in State then
    begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clHighlightText;
    end
    else
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := clWindowText;
    end;

    TDetailGridController.DrawCellText(Sender.Canvas, ARect, Node.Text, FHighlights, True);
    PaintImages := False;
    DefaultDraw := False;
  end
  else
    DefaultDraw := True;
end;

procedure TMMWindowTreeFrame.tvWindowsChange(Sender: TObject; Node: TTreeNode);
begin
  ShowItemDetails(Node);
end;

procedure TMMWindowTreeFrame.tvWindowsDblClick(Sender: TObject);
var
  node: TTreeNode;
  pt: TPoint;
begin
  pt := tvWindows.ScreenToClient(Mouse.CursorPos);
  node := tvWindows.GetNodeAt(pt.X, pt.Y);
  if Assigned(node) then
  begin
    // TODO: dblclick to find this window
  end;
end;

{ TTreeView }

procedure TTreeView.CreateWnd;
begin
  inherited;
  // TODO: Enables custom theme drawing, is this necessary?
  SetWindowTheme(Handle, nil, nil);
end;

end.
