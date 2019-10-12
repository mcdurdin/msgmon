unit MsgMon.UI.WindowTreeFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Grids, Vcl.ExtCtrls,

  System.Generics.Collections,

  MsgMon.Data.Database,
  MsgMon.UI.DetailRenderToGrid,
  MsgMon.System.ContextViewTypes,
  MsgMon.System.Data.Event,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TMMWindowTreeFrame = class(TForm)
    Splitter1: TSplitter;
    gridDetails: TStringGrid;
    grid: TStringGrid;
    procedure FormResize(Sender: TObject);
    procedure gridDetailsDblClick(Sender: TObject);
    procedure gridDetailsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
//    procedure tvWindowsDblClick(Sender: TObject);
  private
    db: TMMDatabase;
    ws: TMMWindowDictionary;
    ps: TMMProcessDictionary;
    ts: TMMThreadDictionary;
    FViewType: TMMContextViewType;
    FHighlights: TSearchInfoArray;
    procedure RefreshTree;
    procedure SetHighlights(const Value: TSearchInfoArray);
//    function ShowInfo(d: Pointer): Boolean;
    procedure ShowProcessDetails(p: TMMProcess);
//    function ShowProcessInfo(PID: Integer): Boolean;
    procedure ShowThreadDetails(t: TMMThread);
//    function ShowThreadInfo(TID: Integer): Boolean;
    procedure ShowWindowDetails(w: TMMWindow);
//    function ShowWindowInfo(hwnd: THandle): Boolean;
    procedure ShowWindowParentTreeContext(m: TMMMessage);
    procedure ShowItemDetails(item: TObject);
  public
    { Public declarations }
    procedure SetDatabase(Adb: TMMDatabase);
    procedure CloseDatabase;

    procedure ShowCurrentContext(m: TMMMessage);
    {
    function ShowWindowInfo(wnd: TMMWindow): Boolean; overload;
    function ShowWindowInfo(hwnd: THandle): Boolean; overload;
    function ShowProcessInfo(PID: Integer): Boolean; overload;
    function ShowProcessInfo(process: TMMProcess): Boolean; overload;
    function ShowThreadInfo(TID: Integer): Boolean; overload;
    function ShowThreadInfo(thread: TMMThread): Boolean; overload;
    }
    property Highlights: TSearchInfoArray read FHighlights write SetHighlights;
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
//  case TDetailGridController.GetClickContext(gridDetails, v) of
//    mdrHwnd: ShowWindowInfo(v);
//    mdrPID: ShowProcessInfo(v);
//    mdrTID: ShowThreadInfo(v);
//    else Exit;
//  end;

//  tvWindows.SetFocus;
end;

procedure TMMWindowTreeFrame.gridDetailsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  TDetailGridController.DrawCellText(gridDetails.Canvas, Rect, gridDetails.Cells[ACol, ARow], FHighlights, ACol > 0);
end;

procedure TMMWindowTreeFrame.RefreshTree;
begin

end;

procedure TMMWindowTreeFrame.SetDatabase(Adb: TMMDatabase);
begin
  db := Adb;
  RefreshTree;
end;

procedure TMMWindowTreeFrame.SetHighlights(const Value: TSearchInfoArray);
begin
  FHighlights := Value;
  gridDetails.Invalidate;
  grid.Invalidate;
end;

procedure TMMWindowTreeFrame.ShowItemDetails(item: TObject);
var
  o: TObject;
begin
  o := item; //TObject(node.Data);
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

//function TMMWindowTreeFrame.ShowWindowInfo(wnd: TMMWindow): Boolean;
//begin
//  Result := ShowInfo(wnd);
//end;

//function TMMWindowTreeFrame.ShowProcessInfo(process: TMMProcess): Boolean;
//begin
//  Result := ShowInfo(process);
//end;

//function TMMWindowTreeFrame.ShowThreadInfo(thread: TMMThread): Boolean;
//begin
//  Result := ShowInfo(thread);
//end;

procedure TMMWindowTreeFrame.ShowCurrentContext(m: TMMMessage);
begin
  if m = nil then
  begin
    grid.RowCount := 1;
    // TODO: clear
    Exit;
  end;
  //
  case FViewType of
    cvtWindowParentTree: ShowWindowParentTreeContext(m);
//    cvtWindowOwnerTree: ShowWindowOwnerTreeContext(m);
//    cvtWindowsByThread: ShowWindowsByThreadContext(m);
  end;
end;

procedure TMMWindowTreeFrame.ShowWindowParentTreeContext(m: TMMMessage);
var
  t: TMMThread;
  w: TMMWindow;
  p: TMMProcess;
  r: Integer;
  ot, ow: TMMDataObject;
  c: Integer;
begin
  // Load the set of all windows, processes and threads.
  // TODO: Consider optimisations -- partial reload based on event_id?
  FreeAndNil(ws);
  FreeAndNil(ps);
  FreeAndNil(ts);
  ws := db.LoadWindows(m.event_id);
  ps := db.LoadProcesses(m.event_id);
  ts := db.LoadThreads(m.event_id);

  grid.RowCount := ts.Count + ws.Count + ps.Count + 1;

  // Form a nice tree.
  for t in ts.Values do
  begin
    if ps.TryGetValue(t.pid, p) then
    begin
      t.Owner := p;
      p.Children.Add(t);
    end;
  end;

  for w in ws.Values do
  begin
    if ts.TryGetValue(w.tid, t) then
    begin
      w.Owner := t;
      t.Children.Add(w);
    end;
  end;

  r := 1;
  for p in ps.Values do
  begin
    c := 0;
    for ot in p.Children do
      Inc(c, ot.Children.Count);

    if c > 0 then
    begin
      grid.Cells[0, r] := ExtractFileName(p.processName);
      Inc(r);
      for ot in p.Children do
      begin
        t := ot as TMMThread;
        if t.Children.Count > 0 then
        begin
          grid.Cells[0, r] := '   ' + IntToStr(t.tid);
          Inc(r);
          for ow in t.Children do
          begin
            w := ow as TMMWindow;
            grid.Cells[0, r] := '      ' + IntToHex(w.hwnd, 8) + ' ' + w.ClassName; //TODO: Use column renderers
            Inc(r);
          end;
        end;
      end;
    end;
  end;

  grid.RowCount := r;
end;

{function TMMWindowTreeFrame.ShowInfo(d: Pointer): Boolean;
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
end;}

{procedure TMMWindowTreeFrame.tvWindowsDblClick(Sender: TObject);
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
end;}

end.
