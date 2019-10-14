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
    procedure gridDetailsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure gridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure gridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
//    procedure tvWindowsDblClick(Sender: TObject);
  private
    db: TMMDatabase;
    FViewType: TMMContextViewType;
    FHighlights: TSearchInfoArray;
    FMessage: TMMMessage;
    procedure SetHighlights(const Value: TSearchInfoArray);
    procedure ShowProcessDetails(p: TMMProcess);
    procedure ShowThreadDetails(t: TMMThread);
    procedure ShowWindowDetails(w: TMMWindow);
    procedure ShowWindowParentTreeContext;
    procedure ShowItemDetails(o: TObject);
    function SelectItem(o: TObject): Boolean;
  public
    { Public declarations }
    procedure SetDatabase(Adb: TMMDatabase);
    procedure CloseDatabase;

    procedure ShowCurrentContext(m: TMMMessage);

//    function ShowWindowInfo(wnd: TMMWindow): Boolean; overload;
    function ShowWindowInfo(hwnd: THandle): Boolean; overload;
//    function ShowProcessInfo(process: TMMProcess): Boolean; overload;
    function ShowProcessInfo(PID: Integer): Boolean; overload;
//    function ShowThreadInfo(thread: TMMThread): Boolean; overload;
    function ShowThreadInfo(TID: Integer): Boolean; overload;

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

procedure TMMWindowTreeFrame.FormCreate(Sender: TObject);
begin
  grid.Cells[0, 0] := 'Object';
  grid.Cells[1, 0] := 'ID/Handle';
  FormResize(Sender);
end;

procedure TMMWindowTreeFrame.FormResize(Sender: TObject);
begin
  TDetailGridController.Resize(gridDetails);
  grid.ColWidths[0] := 5;
  grid.ColWidths[0] := grid.ClientWidth - 100;
  grid.ColWidths[1] := 100;
end;

procedure TMMWindowTreeFrame.gridClick(Sender: TObject);
begin
  ShowItemDetails(grid.Objects[0, grid.Row]);
end;

procedure TMMWindowTreeFrame.gridDetailsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  TDetailGridController.DrawCellText(gridDetails.Canvas, Rect, gridDetails.Cells[ACol, ARow], FHighlights, ACol > 0);
end;

procedure TMMWindowTreeFrame.gridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  TDetailGridController.DrawCellText(gridDetails.Canvas, Rect, gridDetails.Cells[ACol, ARow], FHighlights, ARow > 0);
end;

function TMMWindowTreeFrame.SelectItem(o: TObject): Boolean;
var
  i: Integer;
begin
  for i := 1 to grid.RowCount - 1 do
    if grid.Objects[0, i] = o then
    begin
      grid.Row := i;
      ShowItemDetails(grid.Objects[0, grid.Row]);
      Exit(True);
    end;
  Result := False;
end;

procedure TMMWindowTreeFrame.SetDatabase(Adb: TMMDatabase);
begin
  db := Adb;
end;

procedure TMMWindowTreeFrame.SetHighlights(const Value: TSearchInfoArray);
begin
  FHighlights := Value;
  gridDetails.Invalidate;
  grid.Invalidate;
end;

procedure TMMWindowTreeFrame.ShowItemDetails(o: TObject);
begin
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
    ShowWindowDetails(o as TMMWindow)
  else
    Assert(False, 'Expected TMMProcess, TMMThread or TMMWindow');
end;

procedure TMMWindowTreeFrame.ShowProcessDetails(p: TMMProcess);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderProcess(FMessage.Context, p);
  TDetailGridController.Render(d, gridDetails);
end;

function TMMWindowTreeFrame.ShowProcessInfo(PID: Integer): Boolean;
var
  p: TMMProcess;
begin
  if FMessage.Context.Processes.TryGetValue(PID, p)
    then Result := SelectItem(p)
    else Result := False;
end;

function TMMWindowTreeFrame.ShowThreadInfo(TID: Integer): Boolean;
var
  t: TMMThread;
begin
  if FMessage.Context.Threads.TryGetValue(TID, t)
    then Result := SelectItem(t)
    else Result := False;
end;

function TMMWindowTreeFrame.ShowWindowInfo(hwnd: THandle): Boolean;
var
  w: TMMWindow;
begin
  if FMessage.Context.Windows.TryGetValue(hwnd, w)
    then Result := SelectItem(w)
    else Result := False;
end;

procedure TMMWindowTreeFrame.ShowThreadDetails(t: TMMThread);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderThread(FMessage.Context, t);
  TDetailGridController.Render(d, gridDetails);
end;

procedure TMMWindowTreeFrame.ShowWindowDetails(w: TMMWindow);
var
  d: TMessageDetails;
begin
  d := TDetailRenderer.RenderWindow(FMessage.Context, w);
  TDetailGridController.Render(d, gridDetails);
end;

procedure TMMWindowTreeFrame.ShowCurrentContext(m: TMMMessage);
begin
  FMessage := m;
//  if m = nil then
//  begin
//    grid.RowCount := 1;
    // TODO: clear fields
//    Exit;
//  end;
  //
  case FViewType of
    cvtWindowParentTree: ShowWindowParentTreeContext;
//    cvtWindowOwnerTree: ShowWindowOwnerTreeContext(m);
//    cvtWindowsByThread: ShowWindowsByThreadContext(m);
  end;
end;

procedure TMMWindowTreeFrame.ShowWindowParentTreeContext;
var
  t: TMMThread;
  w: TMMWindow;
  p: TMMProcess;
  r: Integer;
  ot, ow: TMMDataObject;
  c: Integer;
begin
  if FMessage = nil then
    Exit;

  // Load the set of all windows, processes and threads.
  // TODO: Consider optimisations -- partial reload based on event_id?
  // TODO: Don't show threads, processes and windows excluded by current filter

  // TODO: Refactor the data manipulation into a good place

  grid.RowCount := FMessage.Context.Threads.Count + FMessage.Context.Windows.Count + FMessage.Context.Processes.Count + 1;

  // Form a nice tree.
  for t in FMessage.Context.Threads.Values do
  begin
    if FMessage.Context.Processes.TryGetValue(t.pid, p) then
    begin
      t.Owner := p;
      p.Children.Add(t);
    end;
  end;

  for w in FMessage.Context.Windows.Values do
  begin
    if FMessage.Context.Threads.TryGetValue(w.tid, t) then
    begin
      w.Owner := t;
      t.Children.Add(w);
    end;
  end;

  r := 1;
  for p in FMessage.Context.Processes.Values do
  begin
    c := 0;
    for ot in p.Children do
      Inc(c, ot.Children.Count);

    if c > 0 then
    begin
      grid.Cells[0, r] := ExtractFileName(p.processName); // TODO use base renderer
      grid.Cells[1, r] := IntToStr(p.pid);
      grid.Objects[0, r] := p;
      Inc(r);
      for ot in p.Children do
      begin
        t := ot as TMMThread;
        if t.Children.Count > 0 then
        begin
          if t.threadDescription = ''
            then grid.Cells[0, r] := '    ' + IntToStr(t.tid)     // TODO use base renderer
            else grid.Cells[0, r] := '    ' + t.threadDescription;
          grid.Objects[0, r] := t;
          grid.Cells[1, r] := '  ' + IntToStr(t.tid);
          Inc(r);
          for ow in t.Children do
          begin
            w := ow as TMMWindow;
            grid.Cells[0, r] := '        ' + TMMWindow.BaseRender(False, w.hwnd, w.ClassName, '');
            grid.Cells[1, r] := '    ' + TMMWindow.BaseRender(w.hwnd);
            grid.Objects[0, r] := w;
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
