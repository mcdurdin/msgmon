unit MsgMon.UI.DetailRenderToGrid;

interface

uses
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Grids,

  MsgMon.System.Data.MessageDetail;

type
  TDetailGridController = class
    class procedure Render(d: TMessageDetails; grid: TStringGrid);
    class function GetClickContext(grid: TStringGrid; var data: Integer): TMessageDetailRowType;
    class procedure Resize(grid: TStringGrid);

    class procedure DrawCellText(ACanvas: TCanvas; ARect: TRect; t, highlight: string; DrawHighlight: Boolean);

  end;

implementation

uses
  Vcl.Themes;

{ TDetailRenderToGrid }

class procedure TDetailGridController.DrawCellText(ACanvas: TCanvas;
  ARect: TRect; t, highlight: string; DrawHighlight: Boolean);
var
  FLastColor: TColor;
  ALeft, ATop, n: Integer;
  t0: string;
begin
  ACanvas.Brush.Style := bsSolid;
  FLastColor := ACanvas.Font.Color;
  if StyleServices.Enabled then
  begin
    ARect.Left := ARect.Left + 4;
    ALeft := ARect.Left + 2;
    ATop := ARect.Top+((ARect.Height - ACanvas.TextHeight(t)) div 2);
  end
  else
  begin
    ALeft := ARect.Left + 2;
    ATop := ARect.Top + 2;
  end;

  if DrawHighlight and (Pos(highlight, t) > 0) then
  begin
    // Break the text down for display
    n := Pos(highlight, t);
    while n > 0 do
    begin
      t0 := Copy(t, 1, n-1);
      ACanvas.TextRect(ARect, ALeft, ATop, t0);
      ACanvas.Brush.Style := bsClear;
      Delete(t, 1, n + Length(highlight) - 1);
      Inc(ALeft, ACanvas.TextExtent(t0).cx);
      ACanvas.Font.Color := clBlue;
      ACanvas.TextRect(ARect, ALeft, ATop, highlight);
      ACanvas.Font.Color := FLastColor;
      Inc(ALeft, ACanvas.TextExtent(highlight).cx);
      n := Pos(highlight, t);
    end;
  end;

  ACanvas.TextRect(ARect, ALeft, ATop, t);
end;

class function TDetailGridController.GetClickContext(grid: TStringGrid;
  var data: Integer): TMessageDetailRowType;
begin
  Result := TMessageDetailRowType(grid.Objects[1, grid.Row]);
  data := Integer(grid.Objects[2, grid.Row]);
end;

class procedure TDetailGridController.Render(d: TMessageDetails;
  grid: TStringGrid);
var
  i: Integer;
begin
  grid.RowCount := Length(d);
  for i := 0 to High(d) do
  begin
    grid.Cells[0, i] := d[i].Name;
    grid.Cells[1, i] := MessageDetailRowTypeNames[d[i].ValueType];
    grid.Objects[1, i] := Pointer(Integer(d[i].ValueType));
    grid.Cells[2, i] := d[i].RenderToString(False);
    grid.Objects[2, i] := Pointer(d[i].ValueLink);
  end;

end;

class procedure TDetailGridController.Resize(grid: TStringGrid);
begin
  grid.ColWidths[0] := 160;
  grid.ColWidths[1] := 80;

  grid.ColWidths[2] := grid.ClientWidth -
    grid.ColWidths[0] -
    grid.ColWidths[1] - 2;
end;

end.
