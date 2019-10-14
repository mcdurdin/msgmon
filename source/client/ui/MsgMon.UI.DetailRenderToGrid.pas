unit MsgMon.UI.DetailRenderToGrid;

interface

uses
  Winapi.Windows,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.ExtCtrls,
  Vcl.Menus,

  MsgMon.System.Data.MessageDetail,
  MsgMon.System.Data.Search;

type
  TSearchInfo = record
    Search: TMMSearch;
    Control: TPanel;
    MenuItem: TMenuItem;
  end;

  TSearchInfoArray = array of TSearchInfo;

  TDetailGridController = class
    class procedure Render(d: TMessageDetails; grid: TStringGrid);
    class function GetClickContext(grid: TStringGrid; var data: Integer): TMessageDetailRowType;
    class procedure Resize(grid: TStringGrid);

    class procedure DrawCellText(ACanvas: TCanvas; ARect: TRect; t: string; highlights: TSearchInfoArray; DrawHighlight: Boolean);

  end;

implementation

uses
  Vcl.Themes;

{ TDetailRenderToGrid }

class procedure TDetailGridController.DrawCellText(ACanvas: TCanvas;
  ARect: TRect; t: string; highlights: TSearchInfoArray; DrawHighlight: Boolean);
type
  TTextRun = record
    t: string;
    Color: TColor;
  end;

  TTextRuns = array of TTextRun;

  function SplitTextByHighlights(t: string; highlights: TSearchInfoArray): TTextRuns;
  var
    I, n, x0, x: Integer;
  begin
    SetLength(Result, 0);
    while t <> '' do
    begin
      n := -1; x := MaxInt;
      for I := Low(highlights) to High(highlights) do
      begin
        if (highlights[I].Search <> nil) and (highlights[I].Search.Text <> '') then
        begin
          x0 := Pos(highlights[I].Search.Text, t);
          if (x0 > 0) and (x0 < x) then
          begin
            x := x0;
            n := I;
          end;
        end;
      end;

      if n >= 0 then
      begin
        SetLength(Result, Length(Result)+2);
        Result[High(Result)-1].t := Copy(t, 1, x-1);
        Result[High(Result)-1].Color := clNone;
        Result[High(Result)].t := Copy(t, x, Length(highlights[n].Search.Text));
        Result[High(Result)].Color := highlights[n].Search.Color;
        Delete(t, 1, x + Length(highlights[n].Search.Text) - 1);
      end
      else
        Break;
    end;

    if t <> '' then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)].t := t;
      Result[High(Result)].Color := clNone;
    end;
  end;


var
  FLastColor: TColor;
  ALeft, ATop: Integer;
  FTexts: TTextRuns;
  FText: TTextRun;
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

  FTexts := SplitTextByHighlights(t, highlights);
  for FText in FTexts do
  begin
    if FText.Color = clNone
      then ACanvas.Font.Color := FLastColor
      else ACanvas.Font.Color := FText.Color;

    // Bug in VCL: TTreeView overrides font.onchange which means canvas never updates it
    // TODO: Report this bug to Embarcadero
    SetTextColor(ACanvas.Handle, ColorToRGB(ACanvas.Font.Color));

    ACanvas.TextRect(ARect, ALeft, ATop, FText.t);
    ACanvas.Brush.Style := bsClear;
    Inc(ALeft, ACanvas.TextExtent(FText.t).cx);
  end;
  ACanvas.Font.Color := FLastColor;
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
