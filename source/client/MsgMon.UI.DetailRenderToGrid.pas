unit MsgMon.UI.DetailRenderToGrid;

interface

uses
  Vcl.Grids,

  MsgMon.System.Data.MessageDetail;

type
  TDetailGridController = class
    class procedure Render(d: TMessageDetails; grid: TStringGrid);
    class function GetClickContext(grid: TStringGrid; var data: Integer): TMessageDetailRowType;
    class procedure Resize(grid: TStringGrid);
  end;

implementation

{ TDetailRenderToGrid }

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
