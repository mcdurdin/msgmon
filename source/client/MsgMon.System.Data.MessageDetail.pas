unit MsgMon.System.Data.MessageDetail;

interface

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Messages,
  Winapi.Windows,

  MsgMon.System.Data.Context,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Window;

type
  TMessageDetailRowType = (mdrInteger, mdrString, mdrHwnd, mdrBoolean);

  TMessageDetailRow = record
  strict private
    FName: string;
    FValueHWND: HWND;
    FValueType: TMessageDetailRowType;
    FValueBoolean: Boolean;
    FValueString: string;
    FValueInteger: Integer;
    FContext: TMMDataContext;
    FRow: Integer;
    procedure DoSet(valueType: TMessageDetailRowType; context: TMMDataContext;
      row: Integer; n: string);
  private
    procedure SetBool(context: TMMDataContext; row: Integer; n: string;
      v: Boolean);
  public
    procedure SetHwnd(context: TMMDataContext; row: Integer; n: string; v: HWND);
    procedure SetInt(context: TMMDataContext; row: Integer; n: string; v: Integer);
    procedure SetString(context: TMMDataContext; row: Integer; n, v: string);
    function RenderToString: string;
    property Name: string read FName;
    property Context: TMMDataContext read FContext write FContext;
    property Row: Integer read FRow;
    property ValueType: TMessageDetailRowType read FValueType;
    property ValueInteger: Integer read FValueInteger;
    property ValueBoolean: Boolean read FValueBoolean;
    property ValueString: string read FValueString;
    property ValueHwnd: HWND read FValueHWND;
  end;

  TMessageDetails = TArray<TMessageDetailRow>;

  TMessageDetailRenderer = class
  private
    class function WMWindowPosChanging(context: TMMDataContext; data: TMMMessage): TMessageDetails;
    class function WMKey(context: TMMDataContext; data: TMMMessage): TMessageDetails;
  public
    class function Render(context: TMMDataContext; data: TMMMessage): TMessageDetails;
  end;

implementation

uses
  MsgMon.System.Data.VKeyNames;

{ TMessageDetailRenderer }

class function TMessageDetailRenderer.Render(context: TMMDataContext; data: TMMMessage): TMessageDetails;
begin
  case data.message of
    WM_KEYDOWN,
    WM_KEYUP,
    WM_SYSKEYDOWN,
    WM_SYSKEYUP:
      Result := WMKey(context, data);
    WM_WINDOWPOSCHANGED,
    WM_WINDOWPOSCHANGING:
      Result := WMWindowPosChanging(context, data);
  else
    SetLength(Result, 0);
  end;
end;

type
  TFlag = record
    v: DWORD;
    n: string;
  end;

function FlagsToString(v: DWORD; f: array of TFlag): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(f) do
    if (v and f[i].v) = f[i].v then
      if Result <> ''
        then Result := Result + ', ' + f[i].n
        else Result := f[i].n;
end;

class function TMessageDetailRenderer.WMKey(context: TMMDataContext;
  data: TMMMessage): TMessageDetails;
begin
  SetLength(Result, 9);
  Result[0].SetInt(context, data.index, 'vkey', data.wParam);
  Result[1].SetString(context, data.index, 'vkey', IfThen(data.wParam < $100, SVKeyNames[data.wParam], ''));
  Result[2].SetInt(context, data.index, 'repeat', data.lParam and $0000FFFF);
  Result[3].SetInt(context, data.index, 'scancode', (data.lParam and $00FF0000) shr 16);
  Result[4].SetBool(context, data.index, 'extended', (data.lParam and $01000000) = $01000000);
  Result[5].SetInt(context, data.index, 'reserved', (data.lParam and $1E000000) shr 25);
  Result[6].SetBool(context, data.index, 'context', (data.lParam and $20000000) = $20000000);
  Result[7].SetBool(context, data.index, 'previous', (data.lParam and $40000000) = $40000000);
  Result[8].SetBool(context, data.index, 'transition', (data.lParam and $80000000) = $80000000);
end;

class function TMessageDetailRenderer.WMWindowPosChanging(
  context: TMMDataContext; data: TMMMessage): TMessageDetails;
const flags: array[0..10] of TFlag = (
  (v: SWP_NOSIZE; n: 'SWP_NOSIZE'),
  (v: SWP_NOMOVE; n: 'SWP_NOMOVE'),
  (v: SWP_NOZORDER; n: 'SWP_NOZORDER'),
  (v: SWP_NOREDRAW; n: 'SWP_NOREDRAW'),
  (v: SWP_NOACTIVATE; n: 'SWP_NOACTIVATE'),
  (v: SWP_FRAMECHANGED; n: 'SWP_FRAMECHANGED'),
  (v: SWP_SHOWWINDOW; n: 'SWP_SHOWWINDOW'),
  (v: SWP_HIDEWINDOW; n: 'SWP_HIDEWINDOW'),
  (v: SWP_NOCOPYBITS; n: 'SWP_NOCOPYBITS'),
  (v: SWP_NOOWNERZORDER; n: 'SWP_NOOWNERZORDER'),
  (v: SWP_NOSENDCHANGING; n: 'SWP_NOSENDCHANGING')
);
var
  wp: PWindowPos;
begin
  wp := PWindowPos(@data.detail[0]);
  SetLength(Result, 8);
  Result[0].SetHwnd(context, data.index, 'hwnd', wp.hwnd);
  Result[1].SetHwnd(context, data.index, 'hwndInsertAfter', wp.hwndInsertAfter);
  Result[2].SetInt(context, data.index, 'x', wp.x);
  Result[3].SetInt(context, data.index, 'y', wp.y);
  Result[4].SetInt(context, data.index, 'cx', wp.cx);
  Result[5].SetInt(context, data.index, 'cy', wp.cy);
  Result[6].SetInt(context, data.index, 'flags', wp.flags);
  Result[7].SetString(context, data.index, 'flags', FlagsToString(wp.flags, flags));
end;

{ TMessageDetailRow }

function TMessageDetailRow.RenderToString: string;
var
  ws: TMMWindows;
  w: TMMWindow;
begin
  Result := Name + ': ';
  case ValueType of
    mdrInteger: Result := Result + IntToStr(FValueInteger);
    mdrString: Result := Result + FValueString;
    mdrBoolean: Result := Result + BoolToStr(FValueBoolean, True);
    mdrHwnd:
      begin
        if FContext.Windows.TryGetValue(FValueHwnd, ws)
          then w := ws.FromBase(row)
          else w := nil;

        if Assigned(w) then
        begin
          Result := Result + w.Render(True);
        end
        else
          Result := Result + IntToStr(FValueHwnd);
      end;
  end;
end;

procedure TMessageDetailRow.DoSet(valueType: TMessageDetailRowType; context: TMMDataContext; row: Integer; n: string);
begin
  Self.FContext := context;
  Self.FRow := row;
  Self.FValueType := valueType;
  Self.FName := n;
end;

procedure TMessageDetailRow.SetHwnd(context: TMMDataContext; row: Integer; n: string; v: HWND);
begin
  Self.DoSet(mdrHwnd, context, row, n);
  Self.FValueHwnd := v;
end;

procedure TMessageDetailRow.SetInt(context: TMMDataContext; row: Integer; n: string; v: Integer);
begin
  Self.DoSet(mdrInteger, context, row, n);
  Self.FValueInteger := v;
end;

procedure TMessageDetailRow.SetString(context: TMMDataContext; row: Integer; n, v: string);
begin
  Self.DoSet(mdrString, context, row, n);
  Self.FValueString := v;
end;

procedure TMessageDetailRow.SetBool(context: TMMDataContext; row: Integer; n: string; v: Boolean);
begin
  Self.DoSet(mdrBoolean, context, row, n);
  Self.FValueBoolean := v;
end;

end.
