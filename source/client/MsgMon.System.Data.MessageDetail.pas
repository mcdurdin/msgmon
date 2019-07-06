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
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Session,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TMessageDetailRowType = (mdrTitle, mdrInteger, mdrString, mdrHwnd, mdrBoolean, mdrPID, mdrTID);

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
    procedure DoSet(valueType: TMessageDetailRowType; n: string);
  private
    FValuePID: Cardinal;
    FValueTID: Cardinal;
    FValueLink: Integer;
    procedure SetContextInfo(context: TMMDataContext; row: Integer);
    procedure SetHwnd(n: string; v: HWND);
    procedure SetBool(n: string; v: Boolean);
    procedure SetInt(n: string; v: Integer);
    procedure SetPID(n: string; v: Cardinal);
    procedure SetTID(n: string; v: Cardinal);
    procedure SetString(n, v: string);
    procedure SetTitle(n: string);
  public
    function RenderToString(IncludeTitle: Boolean): string;
    property Name: string read FName;
    property Context: TMMDataContext read FContext write FContext;
    property Row: Integer read FRow;
    property ValueType: TMessageDetailRowType read FValueType;
    property ValueLink: Integer read FValueLink;
    property ValueInteger: Integer read FValueInteger;
    property ValueBoolean: Boolean read FValueBoolean;
    property ValueString: string read FValueString;
    property ValueHwnd: HWND read FValueHWND;
    property ValuePID: Cardinal read FValuePID;
    property ValueTID: Cardinal read FValueTID;
  end;

  TMessageDetails = TArray<TMessageDetailRow>;

  TMessageDetailRenderer = class
  private
    class function WMWindowPosChanging(context: TMMDataContext; data: TMMMessage): TMessageDetails;
    class function WMKey(context: TMMDataContext; data: TMMMessage): TMessageDetails;
    class function RenderDefaults(context: TMMDataContext; data: TMMMessage): TMessageDetails;
  public
  end;

  TDetailRenderer = class
  private
    class procedure SetContextInfo(var details: TMessageDetails; context: TMMDataContext; row: Integer);
  public
    class function RenderProcess(context: TMMDataContext; process: TMMProcess): TMessageDetails;
    class function RenderThread(context: TMMDataContext; thread: TMMThread): TMessageDetails;
    class function RenderWindow(context: TMMDataContext; window: TMMWindow): TMessageDetails;
    class function RenderMessage(context: TMMDataContext; data: TMMMessage; IncludeDefaults: Boolean): TMessageDetails;
  end;

const
  MessageDetailRowTypeNames: array[TMessageDetailRowType] of string = ('Title', 'Integer', 'String', 'Hwnd', 'Boolean', 'PID', 'TID');

implementation

uses
  MsgMon.System.Data.VKeyNames;

{ TMessageDetailRenderer }

class function TDetailRenderer.RenderMessage(context: TMMDataContext; data: TMMMessage; IncludeDefaults: Boolean): TMessageDetails;
var
  Defaults: TMessageDetails;
begin
  if IncludeDefaults then
    Defaults := TMessageDetailRenderer.RenderDefaults(context, data);

  case data.message of
    WM_KEYDOWN,
    WM_KEYUP,
    WM_SYSKEYDOWN,
    WM_SYSKEYUP:
      Result := TMessageDetailRenderer.WMKey(context, data);
    WM_WINDOWPOSCHANGED,
    WM_WINDOWPOSCHANGING:
      Result := TMessageDetailRenderer.WMWindowPosChanging(context, data);
  else
    SetLength(Result, 0);
  end;

  if IncludeDefaults then
    Result := Defaults + Result;

  SetContextInfo(Result, context, data.index);
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
  SetLength(Result, 10);
  Result[0].SetTitle('Key Event');
  Result[1].SetInt('vkey', data.wParam);
  Result[2].SetString('vkey', IfThen(data.wParam < $100, SVKeyNames[data.wParam], ''));
  Result[3].SetInt('repeat', data.lParam and $0000FFFF);
  Result[4].SetInt('scancode', (data.lParam and $00FF0000) shr 16);
  Result[5].SetBool('extended', (data.lParam and $01000000) = $01000000);
  Result[6].SetInt('reserved', (data.lParam and $1E000000) shr 25);
  Result[7].SetBool('context', (data.lParam and $20000000) = $20000000);
  Result[8].SetBool('previous', (data.lParam and $40000000) = $40000000);
  Result[9].SetBool('transition', (data.lParam and $80000000) = $80000000);
end;

class function TMessageDetailRenderer.RenderDefaults(context: TMMDataContext; data: TMMMessage): TMessageDetails;
begin
  // TODO: Add wparam, lparam, message time, etc
  SetLength(Result, 2);
  Result[0].SetHwnd('hwnd', data.hwnd);
  Result[1].SetInt('message', data.message);
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
  SetLength(Result, 9);
  Result[0].SetTitle('(WINDOWPOS) lParam');
  Result[1].SetHwnd('hwnd', wp.hwnd);
  Result[2].SetHwnd('hwndInsertAfter', wp.hwndInsertAfter);
  Result[3].SetInt('x', wp.x);
  Result[4].SetInt('y', wp.y);
  Result[5].SetInt('cx', wp.cx);
  Result[6].SetInt('cy', wp.cy);
  Result[7].SetInt('flags', wp.flags);
  Result[8].SetString('flags', FlagsToString(wp.flags, flags));
end;

{ TMessageDetailRow }

function TMessageDetailRow.RenderToString(IncludeTitle: Boolean): string;
var
  ws: TMMWindows;
  w: TMMWindow;
  ps: TMMProcesses;
  p: TMMProcess;
begin
  case ValueType of
    mdrInteger: Result := IntToStr(FValueInteger);
    mdrString: Result := FValueString;
    mdrBoolean: Result := BoolToStr(FValueBoolean, True);
    mdrTitle: Result := '';
    mdrPID:
      begin
        if FContext.Processes.TryGetValue(FValuePID, ps) and (ps.Count > 0)
          then p := ps[0]
          else p := nil;

        if Assigned(p) then
          Result := p.Render(True)
        else
          Result := IntToStr(FValuePID);
      end;
    mdrTID:
      begin
        Result := IntToStr(FValueTID);
      end;
    mdrHwnd:
      begin
        if FContext.Windows.TryGetValue(FValueHwnd, ws)
          then w := ws.FromBase(row)
          else w := nil;

        if Assigned(w) then
        begin
          Result := w.Render(True);
        end
        else
          Result := IntToStr(FValueHwnd);
      end;
  end;
  if IncludeTitle then
    Result := FName + ': ' + Result;
end;

procedure TMessageDetailRow.DoSet(valueType: TMessageDetailRowType; n: string);
begin
  Self.FValueType := valueType;
  Self.FName := n;
end;

procedure TMessageDetailRow.SetHwnd(n: string; v: HWND);
begin
  Self.DoSet(mdrHwnd, n);
  Self.FValueHwnd := v;
  Self.FValueLink := v;
end;

procedure TMessageDetailRow.SetInt(n: string; v: Integer);
begin
  Self.DoSet(mdrInteger, n);
  Self.FValueInteger := v;
end;

procedure TMessageDetailRow.SetPID(n: string; v: Cardinal);
begin
  Self.DoSet(mdrPID, n);
  Self.FValuePID := v;
  Self.FValueLink := v;
end;

procedure TMessageDetailRow.SetString(n, v: string);
begin
  Self.DoSet(mdrString, n);
  Self.FValueString := v;
end;

procedure TMessageDetailRow.SetTID(n: string; v: Cardinal);
begin
  Self.DoSet(mdrTID, n);
  Self.FValueTID := v;
  Self.FValueLink := v;
end;

procedure TMessageDetailRow.SetTitle(n: string);
begin
  Self.DoSet(mdrTitle, n);
end;

procedure TMessageDetailRow.SetBool(n: string; v: Boolean);
begin
  Self.DoSet(mdrBoolean, n);
  Self.FValueBoolean := v;
end;

procedure TMessageDetailRow.SetContextInfo(context: TMMDataContext; row: Integer);
begin
  Self.FContext := context;
  Self.FRow := row;
end;

{ TDetailRenderer }

class function TDetailRenderer.RenderProcess(context: TMMDataContext;
  process: TMMProcess): TMessageDetails;
var
  p: string;
begin
  SetLength(Result, 3);
  Result[0].SetString('Process Name', process.processName);
  Result[1].SetString('Command Line', process.commandLine);
  case process.platform_ of
    PLATFORM_X86: p := 'x86';
    PLATFORM_X64: p := 'x64';
    else p := 'Unknown';
  end;
  Result[2].SetString('Architecture', p);
  SetContextInfo(Result, context, 0);
end;

class function TDetailRenderer.RenderThread(context: TMMDataContext;
  thread: TMMThread): TMessageDetails;
begin
  SetLength(Result, 1);
  Result[0].SetPID('PID', thread.PID);
  SetContextInfo(Result, context, 0);
end;

class function TDetailRenderer.RenderWindow(context: TMMDataContext;
  window: TMMWindow): TMessageDetails;
begin
  SetLength(Result, 6);
  Result[0].SetPID('PID', window.pid);
  Result[1].SetTID('TID', window.tid);
  Result[2].SetHwnd('hwndOwner', window.hwndOwner);
  Result[3].SetHwnd('hwndParent', window.hwndParent);
  Result[4].SetString('Class', window.ClassName);
  Result[5].SetString('Real Class', window.RealClassName);
  SetContextInfo(Result, context, 0);
end;

class procedure TDetailRenderer.SetContextInfo(var details: TMessageDetails;
  context: TMMDataContext; row: Integer);
var
  i: Integer;
begin
  for i := 0 to High(details) do
    details[i].SetContextInfo(context, row);
end;

end.
