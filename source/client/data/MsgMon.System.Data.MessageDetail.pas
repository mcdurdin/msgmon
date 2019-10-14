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
  TMessageDetailRowType = (mdrTitle, mdrInteger, mdrInt64, mdrString, mdrHwnd, mdrBoolean, mdrPID, mdrTID);

  TMessageDetailRow = record
  strict private
    FName: string;
    FValueHWND: HWND;
    FValueType: TMessageDetailRowType;
    FValueBoolean: Boolean;
    FValueString: string;
    FValueInteger: Integer;
    FMessageContext: TMMMessageContext;
    FRow: Integer;
    procedure DoSet(valueType: TMessageDetailRowType; n: string);
  private
    FValuePID: Cardinal;
    FValueTID: Cardinal;
    FValueLink: Integer;
    FValueInt64: Int64;
    procedure SetContextInfo(messageContext: TMMMessageContext; row: Integer);
    procedure SetHwnd(n: string; v: HWND);
    procedure SetBool(n: string; v: Boolean);
    procedure SetInt(n: string; v: Integer);
    procedure SetPID(n: string; v: Cardinal);
    procedure SetTID(n: string; v: Cardinal);
    procedure SetString(n, v: string);
    procedure SetTitle(n: string);
    procedure SetInt64(n: string; v: Int64);
  public
    function RenderToString(IncludeTitle: Boolean): string;
    property Name: string read FName;
    property Row: Integer read FRow;
    property ValueType: TMessageDetailRowType read FValueType;
    property ValueLink: Integer read FValueLink;
    property ValueInteger: Integer read FValueInteger;
    property ValueInt64: Int64 read FValueInt64;
    property ValueBoolean: Boolean read FValueBoolean;
    property ValueString: string read FValueString;
    property ValueHwnd: HWND read FValueHWND;
    property ValuePID: Cardinal read FValuePID;
    property ValueTID: Cardinal read FValueTID;
  end;

  TMessageDetails = TArray<TMessageDetailRow>;

  TMessageDetailRenderer = class
  private
    class function WMWindowPosChanging(data: TMMMessage): TMessageDetails;
    class function WMKey(data: TMMMessage): TMessageDetails;
    class function WMCreate(data: TMMMessage): TMessageDetails;
    class function RenderDefaults(data: TMMMessage): TMessageDetails;
  public
  end;

  TDetailRenderer = class
  private
    class procedure SetContextInfo(var details: TMessageDetails; messageContext: TMMMessageContext; row: Integer);
  public
    class function RenderProcess(messageContext: TMMMessageContext; process: TMMProcess): TMessageDetails;
    class function RenderThread(messageContext: TMMMessageContext; thread: TMMThread): TMessageDetails;
    class function RenderWindow(messageContext: TMMMessageContext; window: TMMWindow): TMessageDetails;
    class function RenderMessage(data: TMMMessage; IncludeDefaults: Boolean): TMessageDetails;
  end;

const
  MessageDetailRowTypeNames: array[TMessageDetailRowType] of string = ('Title', 'Integer', 'Int64', 'String', 'Hwnd', 'Boolean', 'PID', 'TID');

implementation

uses
  MsgMon.System.Data.VKeyNames;

{ TMessageDetailRenderer }

class function TDetailRenderer.RenderMessage(data: TMMMessage; IncludeDefaults: Boolean): TMessageDetails;
var
  Defaults: TMessageDetails;
begin
  if IncludeDefaults then
    Defaults := TMessageDetailRenderer.RenderDefaults(data);

  case data.message of
    WM_KEYDOWN,
    WM_KEYUP,
    WM_SYSKEYDOWN,
    WM_SYSKEYUP:
      Result := TMessageDetailRenderer.WMKey(data);
    WM_WINDOWPOSCHANGED,
    WM_WINDOWPOSCHANGING:
      Result := TMessageDetailRenderer.WMWindowPosChanging(data);
    WM_CREATE:
      Result := TMessageDetailRenderer.WMCreate(data);
  else
    SetLength(Result, 0);
  end;

  if IncludeDefaults then
    Result := Defaults + Result;

  SetContextInfo(Result, data.Context, data.index);
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

class function TMessageDetailRenderer.WMCreate(data: TMMMessage): TMessageDetails;
var
  wp: PCreateStructW;
const style: array[0..19] of TFlag = (
  (v: WS_BORDER; n: 'WS_BORDER'),
  (v: WS_CAPTION; n: 'WS_CAPTION'),
  (v: WS_CHILD; n: 'WS_CHILD'),
  (v: WS_CLIPCHILDREN; n: 'WS_CLIPCHILDREN'),
  (v: WS_CLIPSIBLINGS; n: 'WS_CLIPSIBLINGS'),
  (v: WS_DISABLED; n: 'WS_DISABLED'),
  (v: WS_DLGFRAME; n: 'WS_DLGFRAME'),
  (v: WS_GROUP; n: 'WS_GROUP'),
  (v: WS_HSCROLL; n: 'WS_HSCROLL'),
  (v: WS_ICONIC; n: 'WS_ICONIC'),
  (v: WS_MAXIMIZE; n: 'WS_MAXIMIZE'),
  (v: WS_MAXIMIZEBOX; n: 'WS_MAXIMIZEBOX'),
  (v: WS_MINIMIZE; n: 'WS_MINIMIZE'),
  (v: WS_MINIMIZEBOX; n: 'WS_MINIMIZEBOX'),
  (v: WS_POPUP; n: 'WS_POPUP'),
  (v: WS_SIZEBOX; n: 'WS_SIZEBOX'),
  (v: WS_SYSMENU; n: 'WS_SYSMENU'),
  (v: WS_TABSTOP; n: 'WS_TABSTOP'),
  (v: WS_VISIBLE; n: 'WS_VISIBLE'),
  (v: WS_VSCROLL; n: 'WS_VSCROLL')
);
const WS_EX_NOREDIRECTIONBITMAP = $00200000;
const dwExStyle: array[0..22] of TFlag = (
  (v: WS_EX_ACCEPTFILES; n: 'WS_EX_ACCEPTFILES'),
  (v: WS_EX_APPWINDOW; n: 'WS_EX_APPWINDOW'),
  (v: WS_EX_CLIENTEDGE; n: 'WS_EX_CLIENTEDGE'),
  (v: WS_EX_COMPOSITED; n: 'WS_EX_COMPOSITED'),
  (v: WS_EX_CONTEXTHELP; n: 'WS_EX_CONTEXTHELP'),
  (v: WS_EX_CONTROLPARENT; n: 'WS_EX_CONTROLPARENT'),
  (v: WS_EX_DLGMODALFRAME; n: 'WS_EX_DLGMODALFRAME'),
  (v: WS_EX_LAYERED; n: 'WS_EX_LAYERED'),
  (v: WS_EX_LAYOUTRTL; n: 'WS_EX_LAYOUTRTL'),
  (v: WS_EX_LEFTSCROLLBAR; n: 'WS_EX_LEFTSCROLLBAR'),
  (v: WS_EX_LTRREADING; n: 'WS_EX_LTRREADING'),
  (v: WS_EX_MDICHILD; n: 'WS_EX_MDICHILD'),
  (v: WS_EX_NOACTIVATE; n: 'WS_EX_NOACTIVATE'),
  (v: WS_EX_NOINHERITLAYOUT; n: 'WS_EX_NOINHERITLAYOUT'),
  (v: WS_EX_NOPARENTNOTIFY; n: 'WS_EX_NOPARENTNOTIFY'),
  (v: WS_EX_NOREDIRECTIONBITMAP; n: 'WS_EX_NOREDIRECTIONBITMAP'),
  (v: WS_EX_RIGHT; n: 'WS_EX_RIGHT'),
  (v: WS_EX_RTLREADING; n: 'WS_EX_RTLREADING'),
  (v: WS_EX_STATICEDGE; n: 'WS_EX_STATICEDGE'),
  (v: WS_EX_TOOLWINDOW; n: 'WS_EX_TOOLWINDOW'),
  (v: WS_EX_TOPMOST; n: 'WS_EX_TOPMOST'),
  (v: WS_EX_TRANSPARENT; n: 'WS_EX_TRANSPARENT'),
  (v: WS_EX_WINDOWEDGE; n: 'WS_EX_WINDOWEDGE')
);
begin
  wp := PCreateStructW(@data.detail[0]);
  SetLength(Result, 12);
  Result[0].SetTitle('(CREATESTRUCT) lParam');
  Result[1].SetInt('hInstance', wp.hInstance);
  Result[2].SetInt('hMenu', wp.hMenu);
  Result[3].SetHwnd('hwndParent', wp.hwndParent);
  Result[4].SetInt('cy', wp.cy);
  Result[5].SetInt('cx', wp.cx);
  Result[6].SetInt('y', wp.y);
  Result[7].SetInt('x', wp.x);
  Result[8].SetInt('style', wp.style);
  Result[9].SetString('style', FlagsToString(wp.style, style));
  Result[10].SetInt('dwExStyle', wp.dwExStyle);
  Result[11].SetString('dwExStyle', FlagsToString(wp.dwExStyle, dwExStyle));
end;

class function TMessageDetailRenderer.WMKey(data: TMMMessage): TMessageDetails;
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

class function TMessageDetailRenderer.RenderDefaults(data: TMMMessage): TMessageDetails;
begin
  // TODO: Add wparam, lparam, message time, etc
  SetLength(Result, 6);
  Result[0].SetHwnd('hwnd', data.hwnd);
  Result[1].SetInt('message', data.message);
  // TODO: render message name with column renderer
  if data.messageName <> nil
    then Result[2].SetString('message', data.messageName.name)
    else Result[2].SetInt('message', data.message);
  Result[3].SetInt64('wParam', data.wParam);
  Result[4].SetInt64('lParam', data.lParam);
  Result[5].SetInt64('lResult', data.lResult);
  //TODO Result[6].SetTimestamp('Timestamp', data.timestamp);
end;

class function TMessageDetailRenderer.WMWindowPosChanging(data: TMMMessage): TMessageDetails;
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
  w: TMMWindow;
  p: TMMProcess;
  t: TMMThread;
begin
  // TODO: Merge this with column rendering
  case ValueType of
    mdrInteger: Result := IntToStr(FValueInteger);
    mdrInt64: Result := IntToStr(FValueInt64);
    mdrString: Result := FValueString;
    mdrBoolean: Result := BoolToStr(FValueBoolean, True);
    mdrTitle: Result := '';
    mdrPID:
      begin
        if FMessageContext.Processes.TryGetValue(FValuePID, p)
          then Result := p.Render(True)
          else Result := IntToStr(FValuePID); // TODO: Use default renderer
      end;
    mdrTID:
      begin
        if FMessageContext.Threads.TryGetValue(FValueTID, t)
          then Result := t.Render(True)
          else Result := IntToStr(FValueTID); // TODO: Use default renderer
      end;
    mdrHwnd:
      begin
        if FMessageContext.Windows.TryGetValue(FValueHwnd, w)
          then Result := w.Render(True)
          else Result := TMMWindow.BaseRender(FValueHwnd);
      end;
    {mdrMessage:
      begin
        // TODO: Use default renderer
        if FValueString <> ''
          then Result := FValueString
          else Result := IntToStr(FValueInteger);
      end;}
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

procedure TMessageDetailRow.SetInt64(n: string; v: Int64);
begin
  Self.DoSet(mdrInt64, n);
  Self.FValueInt64 := v;
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

procedure TMessageDetailRow.SetContextInfo(messageContext: TMMMessageContext; row: Integer);
begin
  Self.FMessageContext := messageContext;
  Self.FRow := row;
end;

{ TDetailRenderer }

class function TDetailRenderer.RenderProcess(
  messageContext: TMMMessageContext;
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
  SetContextInfo(Result, messageContext, 0);
end;

class function TDetailRenderer.RenderThread(
  messageContext: TMMMessageContext;
  thread: TMMThread): TMessageDetails;
begin
  SetLength(Result, 11);
  Result[0].SetTID('TID', thread.tid);
  Result[1].SetPID('PID', thread.pid);
  Result[2].SetString('Description', thread.threadDescription);
  Result[3].SetBool('Foreground?', thread.isForegroundThread);
  Result[4].SetHwnd('hwndFocus', thread.hwndFocus);
  Result[5].SetHwnd('hwndActive', thread.hwndActive);
  Result[6].SetHwnd('hwndCapture', thread.hwndCapture);
  Result[7].SetHwnd('hwndCaret', thread.hwndCaret);
  Result[8].SetHwnd('hwndMenuOwner', thread.hwndMenuOwner);
  Result[9].SetHwnd('hwndMoveSize', thread.hwndMoveSize);
  Result[10].SetInt('activeHKL', thread.activeHKL);

  SetContextInfo(Result, messageContext, 0);
end;

class function TDetailRenderer.RenderWindow(
  messageContext: TMMMessageContext;
  window: TMMWindow): TMessageDetails;
begin
  SetLength(Result, 6);
  Result[0].SetPID('PID', window.pid);
  Result[1].SetTID('TID', window.tid);
  Result[2].SetHwnd('hwndOwner', window.hwndOwner);
  Result[3].SetHwnd('hwndParent', window.hwndParent);
  Result[4].SetString('Class', window.ClassName);
  Result[5].SetString('Real Class', window.RealClassName);
  SetContextInfo(Result, messageContext, 0);
end;

class procedure TDetailRenderer.SetContextInfo(var details: TMessageDetails;
  messageContext: TMMMessageContext; row: Integer);
var
  i: Integer;
begin
  for i := 0 to High(details) do
    details[i].SetContextInfo(messageContext, row);
end;

end.
