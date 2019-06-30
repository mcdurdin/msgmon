unit MsgMon.System.Data.Column;

interface

uses
  System.Classes,
  System.Contnrs,
  System.Generics.Collections,
  System.JSON,
  System.SysUtils,
  Winapi.Messages,

  MsgMon.System.Data.Context,
  MsgMon.System.Data.Message,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  // Filter actions here to avoid circular dependencies. I could structure this better
  TMMFilterRelation = (frIs, frIsNot, frLessThan, frMoreThan, frBeginsWith, frEndsWith, frContains, frExcludes);
  TMMFilterAction = (faInclude, faExclude);

  TMMColumn = class
  private
    FWidth: Integer;
    FContext: TMMDataContext;
  protected
    class function DefaultWidth: Integer; virtual;
    class function GetCaption: string; virtual;
    function DoLoad(o: TJSONObject): Boolean; virtual;
    procedure DoSave(o: TJSONObject); virtual;
    function DoRender(data: TMMMessage): string; virtual; abstract;
    function DoCompare(d1, d2: TMMMessage): Integer; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; virtual; abstract;
  public
    constructor Create(context: TMMDataContext); virtual;
    function Clone: TMMColumn;
    function Load(o: TJSONObject): Boolean;
    procedure Save(o: TJSONObject);
    function Render(data: TMMMessage): string;
    function Compare(d1, d2: TMMMessage): Integer;
    function Filter(data: TMMMessage; relation: TMMFilterRelation; const value: string; action: TMMFilterAction): Boolean;
    class function Caption: string;
    property Width: Integer read FWidth write FWidth;
  end;

  TMMColumnClass = class of TMMColumn;

  { Abstract column types }

  TMMColumn_Integer = class(TMMColumn)
  protected
    class function DefaultWidth: Integer; override;
    function DoRender(data: TMMMessage): string; override;
    function DoCompare(d1, d2: TMMMessage): Integer; override;
    function GetData(data: TMMMessage): Integer; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_String = class(TMMColumn)
  protected
    function DoRender(data: TMMMessage): string; override;
    function DoCompare(d1, d2: TMMMessage): Integer; override;
    function GetData(data: TMMMessage): string; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_Time = class(TMMColumn)
  protected
    class function DefaultWidth: Integer; override;
    function DoRender(data: TMMMessage): string; override;
    function DoCompare(d1, d2: TMMMessage): Integer; override;
    function GetData(data: TMMMessage): TDateTime; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_Window = class(TMMColumn)
  protected
    class function DefaultWidth: Integer; override;
    function DoRender(data: TMMMessage): string; override;
    function DoCompare(d1, d2: TMMMessage): Integer; override;
    function GetData(data: TMMMessage): Cardinal; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_WindowClass = class(TMMColumn)
  protected
    class function DefaultWidth: Integer; override;
    function DoRender(data: TMMMessage): string; override;
    function DoCompare(d1, d2: TMMMessage): Integer; override;
    function GetData(data: TMMMessage): Cardinal; virtual; abstract;
    function DoFilter(data: TMMMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  { Actual columns }

  TMMColumn_Sequence = class(TMMColumn_Integer)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_ProcessArchitecture = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_ProcessName = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_ProcessPath = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_CommandLine = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

//  TMMColumn_EventTime = class(TMMColumn_Time)
//  protected
//    function GetData(data: TMsgMonMessage): TDateTime; override;
//  end;

//  TMMColumn_MessageTime = class(TMMColumn_Time)
//  protected
//    function GetData(data: TMsgMonMessage): TDateTime; override;
//  end;

  TMMColumn_PID = class(TMMColumn_Integer)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_TID = class(TMMColumn_Integer)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_hWnd = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWnd_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_Mode = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_MessageName = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_MessageID = class(TMMColumn_Integer)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_wParam = class(TMMColumn_Integer)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_lParam = class(TMMColumn_Integer)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_lResult = class(TMMColumn_Integer)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Integer; override;
  end;

  TMMColumn_Detail = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_MessageScope = class(TMMColumn_String)
  protected
    class function DefaultWidth: Integer; override;
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): string; override;
  end;

  TMMColumn_hWndFocus = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndFocus_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndActive = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndActive_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndCapture = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndCapture_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndCaret = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndCaret_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndMenuOwner = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndMenuOwner_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndMoveSize = class(TMMColumn_Window)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumn_hWndMoveSize_Class = class(TMMColumn_WindowClass)
  protected
    class function GetCaption: string; override;
    function GetData(data: TMMMessage): Cardinal; override;
  end;

  TMMColumnClassList = class(TClassList)
  protected
    function GetItems(Index: Integer): TMMColumnClass; inline;
    procedure SetItems(Index: Integer; AClass: TMMColumnClass); inline;
  public
    function Find(Name: string): TMMColumnClass;
    property Items[Index: Integer]: TMMColumnClass read GetItems write SetItems; default;
  end;

  TMMColumns = class(TObjectList<TMMColumn>)
  private
    FContext: TMMDataContext;
  public
    constructor Create(context: TMMDataContext);
    function LoadFromJSON(const definition: string): Boolean;
    procedure SaveToJSON(var definition: string);
    procedure LoadDefault;
    procedure LoadAll;
    function FindClassName(ClassName: string): TMMColumn;
  end;

var
  FColumnClasses: TMMColumnClassList = nil;
  FDefaultColumnClasses: TMMColumnClassList = nil;

implementation

uses
  System.StrUtils,

  MsgMon.System.Data.MessageDetail;

{ TMMColumn }

class function TMMColumn.Caption: string;
begin
  Result := GetCaption;
end;

function TMMColumn.Clone: TMMColumn;
begin
  Result := FColumnClasses.Find(Self.ClassName).Create(Self.FContext);
end;

function TMMColumn.Compare(d1, d2: TMMMessage): Integer;
begin
  Result := DoCompare(d1, d2);
end;

constructor TMMColumn.Create(context: TMMDataContext);
begin
  inherited Create;
  FContext := context;
  FWidth := DefaultWidth;
end;

class function TMMColumn.DefaultWidth: Integer;
begin
  Result := 64;
end;

class function TMMColumn.GetCaption: string;
begin
  Result := '???';
end;

function TMMColumn.DoLoad(o: TJSONObject): Boolean;
begin
  Result := True;
end;

procedure TMMColumn.DoSave(o: TJSONObject);
begin
  ;
end;

function TMMColumn.Filter(data: TMMMessage; relation: TMMFilterRelation;
  const value: string; action: TMMFilterAction): Boolean;
begin
  Result := DoFilter(data, relation, value);
  if action = faExclude then
    Result := not Result;
end;

function TMMColumn.Load(o: TJSONObject): Boolean;
begin
  FWidth := o.GetValue<Integer>('width', DefaultWidth);
  Result := DoLoad(o);
end;

function TMMColumn.Render(data: TMMMessage): string;
begin
  Result := DoRender(data);
end;

procedure TMMColumn.Save(o: TJSONObject);
begin
  o.AddPair('width', TJSONNumber.Create(FWidth));
  DoSave(o);
end;

{ TMMColumn_Integer }

class function TMMColumn_Integer.DefaultWidth: Integer;
begin
  Result := 92;
end;

function TMMColumn_Integer.DoCompare(d1, d2: TMMMessage): Integer;
begin
  Result := GetData(d1) - GetData(d2);
end;

function TMMColumn_Integer.DoFilter(data: TMMMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue, filterValue: Integer;
begin
  Result := False;

  dataValue := GetData(data);
  if not TryStrToInt(value, filterValue) then
    Exit;

  case relation of
    frIs:         Result := filterValue = dataValue;
    frIsNot:      Result := filterValue <> dataValue;
    frLessThan:   Result := filterValue < dataValue;
    frMoreThan:   Result := filterValue > dataValue;
    frBeginsWith: Result := IntToStr(dataValue).StartsWith(value);
    frEndsWith:   Result := IntToStr(dataValue).EndsWith(value);
    frContains:   Result := IntToStr(dataValue).Contains(value);
    frExcludes:   Result := not IntToStr(dataValue).Contains(value);
  end;
end;

function TMMColumn_Integer.DoRender(data: TMMMessage): string;
begin
  Result := IntToStr(GetData(data));
end;

{ TMMColumn_String }

function TMMColumn_String.DoCompare(d1, d2: TMMMessage): Integer;
begin
  Result := CompareText(GetData(d1), GetData(d2));
end;

function TMMColumn_String.DoFilter(data: TMMMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue: string;
begin
  Result := False;

  dataValue := GetData(data);

  case relation of
    frIs:         Result := SameText(value, dataValue);
    frIsNot:      Result := not SameText(value, dataValue);
    frLessThan:   Result := CompareText(value, dataValue) < 0;
    frMoreThan:   Result := CompareText(value, dataValue) > 0;
    frBeginsWith: Result := dataValue.StartsWith(value, True); // not strictly same as CompareText but good enough
    frEndsWith:   Result := dataValue.EndsWith(value, True); // not strictly same as CompareText but good enough
    frContains:   Result := ContainsText(dataValue, value);
    frExcludes:   Result := not ContainsText(dataValue, value);
  end;
end;

function TMMColumn_String.DoRender(data: TMMMessage): string;
begin
  Result := GetData(data);
end;

{ TMMColumn_Index }

class function TMMColumn_Sequence.DefaultWidth: Integer;
begin
  Result := 48;
end;

class function TMMColumn_Sequence.GetCaption: string;
begin
  Result := 'Sequence';
end;

function TMMColumn_Sequence.GetData(data: TMMMessage): Integer;
begin
  Result := data.Index;
end;

{ TMMColumn_ProcessName }

class function TMMColumn_ProcessName.DefaultWidth: Integer;
begin
  Result := 128;
end;

class function TMMColumn_ProcessName.GetCaption: string;
begin
  Result := 'Process Name';
end;

function TMMColumn_ProcessName.GetData(data: TMMMessage): string;
begin
  if data.process = nil
    then Result := IntToStr(data.pid)
    else Result := data.process.processName;
end;

{ TMMColumn_ProcessPath }

class function TMMColumn_ProcessPath.DefaultWidth: Integer;
begin
  Result := 200;
end;

class function TMMColumn_ProcessPath.GetCaption: string;
begin
  Result := 'Process Path';
end;

function TMMColumn_ProcessPath.GetData(data: TMMMessage): string;
begin
  if data.process = nil
    then Result := ''
    else Result := data.process.processPath;
end;

{ TMMColumn_CommandLine }

class function TMMColumn_CommandLine.DefaultWidth: Integer;
begin
  Result := 200;
end;

class function TMMColumn_CommandLine.GetCaption: string;
begin
  Result := 'Command Line';
end;

function TMMColumn_CommandLine.GetData(data: TMMMessage): string;
begin
  if data.process = nil
    then Result := ''
    else Result := data.process.CommandLine;
end;

{ TMMColumn_PID }

class function TMMColumn_PID.DefaultWidth: Integer;
begin
  Result := 48;
end;

class function TMMColumn_PID.GetCaption: string;
begin
  Result := 'PID';
end;

function TMMColumn_PID.GetData(data: TMMMessage): Integer;
begin
  Result := data.pid;
end;

{ TMMColumn_TID }

class function TMMColumn_TID.DefaultWidth: Integer;
begin
  Result := 48;
end;

class function TMMColumn_TID.GetCaption: string;
begin
  Result := 'TID';
end;

function TMMColumn_TID.GetData(data: TMMMessage): Integer;
begin
  Result := data.tid;
end;

{ TMMColumn_MessageName }

class function TMMColumn_MessageName.DefaultWidth: Integer;
begin
  Result := 160;
end;

class function TMMColumn_MessageName.GetCaption: string;
begin
  Result := 'Message';
end;

function TMMColumn_MessageName.GetData(data: TMMMessage): string;
begin
  if data.messageName = nil then
  begin
    if (data.message >= WM_USER) and (data.message <= $7FFF) then
      Result := Format('WM_USER+%d', [data.message-WM_USER])
    else if (data.message >= WM_APP) and (data.message <= $BFFF) then
      Result := Format('WM_APP+%d', [data.message-WM_APP])
    else
      Result := IntToStr(data.message)
  end
  else Result := data.messageName.name;
end;

{ TMMColumn_MessageID }

class function TMMColumn_MessageID.DefaultWidth: Integer;
begin
  Result := 64;
end;

class function TMMColumn_MessageID.GetCaption: string;
begin
  Result := 'Message#';
end;

function TMMColumn_MessageID.GetData(data: TMMMessage): Integer;
begin
  Result := data.message;
end;

{ TMMColumn_wParam }

class function TMMColumn_wParam.GetCaption: string;
begin
  Result := 'wParam';
end;

function TMMColumn_wParam.GetData(data: TMMMessage): Integer;
begin
  Result := data.wParam;
end;

{ TMMColumn_lParam }

class function TMMColumn_lParam.GetCaption: string;
begin
  Result := 'lParam';
end;

function TMMColumn_lParam.GetData(data: TMMMessage): Integer;
begin
  Result := data.lParam;
end;

{ TMMColumn_lResult }

class function TMMColumn_lResult.GetCaption: string;
begin
  Result := 'lResult';
end;

function TMMColumn_lResult.GetData(data: TMMMessage): Integer;
begin
  Result := data.lResult;
end;

{ TMMColumn_Detail }

class function TMMColumn_Detail.DefaultWidth: Integer;
begin
  Result := -1;
end;

class function TMMColumn_Detail.GetCaption: string;
begin
  Result := 'Detail';
end;

function TMMColumn_Detail.GetData(data: TMMMessage): string;
var
  i: Integer;
  d: TMessageDetails;
begin
  d := TMessageDetailRenderer.Render(FContext, data);
  if High(d) < 0 then
    Exit('');

  Result := d[0].RenderToString;
  for i := 1 to High(d) do
  begin
    Result := Result + '; ' + d[i].RenderToString;
  end;
end;

{ TMMColumn_MessageClass }

class function TMMColumn_MessageScope.DefaultWidth: Integer;
begin
  Result := 64;
end;

class function TMMColumn_MessageScope.GetCaption: string;
begin
  Result := 'Scope';
end;

function TMMColumn_MessageScope.GetData(data: TMMMessage): string;
begin
  if data.messageName = nil then
  begin
    if (data.message >= WM_USER) and (data.message <= $7FFF) then
      Result := 'User'
    else if (data.message >= WM_APP) and (data.message <= $BFFF) then
      Result := 'App'
    else
      Result := 'Unknown'
  end
  else
    Result := data.messageName.ScopeName;
end;

{ TMMColumn_Time }

class function TMMColumn_Time.DefaultWidth: Integer;
begin
  Result := 128;
end;

function TMMColumn_Time.DoCompare(d1, d2: TMMMessage): Integer;
begin
  if GetData(d1) < GetData(d2) then Result := 1
  else if GetData(d1) > GetData(d2) then Result := -1
  else Result := 0;
end;

function TMMColumn_Time.DoFilter(data: TMMMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue, filterValue: TDateTime;
begin
  Result := False;

  // TODO: This is garbage. Just want it to compile for now
  dataValue := GetData(data);
  if not TryStrToDateTime(value, filterValue) then
    Exit;

  case relation of
    frIs:         Result := filterValue = dataValue;
    frIsNot:      Result := filterValue <> dataValue;
    frLessThan:   Result := filterValue < dataValue;
    frMoreThan:   Result := filterValue > dataValue;
    frBeginsWith: Result := DateTimeToStr(dataValue).StartsWith(value);
    frEndsWith:   Result := DateTimeToStr(dataValue).EndsWith(value);
    frContains:   Result := DateTimeToStr(dataValue).Contains(value);
    frExcludes:   Result := not DateTimeToStr(dataValue).Contains(value);
  end;
end;

function TMMColumn_Time.DoRender(data: TMMMessage): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', GetData(data));
end;

{ TMMColumn_Mode }

class function TMMColumn_Mode.DefaultWidth: Integer;
begin
  Result := 24;
end;

class function TMMColumn_Mode.GetCaption: string;
begin
  Result := 'Mode';
end;

function TMMColumn_Mode.GetData(data: TMMMessage): string;
begin
  case data.mode of
    1: Result := 'P';
    2: Result := 'S';
    3: Result := 'R';
    else Result := '';
  end;
end;

{ TMMColumn_ProcessArchitecture }

class function TMMColumn_ProcessArchitecture.DefaultWidth: Integer;
begin
  Result := 32;
end;

class function TMMColumn_ProcessArchitecture.GetCaption: string;
begin
  Result := 'Architecture';
end;

function TMMColumn_ProcessArchitecture.GetData(data: TMMMessage): string;
begin
  if Assigned(data.process) then
    case data.process.platform_ of
      1: Result := 'x86';
      2: Result := 'x64';
      else Result := '';
    end;
end;

{ TMMColumn_hWnd }

class function TMMColumn_hWnd.GetCaption: string;
begin
  Result := 'hwnd';
end;

function TMMColumn_hWnd.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwnd;
end;

{ TMMColumn_Window }

class function TMMColumn_Window.DefaultWidth: Integer;
begin
  Result := 80;
end;

function TMMColumn_Window.DoCompare(d1, d2: TMMMessage): Integer;
begin
  Result := Integer(GetData(d1)) - Integer(GetData(d2));
end;

function TMMColumn_Window.DoFilter(data: TMMMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue: string;
  filterValueInt: Integer;
  dataValueInt: Integer;
begin
  Result := False;

  dataValueInt := Integer(GetData(data));
  dataValue := DoRender(data); // TODO we could be nuanced here

  if not TryStrToInt(value, filterValueInt) then
    Exit;

  case relation of
    frIs:         Result := filterValueInt = dataValueInt;
    frIsNot:      Result := filterValueInt <> dataValueInt;
    frLessThan:   Result := filterValueInt < dataValueInt;
    frMoreThan:   Result := filterValueInt > dataValueInt;
    frBeginsWith: Result := dataValue.StartsWith(value);
    frEndsWith:   Result := dataValue.EndsWith(value);
    frContains:   Result := dataValue.Contains(value);
    frExcludes:   Result := not dataValue.Contains(value);
  end;
end;

function TMMColumn_Window.DoRender(data: TMMMessage): string;
var
  hwnd: Cardinal;
begin
  hwnd := GetData(data);
  Result := IntToStr(hwnd);
end;

{ TMMColumn_hWndFocus }

class function TMMColumn_hWndFocus.GetCaption: string;
begin
  Result := 'hwndFocus';
end;

function TMMColumn_hWndFocus.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndFocus;
end;

{ TMMColumn_hWndActive }

class function TMMColumn_hWndActive.GetCaption: string;
begin
  Result := 'hwndActive';
end;

function TMMColumn_hWndActive.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndActive;
end;

{ TMMColumn_hWndCapture }

class function TMMColumn_hWndCapture.GetCaption: string;
begin
  Result := 'hwndCapture';
end;

function TMMColumn_hWndCapture.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndCapture;
end;

{ TMMColumn_hWndCaret }

class function TMMColumn_hWndCaret.GetCaption: string;
begin
  Result := 'hwndCaret';
end;

function TMMColumn_hWndCaret.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndCaret;
end;

{ TMMColumn_hWndMenuOwner }

class function TMMColumn_hWndMenuOwner.GetCaption: string;
begin
  Result := 'hwndMenuOwner';
end;

function TMMColumn_hWndMenuOwner.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndMenuOwner;
end;

{ TMMColumn_hWndMoveSize }

class function TMMColumn_hWndMoveSize.GetCaption: string;
begin
  Result := 'hwndMoveSize';
end;

function TMMColumn_hWndMoveSize.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndMoveSize;
end;

{ TMMColumn_hWndClass }

class function TMMColumn_hWnd_Class.GetCaption: string;
begin
  Result := 'hwnd class';
end;

function TMMColumn_hWnd_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwnd;
end;

{ TMMColumn_WindowClass }

class function TMMColumn_WindowClass.DefaultWidth: Integer;
begin
  Result := 128;
end;

function TMMColumn_WindowClass.DoCompare(d1, d2: TMMMessage): Integer;
begin
  Result := Integer(GetData(d1)) - Integer(GetData(d2));
end;

function TMMColumn_WindowClass.DoFilter(data: TMMMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue: string;
begin
  Result := False;

  dataValue := DoRender(data); // TODO we could be nuanced here

  case relation of
    frIs:         Result := SameText(value, dataValue);
    frIsNot:      Result := not SameText(value, dataValue);
    frLessThan:   Result := CompareText(value, dataValue) < 0;
    frMoreThan:   Result := CompareText(value, dataValue) > 0;
    frBeginsWith: Result := dataValue.StartsWith(value, True); // not strictly same as CompareText but good enough
    frEndsWith:   Result := dataValue.EndsWith(value, True); // not strictly same as CompareText but good enough
    frContains:   Result := ContainsText(dataValue, value);
    frExcludes:   Result := not ContainsText(dataValue, value);
  end;
end;

function TMMColumn_WindowClass.DoRender(data: TMMMessage): string;
var
  hwnd: Cardinal;
  ws: TMMWindows;
  w: TMMWindow;
begin
  hwnd := GetData(data);

  if FContext.Windows.TryGetValue(hwnd, ws)
    then w := ws.FromBase(data.index)
    else w := nil;

  if Assigned(w) then
  begin
    Result := w.Render(False);
  end
  else
    Result := IntToStr(hwnd);
end;

{ TMMColumn_hWndMoveSize_Class }

class function TMMColumn_hWndMoveSize_Class.GetCaption: string;
begin
  Result := 'hwndMoveSize Class';
end;

function TMMColumn_hWndMoveSize_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndMoveSize;
end;

{ TMMColumn_hWndMenuOwner_Class }

class function TMMColumn_hWndMenuOwner_Class.GetCaption: string;
begin
  Result := 'hwndMenuOwner Class';
end;

function TMMColumn_hWndMenuOwner_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndMenuOwner;
end;

{ TMMColumn_hWndCaret_Class }

class function TMMColumn_hWndCaret_Class.GetCaption: string;
begin
  Result := 'hwndCaret Class';
end;

function TMMColumn_hWndCaret_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndCaret;
end;

{ TMMColumn_hWndCapture_Class }

class function TMMColumn_hWndCapture_Class.GetCaption: string;
begin
  Result := 'hwndCapture Class';
end;

function TMMColumn_hWndCapture_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndCapture;
end;

{ TMMColumn_hWndActive_Class }

class function TMMColumn_hWndActive_Class.GetCaption: string;
begin
  Result := 'hwndActive Class';
end;

function TMMColumn_hWndActive_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndActive;
end;

{ TMMColumn_hWndFocus_Class }

class function TMMColumn_hWndFocus_Class.GetCaption: string;
begin
  Result := 'hwndFocus Class';
end;

function TMMColumn_hWndFocus_Class.GetData(data: TMMMessage): Cardinal;
begin
  Result := data.hwndFocus;
end;

{ TMMColumns }

constructor TMMColumns.Create(context: TMMDataContext);
begin
  inherited Create;
  FContext := context;
end;

function TMMColumns.FindClassName(ClassName: string): TMMColumn;
begin
  for Result in Self do
    if Result.ClassNameIs(ClassName) then
      Exit;

  Result := nil;
end;

procedure TMMColumns.LoadAll;
var
  i: Integer;
  c: TMMColumnClass;
begin
  Clear;
  for i := 0 to FColumnClasses.Count - 1 do
  begin
    c := FColumnClasses[i];
    Add(c.Create(FContext));
  end;
end;

procedure TMMColumns.LoadDefault;
var
  i: Integer;
  c: TMMColumnClass;
begin
  Clear;
  for i := 0 to FDefaultColumnClasses.Count - 1 do
  begin
    c := FDefaultColumnClasses[i];
    Add(c.Create(FContext));
  end;
end;

function TMMColumns.LoadFromJSON(const definition: string): Boolean;
var
  j: TJSONValue;
  o: TJSONObject;
  a: TJSONArray;
  i: Integer;
  c: TMMColumn;
  cc: TMMColumnClass;
  jWidth: TJSONValue;
  jType: TJSONValue;
begin
  Clear;
  j := TJSONObject.ParseJSONValue(definition);
  if not (j is TJSONObject) then Exit(False);
  o := j as TJSONObject;
  j := o.GetValue('columns');
  if not Assigned(j) or not (j is TJSONArray) then Exit(False);
  a := j as TJSONArray;
  for i := 0 to a.Count - 1 do
  begin
    j := a.Items[i];
    if not (j is TJSONObject) then
      Exit(False);
    o := j as TJSONObject;
    jType := o.GetValue('type');
    if not Assigned(jType) or not (jType is TJSONString) then
      Exit(False);
    jWidth := o.GetValue('width');
    if not Assigned(jWidth) or not (jWidth is TJSONNumber) then
      Exit(False);

    cc := FColumnClasses.Find(jType.Value);
    if not Assigned(cc) then
      Exit(False);
    c := cc.Create(FContext);
    c.Width := (jWidth as TJSONNumber).AsInt;
    Add(c);
  end;
  Result := True;
end;

procedure TMMColumns.SaveToJSON(var definition: string);
var
  o, co: TJSONObject;
  a: TJSONArray;
  i: Integer;
begin
  o := TJSONObject.Create;
  a := TJSONArray.Create;
  o.AddPair('columns', a);
  for i := 0 to Count - 1 do
  begin
    co := TJSONObject.Create;
    co.AddPair('type', Items[i].ClassName);
    co.AddPair('width', TJSONNumber.Create(Items[i].Width));
    a.Add(co);
  end;

  definition := o.ToString;
end;

{ TMMColumnClassList }

function TMMColumnClassList.Find(Name: string): TMMColumnClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[i].ClassName = Name then
      Exit(Items[i]);
  Result := nil;
end;

function TMMColumnClassList.GetItems(Index: Integer): TMMColumnClass;
begin
  Result := TMMColumnClass(inherited Items[Index]);
end;

procedure TMMColumnClassList.SetItems(Index: Integer; AClass: TMMColumnClass);
begin
  inherited Items[Index] := AClass;
end;

initialization
  //
  // All available columns
  //
  FColumnClasses := TMMColumnClassList.Create;
  FColumnClasses.Add(TMMColumn_Sequence);
  FColumnClasses.Add(TMMColumn_ProcessArchitecture);
  FColumnClasses.Add(TMMColumn_ProcessName);
  FColumnClasses.Add(TMMColumn_PID);
  FColumnClasses.Add(TMMColumn_TID);
  FColumnClasses.Add(TMMColumn_Mode);
  FColumnClasses.Add(TMMColumn_hWnd);
  FColumnClasses.Add(TMMColumn_hWnd_Class);
  FColumnClasses.Add(TMMColumn_MessageName);
  FColumnClasses.Add(TMMColumn_MessageScope);
  FColumnClasses.Add(TMMColumn_wParam);
  FColumnClasses.Add(TMMColumn_lParam);
  FColumnClasses.Add(TMMColumn_lResult);
  FColumnClasses.Add(TMMColumn_Detail);
  FColumnClasses.Add(TMMColumn_hWndFocus);
  FColumnClasses.Add(TMMColumn_hWndFocus_Class);
  FColumnClasses.Add(TMMColumn_hWndActive);
  FColumnClasses.Add(TMMColumn_hWndActive_Class);
  FColumnClasses.Add(TMMColumn_hWndCapture);
  FColumnClasses.Add(TMMColumn_hWndCapture_Class);
  FColumnClasses.Add(TMMColumn_hWndCaret);
  FColumnClasses.Add(TMMColumn_hWndCaret_Class);
  FColumnClasses.Add(TMMColumn_hWndMenuOwner);
  FColumnClasses.Add(TMMColumn_hWndMenuOwner_Class);
  FColumnClasses.Add(TMMColumn_hWndMoveSize);
  FColumnClasses.Add(TMMColumn_hWndMoveSize_Class);

  //
  // Default columns
  //
  FDefaultColumnClasses := TMMColumnClassList.Create;
  FDefaultColumnClasses.Add(TMMColumn_Sequence);
  FDefaultColumnClasses.Add(TMMColumn_ProcessArchitecture);
  FDefaultColumnClasses.Add(TMMColumn_ProcessName);
  FDefaultColumnClasses.Add(TMMColumn_PID);
  FDefaultColumnClasses.Add(TMMColumn_TID);
  FDefaultColumnClasses.Add(TMMColumn_Mode);
  FDefaultColumnClasses.Add(TMMColumn_hWnd);
  FDefaultColumnClasses.Add(TMMColumn_hWnd_Class);
  FDefaultColumnClasses.Add(TMMColumn_MessageName);
  FDefaultColumnClasses.Add(TMMColumn_MessageScope);
  FDefaultColumnClasses.Add(TMMColumn_wParam);
  FDefaultColumnClasses.Add(TMMColumn_lParam);
  FDefaultColumnClasses.Add(TMMColumn_lResult);
  FDefaultColumnClasses.Add(TMMColumn_Detail);

finalization
  FreeAndNil(FColumnClasses);
  FreeAndNil(FDefaultColumnClasses);
end.
