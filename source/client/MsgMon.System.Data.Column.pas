unit MsgMon.System.Data.Column;

interface

uses
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
    FContext: TMsgMonContext;
  protected
    function DefaultWidth: Integer; virtual;
    function GetCaption: string; virtual;
    function DoLoad(o: TJSONObject): Boolean; virtual;
    procedure DoSave(o: TJSONObject); virtual;
    function DoRender(data: TMsgMonMessage): string; virtual; abstract;
    function DoCompare(d1, d2: TMsgMonMessage): Integer; virtual; abstract;
    function DoFilter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string): Boolean; virtual; abstract;
  public
    constructor Create(context: TMsgMonContext);
    function Load(o: TJSONObject): Boolean;
    procedure Save(o: TJSONObject);
    function Render(data: TMsgMonMessage): string;
    function Compare(d1, d2: TMsgMonMessage): Integer;
    function Filter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string; action: TMMFilterAction): Boolean;
    property Caption: string read GetCaption;
    property Width: Integer read FWidth write FWidth;
  end;

  TMMColumns = class(TObjectList<TMMColumn>)
  private
    FContext: TMsgMonContext;
  public
    constructor Create(context: TMsgMonContext);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadDefaultView;
    procedure LoadAll;
  end;

  TMMColumn_Integer = class(TMMColumn)
  protected
    function DefaultWidth: Integer; override;
    function DoRender(data: TMsgMonMessage): string; override;
    function DoCompare(d1, d2: TMsgMonMessage): Integer; override;
    function GetData(data: TMsgMonMessage): Integer; virtual; abstract;
    function DoFilter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_String = class(TMMColumn)
  protected
    function DoRender(data: TMsgMonMessage): string; override;
    function DoCompare(d1, d2: TMsgMonMessage): Integer; override;
    function GetData(data: TMsgMonMessage): string; virtual; abstract;
    function DoFilter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_Time = class(TMMColumn)
  protected
    function DefaultWidth: Integer; override;
    function DoRender(data: TMsgMonMessage): string; override;
    function DoCompare(d1, d2: TMsgMonMessage): Integer; override;
    function GetData(data: TMsgMonMessage): TDateTime; virtual; abstract;
    function DoFilter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_Window = class(TMMColumn)
  protected
    function DefaultWidth: Integer; override;
    function DoRender(data: TMsgMonMessage): string; override;
    function DoCompare(d1, d2: TMsgMonMessage): Integer; override;
    function GetData(data: TMsgMonMessage): Cardinal; virtual; abstract;
    function DoFilter(data: TMsgMonMessage; relation: TMMFilterRelation; const value: string): Boolean; override;
  public
  end;

  TMMColumn_Index = class(TMMColumn_Integer)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_ProcessPlatform = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_ProcessName = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_ProcessPath = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_CommandLine = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
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
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_TID = class(TMMColumn_Integer)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_hWnd = class(TMMColumn_Window)
  protected
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_Mode = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_MessageName = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_MessageID = class(TMMColumn_Integer)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_wParam = class(TMMColumn_Integer)
  protected
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_lParam = class(TMMColumn_Integer)
  protected
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_lResult = class(TMMColumn_Integer)
  protected
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): Integer; override;
  end;

  TMMColumn_Detail = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_MessageScope = class(TMMColumn_String)
  protected
    function DefaultWidth: Integer; override;
    function GetCaption: string; override;
    function GetData(data: TMsgMonMessage): string; override;
  end;

  TMMColumn_hWndFocus = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_hWndActive = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_hWndCapture = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_hWndCaret = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_hWndMenuOwner = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

  TMMColumn_hWndMoveSize = class(TMMColumn_Window)
  protected
    function GetData(data: TMsgMonMessage): Cardinal; override;
  end;

implementation

uses
  System.StrUtils;

{ TMMColumn }

function TMMColumn.Compare(d1, d2: TMsgMonMessage): Integer;
begin
  Result := DoCompare(d1, d2);
end;

constructor TMMColumn.Create(context: TMsgMonContext);
begin
  inherited Create;
  FContext := context;
  FWidth := DefaultWidth;
end;

function TMMColumn.DefaultWidth: Integer;
begin
  Result := 64;
end;

function TMMColumn.GetCaption: string;
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

function TMMColumn.Filter(data: TMsgMonMessage; relation: TMMFilterRelation;
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

function TMMColumn.Render(data: TMsgMonMessage): string;
begin
  Result := DoRender(data);
end;

procedure TMMColumn.Save(o: TJSONObject);
begin
  o.AddPair('width', TJSONNumber.Create(FWidth));
  DoSave(o);
end;

{ TMMColumn_Integer }

function TMMColumn_Integer.DefaultWidth: Integer;
begin
  Result := 92;
end;

function TMMColumn_Integer.DoCompare(d1, d2: TMsgMonMessage): Integer;
begin
  Result := GetData(d1) - GetData(d2);
end;

function TMMColumn_Integer.DoFilter(data: TMsgMonMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue, filterValue: Integer;
begin
  dataValue := GetData(data);
  if not TryStrToInt(value, filterValue) then
    Exit(False);

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

function TMMColumn_Integer.DoRender(data: TMsgMonMessage): string;
begin
  Result := IntToStr(GetData(data));
end;

{ TMMColumn_String }

function TMMColumn_String.DoCompare(d1, d2: TMsgMonMessage): Integer;
begin
  Result := CompareText(GetData(d1), GetData(d2));
end;

function TMMColumn_String.DoFilter(data: TMsgMonMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue: string;
begin
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

function TMMColumn_String.DoRender(data: TMsgMonMessage): string;
begin
  Result := GetData(data);
end;

{ TMMColumn_Index }

function TMMColumn_Index.DefaultWidth: Integer;
begin
  Result := 48;
end;

function TMMColumn_Index.GetCaption: string;
begin
  Result := '#';
end;

function TMMColumn_Index.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.Index;
end;

{ TMMColumn_ProcessName }

function TMMColumn_ProcessName.DefaultWidth: Integer;
begin
  Result := 128;
end;

function TMMColumn_ProcessName.GetCaption: string;
begin
  Result := 'Process Name';
end;

function TMMColumn_ProcessName.GetData(data: TMsgMonMessage): string;
begin
  if data.process = nil
    then Result := IntToStr(data.pid)
    else Result := data.process.processName;
end;

{ TMMColumn_ProcessPath }

function TMMColumn_ProcessPath.DefaultWidth: Integer;
begin
  Result := 200;
end;

function TMMColumn_ProcessPath.GetCaption: string;
begin
  Result := 'Process Path';
end;

function TMMColumn_ProcessPath.GetData(data: TMsgMonMessage): string;
begin
  if data.process = nil
    then Result := ''
    else Result := data.process.processPath;
end;

{ TMMColumn_CommandLine }

function TMMColumn_CommandLine.DefaultWidth: Integer;
begin
  Result := 200;
end;

function TMMColumn_CommandLine.GetCaption: string;
begin
  Result := 'Command Line';
end;

function TMMColumn_CommandLine.GetData(data: TMsgMonMessage): string;
begin
  if data.process = nil
    then Result := ''
    else Result := data.process.CommandLine;
end;

{ TMMColumn_PID }

function TMMColumn_PID.DefaultWidth: Integer;
begin
  Result := 48;
end;

function TMMColumn_PID.GetCaption: string;
begin
  Result := 'PID';
end;

function TMMColumn_PID.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.pid;
end;

{ TMMColumn_TID }

function TMMColumn_TID.DefaultWidth: Integer;
begin
  Result := 48;
end;

function TMMColumn_TID.GetCaption: string;
begin
  Result := 'TID';
end;

function TMMColumn_TID.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.tid;
end;

{ TMMColumn_MessageName }

function TMMColumn_MessageName.DefaultWidth: Integer;
begin
  Result := 160;
end;

function TMMColumn_MessageName.GetCaption: string;
begin
  Result := 'Message';
end;

function TMMColumn_MessageName.GetData(data: TMsgMonMessage): string;
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

function TMMColumn_MessageID.DefaultWidth: Integer;
begin
  Result := 64;
end;

function TMMColumn_MessageID.GetCaption: string;
begin
  Result := 'Message#';
end;

function TMMColumn_MessageID.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.message;
end;

{ TMMColumn_wParam }

function TMMColumn_wParam.GetCaption: string;
begin
  Result := 'wParam';
end;

function TMMColumn_wParam.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.wParam;
end;

{ TMMColumn_lParam }

function TMMColumn_lParam.GetCaption: string;
begin
  Result := 'lParam';
end;

function TMMColumn_lParam.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.lParam;
end;

{ TMMColumn_lResult }

function TMMColumn_lResult.GetCaption: string;
begin
  Result := 'lResult';
end;

function TMMColumn_lResult.GetData(data: TMsgMonMessage): Integer;
begin
  Result := data.lResult;
end;

{ TMMColumn_Detail }

function TMMColumn_Detail.DefaultWidth: Integer;
begin
  Result := -1;
end;

function TMMColumn_Detail.GetCaption: string;
begin
  Result := 'Detail';
end;

function TMMColumn_Detail.GetData(data: TMsgMonMessage): string;
begin
  Result := data.detail;
end;

{ TMMColumn_MessageClass }

function TMMColumn_MessageScope.DefaultWidth: Integer;
begin
  Result := 64;
end;

function TMMColumn_MessageScope.GetCaption: string;
begin
  Result := 'Scope';
end;

function TMMColumn_MessageScope.GetData(data: TMsgMonMessage): string;
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

function TMMColumn_Time.DefaultWidth: Integer;
begin
  Result := 128;
end;

function TMMColumn_Time.DoCompare(d1, d2: TMsgMonMessage): Integer;
begin
  if GetData(d1) < GetData(d2) then Result := 1
  else if GetData(d1) > GetData(d2) then Result := -1
  else Result := 0;
end;

function TMMColumn_Time.DoFilter(data: TMsgMonMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  dataValue, filterValue: TDateTime;
begin
  // TODO: This is garbage. Just want it to compile for now
  dataValue := GetData(data);
  if not TryStrToDateTime(value, filterValue) then
    Exit(False);

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

function TMMColumn_Time.DoRender(data: TMsgMonMessage): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', GetData(data));
end;

{ TMMColumns }

constructor TMMColumns.Create(context: TMsgMonContext);
begin
  inherited Create;
  FContext := context;
end;

procedure TMMColumns.LoadAll;
begin
  Clear;
  Add(TMMColumn_Index.Create(FContext));
  Add(TMMColumn_ProcessPlatform.Create(FContext));
  Add(TMMColumn_ProcessName.Create(FContext));
  Add(TMMColumn_PID.Create(FContext));
  Add(TMMColumn_TID.Create(FContext));
  Add(TMMColumn_Mode.Create(FContext));
  Add(TMMColumn_hWnd.Create(FContext));
  Add(TMMColumn_MessageName.Create(FContext));
  Add(TMMColumn_MessageScope.Create(FContext));
  Add(TMMColumn_wParam.Create(FContext));
  Add(TMMColumn_lParam.Create(FContext));
  Add(TMMColumn_lResult.Create(FContext));
  Add(TMMColumn_Detail.Create(FContext));
  Add(TMMColumn_hWndFocus.Create(FContext));
  Add(TMMColumn_hWndActive.Create(FContext));
  Add(TMMColumn_hWndCapture.Create(FContext));
  Add(TMMColumn_hWndCaret.Create(FContext));
  Add(TMMColumn_hWndMenuOwner.Create(FContext));
  Add(TMMColumn_hWndMoveSize.Create(FContext));
end;

procedure TMMColumns.LoadDefaultView;
begin
  Clear;
  Add(TMMColumn_Index.Create(FContext));
  Add(TMMColumn_ProcessPlatform.Create(FContext));
  Add(TMMColumn_ProcessName.Create(FContext));
  Add(TMMColumn_PID.Create(FContext));
  Add(TMMColumn_TID.Create(FContext));
  Add(TMMColumn_Mode.Create(FContext));
  Add(TMMColumn_hWnd.Create(FContext));
  Add(TMMColumn_MessageName.Create(FContext));
  Add(TMMColumn_MessageScope.Create(FContext));
  Add(TMMColumn_wParam.Create(FContext));
  Add(TMMColumn_lParam.Create(FContext));
  Add(TMMColumn_lResult.Create(FContext));
  Add(TMMColumn_Detail.Create(FContext));
end;

procedure TMMColumns.LoadFromFile(const Filename: string);
begin

end;

procedure TMMColumns.SaveToFile(const Filename: string);
begin

end;

{ TMMColumn_Mode }

function TMMColumn_Mode.DefaultWidth: Integer;
begin
  Result := 24;
end;

function TMMColumn_Mode.GetCaption: string;
begin
  Result := 'Mode';
end;

function TMMColumn_Mode.GetData(data: TMsgMonMessage): string;
begin
  case data.mode of
    1: Result := 'P';
    2: Result := 'S';
    3: Result := 'R';
    else Result := '';
  end;
end;

{ TMMColumn_ProcessPlatform }

function TMMColumn_ProcessPlatform.DefaultWidth: Integer;
begin
  Result := 32;
end;

function TMMColumn_ProcessPlatform.GetCaption: string;
begin
  Result := 'Platform';
end;

function TMMColumn_ProcessPlatform.GetData(data: TMsgMonMessage): string;
begin
  if Assigned(data.process) then
    case data.process.platform_ of
      1: Result := 'x86';
      2: Result := 'x64';
      else Result := '';
    end;
end;

{ TMMColumn_hWnd }

function TMMColumn_hWnd.GetCaption: string;
begin
  Result := 'hwnd';
end;

function TMMColumn_hWnd.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwnd;
end;

{ TMMColumn_Window }

function TMMColumn_Window.DefaultWidth: Integer;
begin
  Result := 128;
end;

function TMMColumn_Window.DoCompare(d1, d2: TMsgMonMessage): Integer;
begin
  Result := Integer(GetData(d1)) - Integer(GetData(d2));
end;

function TMMColumn_Window.DoFilter(data: TMsgMonMessage;
  relation: TMMFilterRelation; const value: string): Boolean;
var
  ws: TMsgMonWindows;
  w: TMsgMonWindow;
  dataValue: string;
  filterValueInt: Integer;
  dataValueInt: Cardinal;
begin
  dataValueInt := GetData(data);
  dataValue := DoRender(data); // TODO we could be nuanced here
  if TryStrToInt(value, filterValueInt) then
  begin
    case relation of
      frIs:         Result := filterValueInt = dataValueInt;
      frIsNot:      Result := filterValueInt <> dataValueInt;
      frLessThan:   Result := filterValueInt < dataValueInt;
      frMoreThan:   Result := filterValueInt > dataValueInt;
      frBeginsWith: Result := IntToStr(dataValueInt).StartsWith(value);
      frEndsWith:   Result := IntToStr(dataValueInt).EndsWith(value);
      frContains:   Result := IntToStr(dataValueInt).Contains(value);
      frExcludes:   Result := not IntToStr(dataValueInt).Contains(value);
    end;
  end
  else
  begin
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
end;

function TMMColumn_Window.DoRender(data: TMsgMonMessage): string;
var
  hwnd: Cardinal;
  ws: TMsgMonWindows;
  w: TMsgMonWindow;
begin
  hwnd := GetData(data);

  if FContext.Windows.TryGetValue(hwnd, ws)
    then w := ws.FromBase(data.index)
    else w := nil;

  if Assigned(w) then
  begin
    Result := w.ClassName;

    if w.RealClassName <> w.ClassName
      then Result := Result + ' ('+w.RealClassName+')';
  end
  else
    Result := IntToStr(hwnd);
end;

{ TMMColumn_hWndFocus }

function TMMColumn_hWndFocus.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndFocus;
end;

{ TMMColumn_hWndActive }

function TMMColumn_hWndActive.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndActive;
end;

{ TMMColumn_hWndCapture }

function TMMColumn_hWndCapture.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndCapture;
end;

{ TMMColumn_hWndCaret }

function TMMColumn_hWndCaret.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndCaret;
end;

{ TMMColumn_hWndMenuOwner }

function TMMColumn_hWndMenuOwner.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndMenuOwner;
end;

{ TMMColumn_hWndMoveSize }

function TMMColumn_hWndMoveSize.GetData(data: TMsgMonMessage): Cardinal;
begin
  Result := data.hwndMoveSize;
end;

end.
