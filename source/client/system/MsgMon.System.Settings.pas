unit MsgMon.System.Settings;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Win.Registry;

type
  TMMSettings = class
  private
    FRegistry: TRegistry;
    FSymbolPath: string;
    FEnableStackTraces: Boolean;
    FDbghelpPath: string;
    FDisplayStackTraces: Boolean;
    FNotifiers: array of TNotifyEvent;
    procedure SetDbghelpPath(const Value: string);
    procedure SetDisplayStackTraces(const Value: Boolean);
    procedure SetEnableStackTraces(const Value: Boolean);
    procedure SetSymbolPath(const Value: string);
    procedure NotifyChange;
  private
    class var FInstance: TMMSettings;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TMMSettings;
    procedure Notification(e: TNotifyEvent);
    procedure RemoveNotification(e: TNotifyEvent);
//    property WindowLocation: TRect;
//    property DisplayFont: TFont;
    property EnableStackTraces: Boolean read FEnableStackTraces write SetEnableStackTraces;
    property DisplayStackTraces: Boolean read FDisplayStackTraces write SetDisplayStackTraces;
    property SymbolPath: string read FSymbolPath write SetSymbolPath;
    property DbghelpPath: string read FDbghelpPath write SetDbghelpPath;
  end;

implementation

const
  SRegKey = 'Software\MsgMon';
  SRegValue_EnableStackTraces = 'Enable Stack Traces';
  SRegValue_DisplayStackTraces = 'Display Stack Traces';
  SRegValue_SymbolPath = 'Symbol Path';
  SRegValue_DbghelpPath = 'Dbghelp Path';

{ TMMSettings }

constructor TMMSettings.Create;
begin
  inherited Create;
  FRegistry := TRegistry.Create;
  if FRegistry.OpenKey(SRegKey, True) then
  begin
    FDisplayStackTraces := FRegistry.ValueExists(SRegValue_DisplayStackTraces) and FRegistry.ReadBool(SRegValue_DisplayStackTraces);
    FEnableStackTraces := FRegistry.ValueExists(SRegValue_EnableStackTraces) and FRegistry.ReadBool(SRegValue_EnableStackTraces);
    if FRegistry.ValueExists(SRegValue_SymbolPath) then FSymbolPath := FRegistry.ReadString(SRegValue_SymbolPath);
    if FRegistry.ValueExists(SRegValue_DbghelpPath) then FDbghelpPath := FRegistry.ReadString(SRegValue_DbghelpPath);
  end
  else
    FreeAndNil(FRegistry);
end;

destructor TMMSettings.Destroy;
begin
  FreeAndNil(FRegistry);
  inherited Destroy;
end;

class function TMMSettings.Instance: TMMSettings;
begin
  if not Assigned(FInstance) then
    FInstance := TMMSettings.Create;
  Result := FInstance;
end;

procedure TMMSettings.Notification(e: TNotifyEvent);
begin
  RemoveNotification(e);
  SetLength(FNotifiers, Length(FNotifiers)+1);
  FNotifiers[High(FNotifiers)] := e;
end;

procedure TMMSettings.NotifyChange;
var
  i: Integer;
begin
  for i := 0 to High(FNotifiers) do
    FNotifiers[i](Self);
end;

function SameMethod(AMethod1, AMethod2: TMethod): boolean;
begin
  result := (AMethod1.Code = AMethod2.Code)
            and (AMethod1.Data = AMethod2.Data);
end;

procedure TMMSettings.RemoveNotification(e: TNotifyEvent);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to High(FNotifiers) do
    if SameMethod(TMethod(FNotifiers[i]), TMethod(e)) then
    begin
      for j := i+1 to High(FNotifiers) do
        FNotifiers[j-1] := FNotifiers[j];
      SetLength(FNotifiers, High(FNotifiers));
      Exit;
    end;
end;

procedure TMMSettings.SetDbghelpPath(const Value: string);
begin
  FDbghelpPath := Value;
  if Assigned(FRegistry) then
    FRegistry.WriteString(SRegValue_DbghelpPath, FDbghelpPath);
  NotifyChange;
end;

procedure TMMSettings.SetDisplayStackTraces(const Value: Boolean);
begin
  FDisplayStackTraces := Value;
  if Assigned(FRegistry) then
    FRegistry.WriteBool(SRegValue_DisplayStackTraces, FDisplayStackTraces);
  NotifyChange;
end;

procedure TMMSettings.SetEnableStackTraces(const Value: Boolean);
begin
  FEnableStackTraces := Value;
  if Assigned(FRegistry) then
    FRegistry.WriteBool(SRegValue_EnableStackTraces, FEnableStackTraces);
  NotifyChange;
end;

procedure TMMSettings.SetSymbolPath(const Value: string);
begin
  FSymbolPath := Value;
  if Assigned(FRegistry) then
    FRegistry.WriteString(SRegValue_SymbolPath, FSymbolPath);
  NotifyChange;
end;

initialization
finalization
  FreeAndNil(TMMSettings.FInstance);
end.
