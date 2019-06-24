unit MsgMon.System.Data.Context;

interface

uses
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  TMMDataContext = class
  private
    FProcesses: TMMProcessDictionary;
    FMessageNames: TMMMessageNameDictionary;
    FWindows: TMMWindowDictionary;
//    FMessages: TMMMessages;
//    FFilteredMessages: TMMMessages;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MessageNames: TMMMessageNameDictionary read FMessageNames;
    property Processes: TMMProcessDictionary read FProcesses;
    property Windows: TMMWindowDictionary read FWindows;
//    property Messages: TMMMessages read FMessages;
//    property FilteredMessages: TMMMessages read FFilteredMessages;
  end;

implementation

{ TMsgMonContext }

procedure TMMDataContext.Clear;
begin
//  FFilteredMessages.Clear;
//  FMessages.Clear;
  FMessageNames.Clear;
  FMessageNames.FillDefault;
  FProcesses.Clear;
  FWindows.Clear;
end;

constructor TMMDataContext.Create;
begin
  inherited Create;
//  FMessages := TMMMessages.Create;
//  FFilteredMessages := TMMMessages.Create(False);
  FMessageNames := TMMMessageNameDictionary.Create;
  FProcesses := TMMProcessDictionary.Create;
  FWindows := TMMWindowDictionary.Create;
end;

destructor TMMDataContext.Destroy;
begin
  FWindows.Free;
  FProcesses.Free;
  FMessageNames.Free;
//  FFilteredMessages.Free;
//  FMessages.Free;
  inherited Destroy;
end;

end.
