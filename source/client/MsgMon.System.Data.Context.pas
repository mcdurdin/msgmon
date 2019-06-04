unit MsgMon.System.Data.Context;

interface

uses
  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  TMsgMonContext = class
  private
    FProcesses: TMsgMonProcessDictionary;
    FMessageNames: TMsgMonMessageNameDictionary;
    FWindows: TMsgMonWindowDictionary;
    FMessages: TMsgMonMessages;
    FFilteredMessages: TMsgMonMessages;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MessageNames: TMsgMonMessageNameDictionary read FMessageNames;
    property Processes: TMsgMonProcessDictionary read FProcesses;
    property Windows: TMsgMonWindowDictionary read FWindows;
    property Messages: TMsgMonMessages read FMessages;
    property FilteredMessages: TMsgMonMessages read FFilteredMessages;
  end;

implementation

{ TMsgMonContext }

procedure TMsgMonContext.Clear;
begin
  FFilteredMessages.Clear;
  FMessages.Clear;
  FMessageNames.Clear;
  FMessageNames.FillDefault;
  FProcesses.Clear;
  FWindows.Clear;
end;

constructor TMsgMonContext.Create;
begin
  inherited Create;
  FMessages := TMsgMonMessages.Create;
  FFilteredMessages := TMsgMonMessages.Create(False);
  FMessageNames := TMsgMonMessageNameDictionary.Create;
  FProcesses := TMsgMonProcessDictionary.Create;
  FWindows := TMsgMonWindowDictionary.Create;
end;

destructor TMsgMonContext.Destroy;
begin
  FWindows.Free;
  FProcesses.Free;
  FMessageNames.Free;
  FFilteredMessages.Free;
  FMessages.Free;
  inherited Destroy;
end;

end.
