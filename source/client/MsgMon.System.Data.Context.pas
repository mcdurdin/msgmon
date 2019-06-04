unit MsgMon.System.Data.Context;

interface

uses
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  TMsgMonContext = class
  private
    FProcesses: TMsgMonProcessDictionary;
    FMessageNames: TMsgMonMessageNameDictionary;
    FWindows: TMsgMonWindowDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    property MessageNames: TMsgMonMessageNameDictionary read FMessageNames;
    property Processes: TMsgMonProcessDictionary read FProcesses;
    property Windows: TMsgMonWindowDictionary read FWindows;
  end;

implementation

{ TMsgMonContext }

constructor TMsgMonContext.Create;
begin
  inherited Create;
  FMessageNames := TMsgMonMessageNameDictionary.Create;
  FProcesses := TMsgMonProcessDictionary.Create;
  FWindows := TMsgMonWindowDictionary.Create;
end;

destructor TMsgMonContext.Destroy;
begin
  FWindows.Free;
  FProcesses.Free;
  FMessageNames.Free;
  inherited Destroy;
end;

end.
