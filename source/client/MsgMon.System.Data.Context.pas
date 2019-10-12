unit MsgMon.System.Data.Context;

interface

uses
  System.Generics.Collections,

  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TMMDataContext = class
  private
    FMessageNames: TMMMessageNameDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MessageNames: TMMMessageNameDictionary read FMessageNames;
  end;

implementation

{ TMsgMonContext }

procedure TMMDataContext.Clear;
begin
  FMessageNames.Clear;
  FMessageNames.FillDefault;
end;

constructor TMMDataContext.Create;
begin
  inherited Create;
  FMessageNames := TMMMessageNameDictionary.Create;
end;

destructor TMMDataContext.Destroy;
begin
  FMessageNames.Free;
  inherited Destroy;
end;

end.
