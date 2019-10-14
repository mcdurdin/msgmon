unit MsgMon.System.Data.Context;

interface

uses
  System.Generics.Collections,

  MsgMon.System.Data.Message,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window,
  MsgMon.System.Data.MessageName;

type
  TMMGlobalContext = class
  private
    FMessageNames: TMMMessageNameDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MessageNames: TMMMessageNameDictionary read FMessageNames;
  end;

implementation

uses
  MsgMon.Data.Database,
  System.SysUtils;

{ TMsgMonContext }

procedure TMMGlobalContext.Clear;
begin
  FMessageNames.Clear;
  FMessageNames.FillDefault;
end;

constructor TMMGlobalContext.Create;
begin
  inherited Create;
  FMessageNames := TMMMessageNameDictionary.Create;
end;

destructor TMMGlobalContext.Destroy;
begin
  FMessageNames.Free;
  inherited Destroy;
end;


end.
