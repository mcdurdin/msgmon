unit MsgMon.System.Data.Process;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMMProcess = class
    base: Integer;
    pid: DWORD;
    platform_: DWORD;
    processPath, commandLine: string;
    processName: string;
    constructor Create(AEventData: IXMLDOMNode; ABase: Integer);
  end;

  TMMProcesses = class(TObjectList<TMMProcess>)
    function FromBase(ABase: Integer): TMMProcess;
  end;

  TMMProcessDictionary = class(TObjectDictionary<DWORD,TMMProcesses>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonProcess }

constructor TMMProcess.Create(AEventData: IXMLDOMNode; ABase: Integer);
var
  name, value: string;
  valueInt: Int64;
  nameAttr: IXMLDOMNode;
begin
  inherited Create;

  base := ABase;

  if not Assigned(AEventData.ChildNodes) then
    Exit;

  AEventData := AEventData.ChildNodes.nextNode;
  while Assigned(AEventData) do
  begin
    nameAttr := AEventData.attributes.getNamedItem('Name');
    if Assigned(nameAttr) then
    begin
      name := VarToStr(nameAttr.text);
      value := Trim(VarToStr(AEventData.text));
      valueInt := StrToIntDef(value, 0);
      if name = 'PID' then pid := valueInt
      else if name = 'Platform' then platform_ := valueInt
      else if name = 'Process' then Self.processPath := value
      else if name = 'CommandLine' then Self.commandLine := value;
    end;

    AEventData := AEventData.NextSibling;
  end;

  processName := ExtractFileName(processPath);
end;

{ TMsgMonProcesses }

function TMMProcesses.FromBase(ABase: Integer): TMMProcess;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].base <= ABase then
      Exit(Items[i]);
  if Count = 0 then
    Exit(nil);
  Result := Items[0];
end;

end.

