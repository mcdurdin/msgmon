unit MsgMon.System.Data.Process;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMsgMonProcess = class
    pid: DWORD;
    platform_: DWORD;
    processPath, commandLine: string;
    constructor Create(AEventData: IXMLDOMNode);
  end;

  TMsgMonProcesses = class(TObjectDictionary<DWORD,TMsgMonProcess>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonProcess }

constructor TMsgMonProcess.Create(AEventData: IXMLDOMNode);
var
  name, value: string;
  valueInt: Int64;
  nameAttr: IXMLDOMNode;
begin
  inherited Create;

  if not Assigned(AEventData.ChildNodes) then
    Exit;

  AEventData := AEventData.ChildNodes.nextNode;
  while Assigned(AEventData) do
  begin
    nameAttr := AEventData.attributes.getNamedItem('Name');
    if Assigned(nameAttr) then
    begin
      name := VarToStr(nameAttr.nodeValue);
      value := Trim(VarToStr(AEventData.NodeValue));
      valueInt := StrToIntDef(value, 0);
      if name = 'PID' then pid := valueInt
      else if name = 'Platform' then platform_ := valueInt
      else if name = 'Process' then Self.processPath := value
      else if name = 'CommandLine' then Self.commandLine := value;
    end;

    AEventData := AEventData.NextSibling;
  end;
end;

end.

