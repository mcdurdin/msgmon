unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMsgMonWindow = class
    hwnd: DWORD;
    pid, tid: DWORD;
    hwndOwner, hwndParent: DWORD;
    ClassName, RealClassName: string;
    constructor Create(AEventData: IXMLDOMNode);
  end;

  TMsgMonWindows = class(TObjectDictionary<DWORD,TMsgMonWindow>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonWindow }

constructor TMsgMonWindow.Create(AEventData: IXMLDOMNode);
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
      if name = 'hwnd' then Self.hwnd := valueInt
      else if name = 'PID' then pid := valueInt
      else if name = 'TID' then tid := valueInt
      else if name = 'hwndOwner' then Self.hwndOwner := valueInt
      else if name = 'hwndParent' then Self.hwndParent := valueInt
      else if name = 'ClassName' then Self.ClassName := value
      else if name = 'RealClassName' then Self.RealClassName := value;
    end;

    AEventData := AEventData.NextSibling;
  end;
end;

end.
