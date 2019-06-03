unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMsgMonWindow = class
    base: Integer;  // First reference in messages index
    hwnd: DWORD;
    pid, tid: DWORD;
    hwndOwner, hwndParent: DWORD;
    ClassName, RealClassName: string;
    constructor Create(AEventData: IXMLDOMNode; ABase: Integer);
  end;

  TMsgMonWindows = class(TObjectList<TMsgMonWindow>)
    function FromBase(ABase: Integer): TMsgMonWindow;
  end;

  TMsgMonWindowDictionary = class(TObjectDictionary<DWORD,TMsgMonWindows>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonWindow }

constructor TMsgMonWindow.Create(AEventData: IXMLDOMNode; ABase: Integer);
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

{ TMsgMonWindows }

function TMsgMonWindows.FromBase(ABase: Integer): TMsgMonWindow;
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
