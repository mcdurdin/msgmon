unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMMWindow = class
    base: Integer;  // First reference in messages index
    hwnd: DWORD;
    pid, tid: DWORD;
    hwndOwner, hwndParent: DWORD;
    ClassName, RealClassName: string;
    constructor Create(AEventData: IXMLDOMNode; ABase: Integer);
    function Render(IncludeHandle: Boolean): string;
  end;

  TMMWindows = class(TObjectList<TMMWindow>)
    function FromBase(ABase: Integer): TMMWindow;
  end;

  TMMWindowDictionary = class(TObjectDictionary<DWORD,TMMWindows>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonWindow }

constructor TMMWindow.Create(AEventData: IXMLDOMNode; ABase: Integer);
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
      else if name = 'pid' then pid := valueInt
      else if name = 'tid' then tid := valueInt
      else if name = 'hwndOwner' then Self.hwndOwner := valueInt
      else if name = 'hwndParent' then Self.hwndParent := valueInt
      else if name = 'className' then Self.ClassName := value
      else if name = 'realClassName' then Self.RealClassName := value;
    end;

    AEventData := AEventData.NextSibling;
  end;
end;

{ TMsgMonWindows }

function TMMWindows.FromBase(ABase: Integer): TMMWindow;
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

function TMMWindow.Render(IncludeHandle: Boolean): string;
begin
  Result := ClassName;

  if IncludeHandle then
    Result := Result + ' ['+IntToStr(hwnd)+']'
  else if RealClassName <> ClassName
    then Result := Result + ' ('+RealClassName+')';
end;

end.
