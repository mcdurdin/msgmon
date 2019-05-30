unit MsgMon.System.Data.Message;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml;

type
  TMsgMonMessage = class
  strict private
    FEventData, FStackData: IXMLDOMNode;
  public
    pid, tid: DWORD;
    hwndFocus,
    hwndActive,
    hwndCapture,
    hwndCaret,
    hwndMenuOwner,
    hwndMoveSize: DWORD;

    activeHKL: DWORD;

    hwnd,
    message: DWORD;
    wParam, lParam, lResult: UINT64;

    mode: DWORD;
    detail: string;
    stack: string;
    procedure Fill;
    constructor Create(AEventData, AStackData: IXMLDOMNode);
  end;

type
  TMsgMonMessages = class(TObjectList<TMsgMonMessage>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonMessage }

constructor TMsgMonMessage.Create(AEventData, AStackData: IXMLDOMNode);
begin
  inherited Create;
  FEventData := AEventData;
  FStackData := AStackData;
  Assert(FEventData <> nil);
end;

procedure TMsgMonMessage.Fill;
var
  name, value: string;
  valueInt: Int64;
  nameAttr: IXMLDOMNode;
begin
  if not Assigned(FEventData) then
    // Already populated
    Exit;

  if not Assigned(FEventData.ChildNodes) then
    Exit;

  FEventData := FEventData.ChildNodes.nextNode;
  while Assigned(FEventData) do
  begin
    nameAttr := FEventData.attributes.getNamedItem('Name');
    if Assigned(nameAttr) then
    begin
      name := VarToStr(nameAttr.nodeValue);
      value := Trim(VarToStr(FEventData.NodeValue));
      valueInt := StrToIntDef(value, 0);
      if name = 'PID' then pid := valueInt
      else if name = 'TID' then tid := valueInt
      else if name = 'hwndFocus' then Self.hwndFocus := valueInt
      else if name = 'hwndActive' then Self.hwndActive := valueInt
      else if name = 'hwndCapture' then Self.hwndCapture := valueInt
      else if name = 'hwndCaret' then Self.hwndCaret := valueInt
      else if name = 'hwndMenuOwner' then Self.hwndMenuOwner := valueInt
      else if name = 'hwndMoveSize' then Self.hwndMoveSize := valueInt
      else if name = 'hklActive' then Self.activeHKL := valueInt
      else if name = 'hwnd' then Self.hwnd := valueInt
      else if name = 'message' then Self.message := valueInt
      else if name = 'wParam' then Self.wParam := valueInt
      else if name = 'lParam' then Self.lParam := valueInt
      else if name = 'lResult' then Self.lResult := valueInt
      else if name = 'Mode' then Self.Mode := valueInt
      else if name = 'Detail' then Self.Detail := value;
    end;

    FEventData := FEventData.NextSibling;
  end;

  FEventData := nil;

  if Assigned(FStackData) then
    stack := FStackData.XML;
end;

end.
