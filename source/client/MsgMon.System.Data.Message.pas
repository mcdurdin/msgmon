unit MsgMon.System.Data.Message;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.msxml,

  MsgMon.System.Data.Context,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Window;

type
  TMsgMonMessage = class
  strict private
    FEventData, FStackData: IXMLDOMNode;
  public
    index: Integer;

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

    process: TMsgMonProcess;
    window: TMsgMonWindow;
    messageName: TMsgMonMessageName;

    procedure Fill(context: TMsgMonContext);
    constructor Create(AIndex: Integer; AEventData, AStackData: IXMLDOMNode);
  end;

type
  TMsgMonMessages = class(TObjectList<TMsgMonMessage>)
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TMsgMonMessage }

constructor TMsgMonMessage.Create(AIndex: Integer; AEventData, AStackData: IXMLDOMNode);
begin
  inherited Create;
  index := AIndex;
  FEventData := AEventData;
  FStackData := AStackData;
  Assert(FEventData <> nil);
end;

procedure TMsgMonMessage.Fill(context: TMsgMonContext);
var
  name, value: string;
  valueInt: Int64;
  nameAttr: IXMLDOMNode;
  ps: TMsgMonProcesses;
  ws: TMsgMonWindows;
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
      name := VarToStr(nameAttr.text);
      value := Trim(VarToStr(FEventData.text));
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
      // TODO: extradetail
      // TODO: messagetime etc
    end;

    FEventData := FEventData.NextSibling;
  end;

  FEventData := nil;

  if Assigned(FStackData) then
    stack := FStackData.XML;

  // Lookup data from context

  if context.Processes.TryGetValue(pid, ps)
    then process := ps.FromBase(index)
    else process := nil;

  if context.Windows.TryGetValue(hwnd, ws)
    then window := ws.FromBase(index)
    else window := nil;

  if not context.MessageNames.TryGetValue(message, messageName)
    then messageName := nil;
end;

end.
