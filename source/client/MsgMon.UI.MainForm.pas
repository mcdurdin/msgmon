unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Xml.XmlIntf,
  Xml.XmlDoc,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, Vcl.ToolWin,
  Vcl.ComCtrls, JwaEventTracing, JwaEvntCons, JwaEventDefs, JwaWmistr, System.Generics.Collections;

type
  TMsgMonMessage = class
    platform_: DWORD;
    processPath: string;
    pid, tid: DWORD;
    tickCount: DWORD;
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

    className, realClassName: string;
    mode: DWORD;
    detail: string;
    stack: string;
    constructor Create(AEventData, AStackData: IXMLNode);
  end;

  TForm1 = class(TForm)
    CoolBar1: TCoolBar;
    cmdStartStopTrace: TButton;
    cmdClear: TButton;
    lvMessages: TListView;
    statusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdStartStopTraceClick(Sender: TObject);
    procedure lvMessagesData(Sender: TObject; Item: TListItem);
  private
    messages: TObjectList<TMsgMonMessage>;

    x64Thread: Cardinal;

    // Trace controller
    FTracing: Boolean;
    FSessionHandle: TRACEHANDLE;
    pSessionProperties: PEVENT_TRACE_PROPERTIES;
    procedure Controller_StartTrace;
    procedure Controller_StopTrace;
    procedure EnableDisableTrace;
    procedure LoadData;
    procedure FlushLibrary;

    // Trace consumer
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Winapi.ActiveX,
  Winapi.Tlhelp32,
  MsgMon.System.ExecProcess;

const
  LibraryName = 'msgmon.capture.x86.dll';

function BeginLog: BOOL; stdcall; external LibraryName;
function EndLog: BOOL; stdcall; external LibraryName;

{$DEFINE RUNX64}

type
  TEVENT_FILTER_DESCRIPTOR = record
    Ptr: ULONGLONG;
    Size: ULONG;
    Type_: ULONG;
  end;

  PEVENT_FILTER_DESCRIPTOR = ^TEVENT_FILTER_DESCRIPTOR;

  TENABLE_TRACE_PARAMETERS = record
    Version: ULONG;
    EnableProperty: ULONG;
    ControlFlags: ULONG;
    SourceId: TGUID;
    EnableFilterDesc: PEVENT_FILTER_DESCRIPTOR;
    FilterDescCount: ULONG;
  end;

  PENABLE_TRACE_PARAMETERS = ^TENABLE_TRACE_PARAMETERS;

function EnableTraceEx2(
    {__in} TraceHandle : TRACEHANDLE;
    {__in} ProviderId : PGUID;
    {__in} ControlCode : ULONG;
    {__in} Level : UCHAR;
    {__in} MatchAnyKeyword : ULONGLONG;
    {__in} MatchAllKeyword : ULONGLONG;
    {__in} Timeout : ULONG;
    {__in_opt} EnableParameters: PENABLE_TRACE_PARAMETERS) : ULONG; stdcall; external advapi32 name 'EnableTraceEx2';

const
  ENABLE_TRACE_PARAMETERS_VERSION_2 = 2;
  EVENT_ENABLE_PROPERTY_STACK_TRACE = 4;
  EVENT_CONTROL_CODE_ENABLE_PROVIDER = 1;
  EVENT_CONTROL_CODE_DISABLE_PROVIDER = 0;

  MsgMonProviderGuid: TGUID = '{082E6CC6-239C-4B96-9475-159AA241B4AB}';

procedure TForm1.cmdStartStopTraceClick(Sender: TObject);
begin
  FTracing := not FTracing;
  EnableDisableTrace;
end;

procedure TForm1.EnableDisableTrace;
var
  params: TENABLE_TRACE_PARAMETERS;
  status: ULONG;
begin
  if FTracing then
  begin
    Controller_StartTrace;

    cmdStartStopTrace.Caption := '&Stop Tracing';
    FillChar(params, Sizeof(params), 0);
    params.Version := ENABLE_TRACE_PARAMETERS_VERSION_2;
    params.EnableProperty := EVENT_ENABLE_PROPERTY_STACK_TRACE;

    status := EnableTraceEx2(
      FSessionHandle,
      @MsgMonProviderGuid,
      EVENT_CONTROL_CODE_ENABLE_PROVIDER,
      TRACE_LEVEL_INFORMATION,
      0,
      0,
      0,
      @params);

    if ERROR_SUCCESS <> status then
      RaiseLastOSError(status, 'EnableTraceEx2');
  end
  else
  begin
    cmdStartStopTrace.Caption := '&Start Tracing';

    Controller_StopTrace;

    LoadData;

{    status := EnableTraceEx2(
      FSessionHandle,
      @MsgMonProviderGuid,
      EVENT_CONTROL_CODE_DISABLE_PROVIDER,
      TRACE_LEVEL_INFORMATION,
      0,
      0,
      0,
      nil);

    if ERROR_SUCCESS <> status then
      RaiseLastOSError(status, 'EnableTraceEx2');
 }
//    StopTrace(FSessionHandle, PWideChar(LOGSESSION_NAME),
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
{$IFDEF RUNX64}
var
  app: string;
  si: TStartupInfo;
  pi: TProcessInformation;
{$ENDIF}
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  messages := TObjectList<TMsgMonMessage>.Create;

{$IFDEF RunX64}
  app := 'msgmon.x64host.exe';

  FillChar(si, SizeOf(TStartupInfo), 0);
  FillChar(pi, Sizeof(TProcessInformation), 0);

  si.cb := SizeOf(TStartupInfo);
  if not CreateProcess(PChar(app), nil, nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
  begin
    ShowMessage('Unable to start x64 process');
    Exit;
  end;

  CloseHandle(pi.hProcess);
  x64Thread := pi.hThread;
{$ENDIF}

  BeginLog;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  h: THandle;
begin
  EndLog;

{$IFDEF RunX64}
  h := FindWindow('Tmsgmonx64Host', nil);
  PostMessage(h, WM_USER+100, 0, 0);
  WaitForSingleObject(x64Thread, INFINITE);
  CloseHandle(x64Thread);
{$ENDIF}

  FlushLibrary;

  CoUninitialize;
end;

procedure TForm1.lvMessagesData(Sender: TObject; Item: TListItem);
var
  m: TMsgMonMessage;
begin
  m := messages[Item.Index];
  Item.Caption := IntToStr(m.pid);
  Item.SubItems.Add(IntToStr(m.tid));
  Item.SubItems.Add(IntToStr(m.hwnd));
  Item.SubItems.Add(IntToStr(m.message));
  Item.SubItems.Add(IntToStr(m.wParam));
  Item.SubItems.Add(IntToStr(m.lParam));
  Item.SubItems.Add(IntToStr(m.lResult));
end;

function StringBufferSize(const s: string): Integer;
begin
  Result := (Length(s) + 1) * sizeof(WCHAR);
end;

const
  LOGSESSION_NAME = 'MsgMon_Session';
  LOGSESSION_1_FILENAME = 'c:\temp\msgmon1.etl';
  LOGSESSION_FILENAME = 'c:\temp\msgmon.etl';
  LOGSESSION_XML_FILENAME = 'c:\temp\msgmon.xml';

procedure TForm1.Controller_StartTrace;
var
  BufferSize: ULONG;
  status: ULONG;
begin
  // Allocate memory for the session properties. The memory must
  // be large enough to include the log file name and session name,
  // which get appended to the end of the session properties structure.

  BufferSize := sizeof(EVENT_TRACE_PROPERTIES) + StringBufferSize(LOGSESSION_FILENAME) + StringBufferSize(LOGSESSION_NAME);
  pSessionProperties := PEVENT_TRACE_PROPERTIES(AllocMem(BufferSize));

  // Set the session properties. You only append the log file name
  // to the properties structure; the StartTrace function appends
  // the session name for you.

  pSessionProperties.Wnode.BufferSize := BufferSize;
  pSessionProperties.Wnode.Flags := WNODE_FLAG_TRACED_GUID;
  pSessionProperties.Wnode.ClientContext := 1; //QPC clock resolution
  pSessionProperties.Wnode.Guid := MsgMonProviderGuid;
  pSessionProperties.LogFileMode := EVENT_TRACE_FILE_MODE_SEQUENTIAL;
  pSessionProperties.MaximumFileSize := 256;  // 256 MB
  pSessionProperties.LoggerNameOffset := sizeof(EVENT_TRACE_PROPERTIES);
  pSessionProperties.LogFileNameOffset := sizeof(EVENT_TRACE_PROPERTIES) + sizeof(LOGSESSION_NAME);
  StrPCopy(PWideChar(PByte(pSessionProperties) + pSessionProperties.LogFileNameOffset), LOGSESSION_FILENAME);

  status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  if ERROR_ALREADY_EXISTS = status then
  begin
    // The trace was already started, perhaps Keyman did not close down cleanly
    // We'll stop it and restart it
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)));

    status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  end;

  if ERROR_SUCCESS <> status then
    RaiseLastOSError(status, 'StartTrace');

  {if not TExecProcess.WaitForProcess(
      'xperf_run.bat -on LOADER+PROC_THREAD -start "'+LOGSESSION_NAME+'" -on MsgMon:::''stack'' -f "'+LOGSESSION_1_FILENAME+'"',
      GetCurrentDir) then
    RaiseLastOSError;}
end;

procedure TForm1.Controller_StopTrace;
var
  status: ULONG;
begin
  if FSessionHandle <> 0 then
  begin
    if FTracing then
    begin
      FTracing := False;
      EnableDisableTrace;
    end;

    // We use ControlTraceW because JwaEventTracing has a typo for ControlTrace
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)+' '+SysErrorMessage(status)));

    FSessionHandle := 0;
  end;

  if pSessionProperties <> nil then
  begin
    FreeMem(pSessionProperties);
    pSessionProperties := nil;
  end;

  {if not TExecProcess.WaitForProcess(
      'xperf.exe -stop "'+LOGSESSION_NAME+'" -stop -d "'+LOGSESSION_FILENAME+'"',
      GetCurrentDir) then
    RaiseLastOSError;}
end;

procedure TForm1.LoadData;
var
  doc: IXMLDocument;
  events: IXMLNodeList;
  m: TMsgMonMessage;
  stack, event, eventData, system, provider: IXMLNode;
begin
  if FileExists(LOGSESSION_XML_FILENAME) then
    DeleteFile(LOGSESSION_XML_FILENAME);

  // Use tracerpt to generate an xml file which we load. This could be rewritten into
  // a direct load with ProcessTrace in the future, but this saves a lot of dev time!
  if not TExecProcess.WaitForProcess('tracerpt "'+LOGSESSION_FILENAME+'" -o "'+LOGSESSION_XML_FILENAME+'" -of XML', GetCurrentDir) then
    RaiseLastOSError;

  // Load the XML data
  doc := LoadXMLDocument(LOGSESSION_XML_FILENAME);
  events := doc.DocumentElement.ChildNodes;
  event := events.First;
  messages.Clear;
  while event <> nil do
  begin
    try
      system := event.ChildNodes.FindNode('System');
      if not Assigned(system) then Continue;
      provider := system.ChildNodes.FindNode('Provider');
      if not Assigned(provider) then Continue;
      if not provider.HasAttribute('Name') then Continue;
      if provider.Attributes['Name'] <> 'MsgMon' then Continue;

      eventData := event.ChildNodes.FindNode('EventData');
      if not Assigned(eventData) then Continue;

      stack := system.ChildNodes.FindNode('Stack');
      m := TMsgMonMessage.Create(eventData, stack);
      messages.Add(m);
    finally
      event := event.NextSibling;
    end;
  end;

  lvMessages.Items.Count := messages.Count;
  lvMessages.Invalidate;
end;

{ TMsgMonMessage }

constructor TMsgMonMessage.Create(AEventData, AStackData: IXMLNode);
var
  name, value: string;
  valueInt: Int64;
begin
  inherited Create;

  if not Assigned(AEventData.ChildNodes) then
    Exit;

  AEventData := AEventData.ChildNodes.First;
  while Assigned(AEventData) do
  begin
    if AEventData.HasAttribute('Name') then
    begin
      name := AEventData.Attributes['Name'];
      value := Trim(VarToStr(AEventData.NodeValue));
      valueInt := StrToIntDef(value, 0);
      if name = 'Platform' then platform_ := valueInt
      else if name = 'Process' then processPath := value
      else if name = 'PID' then pid := valueInt
      else if name = 'TID' then tid := valueInt
      else if name = 'dwTickCount' then tickCount := valueInt
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
      else if name = 'ClassName' then Self.ClassName := value
      else if name = 'RealClassName' then Self.RealClassName := value
      else if name = 'Mode' then Self.Mode := valueInt
      else if name = 'Detail' then Self.Detail := value;
    end;

    AEventData := AEventData.NextSibling;
  end;

  if Assigned(AStackData) then
    stack := AStackData.XML;
end;

procedure TForm1.FlushLibrary;
var
  h: THandle;
  te: TThreadEntry32;
begin
  // A better way of doing may be to make each loaded process have a thread
  // waiting on this process handle. Then after the process exits, the thread
  // does a PostThreadMessage and then FreeLibraryAndExitThread
  // e.g. https://stackoverflow.com/a/25597741/1836776

  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    FillChar(te, sizeof(te), 0);
    te.dwSize := sizeof(te);
    if Thread32First(h, te) then
    begin
      repeat
        if te.dwSize >= 12 then // see https://devblogs.microsoft.com/oldnewthing/20060223-14/?p=32173
          PostThreadMessage(te.th32ThreadID, WM_NULL, 0, 0);
      until not Thread32Next(h, te);
    end;
    CloseHandle(h);
  end;
end;

end.
