unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
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

    // Trace consumer
    procedure Consume_ProcessTrace;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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

    status := EnableTraceEx2(
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
  messages := TObjectList<TMsgMonMessage>.Create;

(*
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
  Controller_StartTrace;

  BeginLog;
*)

  Consume_ProcessTrace;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  h: THandle;
begin
(*
  EndLog;

{$IFDEF RunX64}
  h := FindWindow('Tmsgmonx64Host', nil);
  PostMessage(h, WM_USER+100, 0, 0);
  CloseHandle(x64Thread);
{$ENDIF}

  Controller_StopTrace;
*)
end;

procedure TForm1.lvMessagesData(Sender: TObject; Item: TListItem);
begin
  Item.SubItems.Add(IntToStr(Item.Index));
  Item.SubItems.Add('2');
  Item.SubItems.Add('3');
  Item.SubItems.Add('4');
  Item.SubItems.Add('5');
  Item.SubItems.Add('6');
//  Item.Index
end;

function StringBufferSize(const s: string): Integer;
begin
  Result := (Length(s) + 1) * sizeof(WCHAR);
end;

const
  LOGSESSION_NAME = 'MsgMon Session';
  LOGSESSION_FILENAME = 'c:\temp\msgmon.etl';

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
  pSessionProperties.MaximumFileSize := 1;  // 1 MB
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
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)));

    FSessionHandle := 0;
  end;

  if pSessionProperties <> nil then
  begin
    FreeMem(pSessionProperties);
    pSessionProperties := nil;
  end;
end;

type
  TDHSTATUS = ULONG;

function TdhLoadManifest(Manifest: PWChar): TDHSTATUS; stdcall; external 'tdh.dll';

type
  TPROPERTY_DATA_DESCRIPTOR = record
   PropertyName: ULONGLONG;
   ArrayIndex:   ULONG;
   Reserved:     ULONG;
  end;

  PPROPERTY_DATA_DESCRIPTOR = ^TPROPERTY_DATA_DESCRIPTOR;

function TdhGetProperty(
  pEvent: PEVENT_RECORD;
  TdhContextCount: ULONG;
  pTdhContext: Pointer;
  PropertyDataCount: ULONG;
  pPropertyData: PPROPERTY_DATA_DESCRIPTOR;
  BufferSize: ULONG;
  pBuffer: PBYTE
): TDHSTATUS; stdcall; external 'tdh.dll';

function TdhGetPropertySize(
  pEvent: PEVENT_RECORD;
  TdhContextCount: ULONG;
  pTdhContext: Pointer;
  PropertyDataCount: ULONG;
  pPropertyData: PPROPERTY_DATA_DESCRIPTOR;
  BufferSize: PULONG
): TDHSTATUS; stdcall; external 'tdh.dll';

// Todo: this needs to be in its own thread


// Get the metadata for the event.

DWORD GetEventInformation(PEVENT_RECORD pEvent, PTRACE_EVENT_INFO & pInfo)
{
    DWORD status = ERROR_SUCCESS;
    DWORD BufferSize = 0;

    // Retrieve the required buffer size for the event metadata.

    status = TdhGetEventInformation(pEvent, 0, NULL, pInfo, &BufferSize);

    if (ERROR_INSUFFICIENT_BUFFER == status)
    {
        pInfo = (TRACE_EVENT_INFO*) malloc(BufferSize);
        if (pInfo == NULL)
        {
            wprintf(L"Failed to allocate memory for event info (size=%lu).\n", BufferSize);
            status = ERROR_OUTOFMEMORY;
            goto cleanup;
        }

        // Retrieve the event metadata.

        status = TdhGetEventInformation(pEvent, 0, NULL, pInfo, &BufferSize);
    }

    if (ERROR_SUCCESS != status)
    {
        wprintf(L"TdhGetEventInformation failed with 0x%x.\n", status);
    }

cleanup:

    return status;
}

procedure ProcessEvent(pEvent: PEVENT_RECORD); stdcall;
var
  status: DWORD;
  pUserData: PBYTE;
  pEndOfUserData: PByte;
  data: array[0..20] of TPROPERTY_DATA_DESCRIPTOR;
  buf: PByte;
  sz: DWORD;
  PointerSize: Integer;
  i: Integer;
  m: TMsgMonMessage;
begin
  if IsEqualGUID(pEvent.EventHeader.ProviderId, EventTraceGuid) and
    (pEvent.EventHeader.EventDescriptor.Opcode = EVENT_TRACE_TYPE_INFO) then
  begin
    // Skip header information
    Exit;
  end;

  if not IsEqualGUID(pEvent.EventHeader.ProviderId, MsgMonProviderGuid) then
    Exit;

  // Process the event. The pEvent->UserData member is a pointer to
  // the event specific data, if it exists.

//  status := GetEventInformation(pEvent, pInfo);
//  try
//    if ERROR_SUCCESS <> status then
//    begin
//       TODO: Log
//      Exit;
//    end;

  if (EVENT_HEADER_FLAG_32_BIT_HEADER = (pEvent.EventHeader.Flags and EVENT_HEADER_FLAG_32_BIT_HEADER)) then
  begin
    PointerSize := 4;
  end
  else
  begin
    PointerSize := 8;
  end;

  FillChar(data, sizeof(data), 0);
{  data[0].PropertyName := 'Platform';
  data[1].PropertyName := 'Process';
  data[2].PropertyName := 'PID';
  data[3].PropertyName := 'TID';
  data[4].PropertyName := 'dwTickCount';
  data[5].PropertyName := 'hwndFocus';
  data[6].PropertyName := 'hwndActive';
  data[7].PropertyName := 'hwndCapture';
  data[8].PropertyName := 'hwndCaret';
  data[9].PropertyName := 'hwndMenuOwner';
  data[10].PropertyName := 'hwndMoveSize';
  data[11].PropertyName := 'hklActive';
  data[12].PropertyName := 'hwnd';
  data[13].PropertyName := 'message';
  data[14].PropertyName := 'wParam';
  data[15].PropertyName := 'lParam';
  data[16].PropertyName := 'lResult';
  data[17].PropertyName := 'ClassName';
  data[18].PropertyName := 'RealClassName';
  data[19].PropertyName := 'Mode';
  data[20].PropertyName := 'Detail';
  for i := Low(data) to High(data) do data[i].ArrayIndex := $FFFFFFFF;
  //data[0].PropertyName := 'Platform'; //(ULONGLONG)((PBYTE)(pInfo) + pInfo->EventPropertyInfoArray[i].NameOffset);
}
  pUserData := PBYTE(pEvent.UserData);
  pEndOfUserData := PBYTE(PBYTE(pEvent.UserData) + pEvent.UserDataLength);

  status := TdhGetPropertySize(pEvent, 0, nil, Length(data), @data[0], @sz);
  if status <> ERROR_SUCCESS then
    //TODO LOG if status <> ERROR_SUCCESS then
    Exit;

  buf := AllocMem(sz);
  try
    status := TdhGetProperty(pEvent, 0, nil, Length(data), @data[0], sz, buf);
    if status <> ERROR_SUCCESS then
    begin
      // TODO: LOG
      Exit;
    end;

    m := TMsgMonMessage.Create;
    form1.messages.Add(TMsgMonMessage.Create);

  finally
    FreeMem(buf);
  end;
end;

procedure TForm1.Consume_ProcessTrace;
var
  hTrace: TRACEHANDLE;
  status: TDHSTATUS;
  logfile: EVENT_TRACE_LOGFILE;
begin
  lvMessages.Items.Count := 1024;

  status := TdhLoadManifest('msgmon.man');
  if status <> ERROR_SUCCESS then
    RaiseLastOSError(status, 'OpenTraceConsumer: TdhLoadManifest');

  FillChar(logfile, sizeof(logfile), 0);
  logfile.LogFileName := LOGSESSION_FILENAME;
  logfile.LoggerName := nil; // For Real Time Trace
  logfile.LogFileMode.ProcessTraceMode := PROCESS_TRACE_MODE_EVENT_RECORD; // or PROCESS_TRACE_MODE_REAL_TIME
  logfile.EventCallback.EventRecordCallback := ProcessEvent;
  hTrace := OpenTrace(logfile);
  if hTrace = INVALID_HANDLE_VALUE then
    RaiseLastOSError(GetLastError, 'OpenTraceConsumer: OpenTrace');

  status := ProcessTrace(@hTrace, 1, nil, nil);
  if status <> ERROR_SUCCESS then
    RaiseLastOSError(status, 'OpenTraceConsumer: ProcessTrace');

  CloseTrace(hTrace);
  hTrace := 0;

  lvMessages.Items.Count := messages.Count;
end;

end.
