#include "pch.h"
#include "recorder.h"

#ifdef _WIN64
#define LibraryName L"msgmon.capture.x64.dll"
#else
#define LibraryName L"msgmon.capture.x86.dll"
#endif

extern "C" {
  BOOL CALLBACK BeginLog();
  BOOL CALLBACK EndLog();
}

TRACEHANDLE FSessionHandle = 0;
PEVENT_TRACE_PROPERTIES pSessionProperties = NULL;

void StopMsgMonTrace();

// Microsoft-Windows-User-Loader            {B059B83F-D946-4B13-87CA-4292839DC2F2}
// {B059B83F-D946-4B13-87CA-4292839DC2F2} 
static const GUID g_UserLoader =
{ 0xB059B83F, 0xD946, 0x4B13, { 0x87, 0xCA, 0x42, 0x92, 0x83, 0x9D, 0xC2, 0xF2 } };


ULONG StringBufferSize(const wchar_t *logfile) {
  if (logfile == NULL) return 0;
  return (ULONG) (wcslen(logfile) + 1) * sizeof(wchar_t);
}

BOOL StartMsgMonTrace(wchar_t *logfile, BOOL overwrite) {
  ENABLE_TRACE_PARAMETERS params = { 0 };
  ULONG status, BufferSize;

  // Allocate memory for the session properties. The memory must
  // be large enough to include the log file name and session name,
  // which get appended to the end of the session properties structure.

  BufferSize = sizeof(EVENT_TRACE_PROPERTIES) + StringBufferSize(logfile) + sizeof(SESSION_NAME) + sizeof(wchar_t);
  pSessionProperties = (PEVENT_TRACE_PROPERTIES)malloc(BufferSize);
  ZeroMemory(pSessionProperties, BufferSize);

  // Set the session properties. You only append the log file name
  // to the properties structure; the StartTrace function appends
  // the session name for you.

  pSessionProperties->Wnode.BufferSize = BufferSize;
  pSessionProperties->Wnode.Flags = WNODE_FLAG_TRACED_GUID;
  pSessionProperties->Wnode.ClientContext = 1; //QPC clock resolution
  pSessionProperties->Wnode.Guid = g_Provider; //SystemTraceControlGuid; // g_Provider;
  pSessionProperties->MaximumFileSize = 1024; // 1GB max trace size
  pSessionProperties->LoggerNameOffset = sizeof(EVENT_TRACE_PROPERTIES);
  pSessionProperties->LogFileMode = EVENT_TRACE_SYSTEM_LOGGER_MODE;
  if (logfile == NULL) {
    pSessionProperties->LogFileMode |= EVENT_TRACE_REAL_TIME_MODE;
    pSessionProperties->LogFileNameOffset = 0;
  }
  else {
    pSessionProperties->LogFileMode |= EVENT_TRACE_FILE_MODE_SEQUENTIAL;
    pSessionProperties->LogFileNameOffset = sizeof(EVENT_TRACE_PROPERTIES) + sizeof(SESSION_NAME) + sizeof(wchar_t);
    wcscpy_s(PWCHAR(PBYTE(pSessionProperties) + pSessionProperties->LogFileNameOffset), wcslen(logfile)+1, logfile);
  }
  pSessionProperties->EnableFlags = EVENT_TRACE_FLAG_IMAGE_LOAD; //?? may want these as well? | EVENT_TRACE_FLAG_PROCESS | EVENT_TRACE_FLAG_THREAD;

  status = StartTrace(&FSessionHandle, SESSION_NAME, pSessionProperties);
  if (ERROR_ALREADY_EXISTS == status) {
    // The trace was already started, perhaps it did not close down cleanly
    // We'll stop it and restart it
    status = ControlTraceW(FSessionHandle, SESSION_NAME, pSessionProperties, EVENT_TRACE_CONTROL_STOP);
    if (ERROR_SUCCESS != status) {
      MMLogError(L"ControlTrace failed with error %d", status);
      StopMsgMonTrace();
      return FALSE;
    }
    status = StartTrace(&FSessionHandle, SESSION_NAME, pSessionProperties);
  }
  if (ERROR_SUCCESS != status) {
    MMLogError(L"StartTrace failed with error %d", status);
    StopMsgMonTrace();
    return FALSE;
  }

  params.Version = ENABLE_TRACE_PARAMETERS_VERSION_2;
  params.EnableProperty = EVENT_ENABLE_PROPERTY_STACK_TRACE;

  status = EnableTraceEx2(
    FSessionHandle,
    &g_Provider,
    EVENT_CONTROL_CODE_ENABLE_PROVIDER,
    TRACE_LEVEL_INFORMATION,
    0,
    0,
    0,
    &params
  );

  if (ERROR_SUCCESS != status) {
    MMLogError(L"EnableTraceEx2 failed with error %d", status);
    StopMsgMonTrace();
    return FALSE;
  }

  return TRUE;
}

void StopMsgMonTrace() {
  ULONG status;
  if (FSessionHandle != NULL) {
    status = ControlTrace(FSessionHandle, SESSION_NAME, pSessionProperties, EVENT_TRACE_CONTROL_STOP);
    if (ERROR_SUCCESS != status) {
      MMLogError(L"ControlTrace failed with %d", status);
    }

    FSessionHandle = NULL;
  }

  if (pSessionProperties != NULL) {
    free(pSessionProperties);
    pSessionProperties = NULL;
  }
}

BOOL Capture(wchar_t *eventName, wchar_t *logfile, BOOL overwrite) {
  MSG msg;
  BOOL bResult = FALSE;
  DWORD mode;
  HANDLE hstdin = NULL;
  HANDLE hEvent = NULL;
  HANDLE hWaitObject = NULL;

  if (eventName) {
	  hEvent = OpenEvent(SYNCHRONIZE, FALSE, eventName);
	  if (!hEvent) {
      MMLogError(L"Capture: OpenEvent failed with error %d", GetLastError());
		  goto cleanup;
	  }
  }
  else {
    // Switch to raw mode
    hstdin = GetStdHandle(STD_INPUT_HANDLE);
    GetConsoleMode(hstdin, &mode);
    SetConsoleMode(hstdin, 0);

    MMShowInfo(L"Press any key to end trace.");
  }
  
  hWaitObject = hEvent ? hEvent : hstdin;

#ifndef _WIN64
  // Start the trace, only on x86 host (x64 host will piggy back)
  if (!StartMsgMonTrace(logfile, overwrite)) {
    MMLogError(L"Capture: StartMsgMonTrace failed");
    goto cleanup;
  }
#endif

  if (!BeginLog()) {
    MMLogError(L"BeginLog failed (GLE=%d)", GetLastError());
    goto cleanup;
  }

  bResult = TRUE;

	// Because we are running a hook, it's a good idea to run our own message loop.
	while (MsgWaitForMultipleObjects(1, &hWaitObject, FALSE, INFINITE, QS_ALLINPUT) == WAIT_OBJECT_0 + 1) {
		while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
			DispatchMessage(&msg);
		}
	}

  if (hstdin) {
    // Flush console and restore
    WCHAR ch;
    DWORD count;

    // Read the (single) key pressed
    ReadConsole(hstdin, &ch, 1, &count, NULL);
  }
  
cleanup:
  if (hstdin) {
    // Restore the console to its previous state
    SetConsoleMode(hstdin, mode);
  }

  if (hEvent != NULL) {
    CloseHandle(hEvent);
  }

  if (bResult) {
    EndLog();
  }

#ifndef _WIN64
  StopMsgMonTrace();
#endif

  return bResult;
}