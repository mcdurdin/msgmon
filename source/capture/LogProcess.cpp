#include "stdafx.h"
#include <winmeta.h>
#include "MsgMon.h"

//
// Log a message
//

void LogProcess() {
  PTHREADDATA pThreadData = ThreadData();
  if (pThreadData == NULL || pThreadData->etwRegHandle == NULL) {
    // We'll silently fail so we don't spam debug console
    return;
  }

  if (pThreadData->processLogged) {
    return;
  }

  if (!EventProviderEnabled(pThreadData->etwRegHandle, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
    return;
  }

  pThreadData->processLogged = TRUE;

  DWORD pid = GetCurrentProcessId();
  EVENT_DATA_DESCRIPTOR Descriptors[MAX_DESCRIPTORS_PROCESS];

#ifdef _WIN64
  DWORD platform = PLATFORM_X64;
#else
  DWORD platform = PLATFORM_X86;
#endif

  LPWSTR pszCommandLine = GetCommandLine();

  // These must match the manifest template in logtrace.man


  // Event metadata
  EventDataDescCreate(&Descriptors[0], &pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
  EventDataDescCreate(&Descriptors[1], &platform, sizeof(DWORD)); // <data name = "Platform" inType = "Platform" / >
  EventDataDescCreate(&Descriptors[2], ProcessData()->szProcessPath, (ULONG)(ProcessData()->cchProcessPath + 1) * sizeof(WCHAR)); //  <data name = "Process" inType = "win:UnicodeString" / >
  EventDataDescCreate(&Descriptors[3], pszCommandLine, (ULONG)(wcslen(pszCommandLine) + 1) * sizeof(WCHAR)); //  <data name = "CommandLine" inType = "win:UnicodeString" / >


#if MAX_DESCRIPTORS_PROCESS != 4
#error MAX_DESCRIPTORS_PROCESS must match the number of fields in the .man file
  // because we are building this up here, this is a bit of a safety valve
  // when we update the event descriptors, reminding us to keep the constant,
  // the code here, and the .man file in sync (ugh!)
#endif

  DWORD dwErr = EventWrite(pThreadData->etwRegHandle, &MsgMonProcessEvent, (ULONG)MAX_DESCRIPTORS_PROCESS, &Descriptors[0]);
  if (dwErr != ERROR_SUCCESS) {
    OutputDebugError(L"Log", L"EventWrite", dwErr);
  }
}
