#include "stdafx.h"

//
// Log a message
//

void LogProcess() {
  PTHREADDATA pThreadData = ThreadData();
  if (pThreadData == NULL) {
    // We'll silently fail so we don't spam debug console
    return;
  }

  if (pThreadData->processLogged) {
    return;
  }

  if (!TraceLoggingProviderEnabled(g_Provider, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
    return;
  }

  pThreadData->processLogged = TRUE;

  DWORD pid = GetCurrentProcessId();

#ifdef _WIN64
  DWORD platform = PLATFORM_X64;
#else
  DWORD platform = PLATFORM_X86;
#endif

  LPWSTR pszCommandLine = GetCommandLine();

  TraceLoggingWrite(g_Provider, MMEVENTNAME_PROCESS,
	  TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
	  TraceLoggingKeyword(READ_KEYWORD),
	  //TraceLoggingValue(EVENT_PROCESS, "eventType"),
	  TraceLoggingValue(pid, "pid"),
	  TraceLoggingValue(platform, "platform"),
	  TraceLoggingValue(ProcessData()->szProcessPath, "process"),
	  TraceLoggingValue(pszCommandLine, "commandLine")
  );
}
