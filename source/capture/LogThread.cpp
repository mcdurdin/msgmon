#include "stdafx.h"
#include "LogTraceMonitor.h"

//
// Log current thread state changes. Each thread is responsible for recording its own
// state changes. Generally, these will happen in line with a message coming in.
//

void LogThread() {
  PTHREADDATA pThreadData = ThreadData();
  if (pThreadData == NULL) {
    // We'll silently fail so we don't spam debug console
    //MMLogError(L"pThreadData == NULL");
    return;
  }

  if (!TraceLoggingProviderEnabled(g_Provider, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
    //MMLogError(L"trace log provider not enabled");
    return;
  }

  LogProcess();

  DWORD tid = GetCurrentThreadId();

  THREADSTATE ts = { 0 };
  HWND hwnd = GetForegroundWindow();
  ts.isForegroundThread = hwnd != 0 && GetWindowThreadProcessId(hwnd, NULL) == tid;
  ts.gti.cbSize = sizeof(GUITHREADINFO);
  if (!GetGUIThreadInfo(tid, &ts.gti)) {
    // TODO: consider logging failure
    memset(&ts.gti, 0, sizeof(GUITHREADINFO));
  }
  ts.activeHKL = GetKeyboardLayout(0);

  if (memcmp(&pThreadData->state, &ts, sizeof(THREADSTATE)) == 0) {
    // State has not changed, so don't report it
    return;
  }

  PWSTR pszThreadDescription;
  // TODO: We only want to do this call if >= Win10 1607
  // We don't track changes to thread description; not worth the extra overhead
  if (!SUCCEEDED(GetThreadDescription(GetCurrentThread(), &pszThreadDescription))) {
    pszThreadDescription = NULL;
  }

  pThreadData->state = ts;

  TraceLoggingWrite(g_Provider, MMEVENTNAME_THREAD,
    TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
    TraceLoggingKeyword(READ_KEYWORD),
    TraceLoggingValue((UINT)tid, "tid"),
    TraceLoggingValue(pszThreadDescription ? pszThreadDescription : L"", "threadDescription"),
    TraceLoggingValue((UINT64)ts.isForegroundThread, "isForegroundThread"),
    TraceLoggingValue((UINT64)ts.gti.hwndFocus, "hwndFocus"),
    TraceLoggingValue((UINT64)ts.gti.hwndActive, "hwndActive"),
    TraceLoggingValue((UINT64)ts.gti.hwndCapture, "hwndCapture"),
    TraceLoggingValue((UINT64)ts.gti.hwndCaret, "hwndCaret"),
    TraceLoggingValue((UINT64)ts.gti.hwndMenuOwner, "hwndMenuOwner"),
    TraceLoggingValue((UINT64)ts.gti.hwndMoveSize, "hwndMoveSize"),
    TraceLoggingValue((DWORD)(UINT64)ts.activeHKL, "activeHKL")
  );

  if (pszThreadDescription) {
    LocalFree(pszThreadDescription);
  }
}
