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

  THREADSTATE ts = { 0 };
  HWND hwnd = GetForegroundWindow();
  ts.isForegroundThread = hwnd != 0 && GetWindowThreadProcessId(hwnd, NULL) == GetCurrentThreadId();
  ts.gti.cbSize = sizeof(GUITHREADINFO);
  if (!GetGUIThreadInfo(GetCurrentThreadId(), &ts.gti)) {
    // TODO: consider logging failure
    memset(&ts.gti, 0, sizeof(GUITHREADINFO));
  }
  ts.activeHKL = GetKeyboardLayout(0);

  if (memcmp(&pThreadData->state, &ts, sizeof(THREADSTATE)) == 0) {
    // State has not changed, so don't report it
    return;
  }

  pThreadData->state = ts;

  TraceLoggingWrite(g_Provider, MMEVENTNAME_THREAD,
    TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
    TraceLoggingKeyword(READ_KEYWORD),
    TraceLoggingValue((UINT64)ts.isForegroundThread, "isForegroundThread"),
    TraceLoggingValue((UINT64)ts.gti.hwndFocus, "hwndFocus"),
    TraceLoggingValue((UINT64)ts.gti.hwndActive, "hwndActive"),
    TraceLoggingValue((UINT64)ts.gti.hwndCapture, "hwndCapture"),
    TraceLoggingValue((UINT64)ts.gti.hwndCaret, "hwndCaret"),
    TraceLoggingValue((UINT64)ts.gti.hwndMenuOwner, "hwndMenuOwner"),
    TraceLoggingValue((UINT64)ts.gti.hwndMoveSize, "hwndMoveSize"),
    TraceLoggingValue((DWORD)ts.activeHKL, "activeHKL")
  );
}
