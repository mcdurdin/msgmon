#include "pch.h"

#ifdef WIN64
#define LibraryName L"msgmon.capture.x64.dll"
#else
#define LibraryName L"msgmon.capture.x86.dll"
#endif

extern "C" {
  BOOL CALLBACK BeginLog();
  BOOL CALLBACK EndLog();
}

BOOL StartMsgMonTrace(wchar_t *logfile, BOOL overwrite) {
  return FALSE;
}

void StopMsgMonTrace() {

}

BOOL Startx64Host(HANDLE hEvent) {
  return FALSE;
}

BOOL Capture(HANDLE hEvent, wchar_t *logfile, BOOL overwrite, BOOL x86only) {
  MSG msg;
  BOOL bResult = FALSE;

#ifndef WIN64
  // Start the trace and the x64 host
  if (!StartMsgMonTrace(logfile, overwrite))
    return FALSE;

  if (!x86only && !Startx64Host(hEvent))
    goto cleanup;
#endif

  if (!BeginLog()) {
    goto cleanup;
  }

  bResult = TRUE;

  while (MsgWaitForMultipleObjects(1, &hEvent, TRUE, INFINITE, QS_ALLINPUT) == WAIT_OBJECT_0 + 1) {
    while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
      DispatchMessage(&msg);
    }
  }

cleanup:
  if (bResult) {
    EndLog();
  }

#ifndef WIN64
  StopMsgMonTrace();
#endif

  return bResult;
}