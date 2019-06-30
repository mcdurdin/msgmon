#include "stdafx.h"
#include "LogTraceMonitor.h"

//
// Log a message
//

void LogMessage(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL) {
		// We'll silently fail so we don't spam debug console
		return;
	}

	if (!TraceLoggingProviderEnabled(g_Provider, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
		// We may need to flush our cache
		pThreadData->windows->clear();
		return;
	}

	LogProcess();

	GUITHREADINFO gti;
	gti.cbSize = sizeof(gti);
	if (!GetGUIThreadInfo(GetCurrentThreadId(), &gti)) {
		// TODO: consider logging failure
		memset(&gti, 0, sizeof(GUITHREADINFO));
	}

	DWORD pid = GetCurrentProcessId();
	DWORD tid = GetCurrentThreadId();
	HKL activeHKL = GetKeyboardLayout(0);

  BYTE bDetail[MAX_DETAIL_BUFFER_SIZE], *pbDetail = bDetail;
  int nDetail;

	if (!GetMessageDetail(hwnd, message, wParam, lParam, lResult, pbDetail, &nDetail)) {
    pbDetail = NULL;
    nDetail = 0;
	}

	// TODO: handle window destruction to remove from info cache

	LogWindow(hwnd);

	// These parameters are 64 bit on 64 bit Windows, 32 bit otherwise
	UINT64 wParam64 = (UINT64)wParam;
	UINT64 lParam64 = (UINT64)lParam;
	UINT64 lResult64 = (UINT64)lResult;

	TraceLoggingWrite(g_Provider, EVENT_MESSAGE,
		TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
		TraceLoggingKeyword(READ_KEYWORD),
		//TraceLoggingValue(EVENT_MESSAGE, "eventType"),
		TraceLoggingValue(pid, "pid"),
		TraceLoggingValue(tid, "tid"),
		TraceLoggingValue((UINT64)gti.hwndFocus, "hwndFocus"),
		TraceLoggingValue((UINT64)gti.hwndActive, "hwndActive"),
		TraceLoggingValue((UINT64)gti.hwndCapture, "hwndCapture"),
		TraceLoggingValue((UINT64)gti.hwndCaret, "hwndCaret"),
		TraceLoggingValue((UINT64)gti.hwndMenuOwner, "hwndMenuOwner"),
		TraceLoggingValue((UINT64)gti.hwndMoveSize, "hwndMoveSize"),
		TraceLoggingValue((DWORD)activeHKL, "activeHKL"),
		TraceLoggingValue((UINT64)hwnd, "hwnd"),
		TraceLoggingValue(message, "message"),
		TraceLoggingValue(wParam64, "wParam"),
		TraceLoggingValue(lParam64, "lParam"),
		TraceLoggingValue(lResult64, "lResult"),
		TraceLoggingValue(mode, "mode"),
    TraceLoggingBinary(pbDetail, nDetail, "detail")
	);
}
