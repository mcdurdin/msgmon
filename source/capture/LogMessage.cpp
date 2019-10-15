#include "stdafx.h"
#include "LogTraceMonitor.h"

//
// Log a message
//



void LogMessage(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL) {
		// We'll silently fail so we don't spam debug console
    MMLogError(L"pThreadData == NULL");
		return;
	}

	if (!TraceLoggingProviderEnabled(g_Provider, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
		// We may need to flush our cache
		pThreadData->windows->clear();
    MMLogError(L"trace log provider not enabled");
    return;
	}

	LogThread();

	DWORD pid = GetCurrentProcessId();
	DWORD tid = GetCurrentThreadId();

  BYTE bDetail[MAX_DETAIL_BUFFER_SIZE], *pbDetail = bDetail;
  wchar_t chDetail[MAX_DETAIL_BUFFER_SIZE * 2 + 1];
  int nDetail;

  if (GetMessageDetail(hwnd, message, wParam, lParam, lResult, pbDetail, &nDetail)) {
    hexStr2(pbDetail, nDetail, chDetail);
  } else {
    pbDetail = NULL;
    nDetail = 1;
    chDetail[0] = L'.';
    chDetail[1] = 0;
	}

	// TODO: handle window destruction to remove from info cache

	LogWindow(hwnd, TRUE);

	// These parameters are 64 bit on 64 bit Windows, 32 bit otherwise
	UINT64 wParam64 = (UINT64)wParam;
	UINT64 lParam64 = (UINT64)lParam;
	UINT64 lResult64 = (UINT64)lResult;

	TraceLoggingWrite(g_Provider, MMEVENTNAME_MESSAGE,
		TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
		TraceLoggingKeyword(READ_KEYWORD),
		//TraceLoggingValue(EVENT_MESSAGE, "eventType"),
		TraceLoggingValue((UINT64)hwnd, "hwnd"),
		TraceLoggingValue(message, "message"),
		TraceLoggingValue(wParam64, "wParam"),
		TraceLoggingValue(lParam64, "lParam"),
		TraceLoggingValue(lResult64, "lResult"),
		TraceLoggingValue(mode, "mode"),
    TraceLoggingValue(chDetail, "detail")
	);
  //,
  //TraceLoggingBinary(pbDetail, nDetail, "detail")
}
