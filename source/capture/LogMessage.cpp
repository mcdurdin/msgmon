#include "stdafx.h"
#include <winmeta.h>
#include "MsgMon.h"

//
// Log a message
//

void LogMessage(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL || pThreadData->etwRegHandle == NULL) {
		// We'll silently fail so we don't spam debug console
		return;
	}

  if (!EventProviderEnabled(pThreadData->etwRegHandle, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
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

	WCHAR szDetail[768]; // TODO: get rid of this evil; pass detail as structured data
	if (!GetMessageDetail(hwnd, message, wParam, lParam, lResult, szDetail)) {
		szDetail[0] = 0;
	}

	// TODO: handle window destruction to remove from info cache

	LogWindow(hwnd);

	EVENT_DATA_DESCRIPTOR Descriptors[MAX_DESCRIPTORS_MESSAGE];

	// These parameters are 64 bit on 64 bit Windows, 32 bit otherwise
	UINT64 wParam64 = (UINT64)wParam;
	UINT64 lParam64 = (UINT64)lParam;
	UINT64 lResult64 = (UINT64)lResult;

	// These must match the manifest template in logtrace.man

	// Event metadata
	EventDataDescCreate(&Descriptors[0], &pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[1], &tid, sizeof(DWORD)); //  <data name = "TID" inType = "win:UInt32" / >

																	 // GUI info
	EventDataDescCreate(&Descriptors[2], (PDWORD)&gti.hwndFocus, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[3], (PDWORD)&gti.hwndActive, sizeof(DWORD)); //  <data name = "ActiveHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[4], (PDWORD)&gti.hwndCapture, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[5], (PDWORD)&gti.hwndCaret, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[6], (PDWORD)&gti.hwndMenuOwner, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[7], (PDWORD)&gti.hwndMoveSize, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
																					 //gti.flags, gti.rcCaret

																					 // Keyboard info
	EventDataDescCreate(&Descriptors[8], (PDWORD)&activeHKL, sizeof(DWORD)); //  <data name = "ActiveHKL" inType = "win:UInt32" / >

																			  // Message raw values
	EventDataDescCreate(&Descriptors[9], (PDWORD)&hwnd, sizeof(DWORD)); //  <data name = "hwnd" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[10], (PDWORD)&message, sizeof(DWORD)); //  <data name = "message" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[11], (PUINT64)&wParam64, sizeof(UINT64)); //  <data name = "wParam" inType = "win:UInt64" / >
	EventDataDescCreate(&Descriptors[12], (PUINT64)&lParam64, sizeof(UINT64)); //  <data name = "lParam" inType = "win:UInt64" / >
	EventDataDescCreate(&Descriptors[13], (PUINT64)&lResult64, sizeof(UINT64)); //  <data name = "lResult64" inType = "win:UInt64" / >

																				// Message detail
	EventDataDescCreate(&Descriptors[14], (PDWORD)&mode, sizeof(DWORD)); //  <data name = "mode" inType = "Mode" / >
	EventDataDescCreate(&Descriptors[15], szDetail, (ULONG)(wcslen(szDetail) + 1) * sizeof(WCHAR)); //  <data name = "mode" inType = "Mode" / >
#if MAX_DESCRIPTORS_MESSAGE != 16
#error MAX_DESCRIPTORS_MESSAGE must match the number of fields in the .man file
  // because we are building this up here, this is a bit of a safety valve
  // when we update the event descriptors, reminding us to keep the constant,
  // the code here, and the .man file in sync (ugh!)
#endif

	DWORD dwErr = EventWrite(pThreadData->etwRegHandle, &MsgMonMessageEvent, (ULONG)MAX_DESCRIPTORS_MESSAGE, &Descriptors[0]);
	if (dwErr != ERROR_SUCCESS) {
		OutputDebugError(L"Log", L"EventWrite", dwErr);
	}
}
