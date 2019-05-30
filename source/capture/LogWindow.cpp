#include "stdafx.h"
#include <winmeta.h>

//
// Log a message
//

void LogWindow(HWND hwnd) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL || pThreadData->etwRegHandle == NULL) {
		// We'll silently fail so we don't spam debug console
		return;
	}

	auto v = pThreadData->windows->find(hwnd);
	if (v != pThreadData->windows->end()) {
		return;
	}

	if (!EventProviderEnabled(pThreadData->etwRegHandle, WINEVENT_LEVEL_INFO, READ_KEYWORD)) return;

	// First time we've seen this window. Collect details about it and log an event about it

	OutputDebugString(L"About to log window");

	WINDOWCONSTANTDATA *w = new WINDOWCONSTANTDATA(hwnd);

	(*pThreadData->windows)[hwnd] = w;

	EVENT_DATA_DESCRIPTOR Descriptors[MAX_DESCRIPTORS_WINDOW];

	// These must match the manifest template in logtrace.man

	// Event metadata
	EventDataDescCreate(&Descriptors[0], (PDWORD)&hwnd, sizeof(DWORD)); // <data name="hwnd" inType="win:UInt32" />
	EventDataDescCreate(&Descriptors[1], &w->pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[2], &w->tid, sizeof(DWORD)); //  <data name = "TID" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[3], (PDWORD)&w->hwndOwner, sizeof(DWORD)); //  <data name = "hwndOwner" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[4], (PDWORD)&w->hwndParent, sizeof(DWORD)); //  <data name = "hwndParent" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[5], w->className.c_str(), (ULONG)(w->className.size() + 1) * sizeof(WCHAR)); //  <data name = "ClassName" inType = "win:UnicodeString" / >
	EventDataDescCreate(&Descriptors[6], w->realClassName.c_str(), (ULONG)(w->realClassName.size() + 1) * sizeof(WCHAR)); //  <data name = "RealClassName" inType = "win:UnicodeString" / >
	//assert(MAX_DESCRIPTORS_WINDOW == 6 + 1);

	DWORD dwErr = EventWrite(pThreadData->etwRegHandle, &MsgMonWindowEvent, (ULONG)MAX_DESCRIPTORS_WINDOW, &Descriptors[0]);
	if (dwErr != ERROR_SUCCESS) {
		OutputDebugError(L"Log", L"EventWrite", dwErr);
	}
}

WINDOWCONSTANTDATA::WINDOWCONSTANTDATA(HWND hwnd) {
	const auto CLASSNAME_MAX = 256;

	this->hwnd = hwnd;
	this->tid = GetWindowThreadProcessId(hwnd, &this->pid);

	this->className.resize(CLASSNAME_MAX);
	int cchClassName = GetClassName(hwnd, &this->className[0], CLASSNAME_MAX);
	this->className.resize(cchClassName);

	// This allows us to use appropriate message types for EM_, BM_ etc
	this->realClassName.resize(CLASSNAME_MAX);
	int cchRealClassName = RealGetWindowClass(hwnd, &this->realClassName[0], CLASSNAME_MAX);
	this->realClassName.resize(cchRealClassName);

	this->hwndParent = GetParent(hwnd);
	this->hwndOwner = GetWindow(hwnd, GW_OWNER);
}