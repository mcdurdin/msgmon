#include "stdafx.h"

//
// Log a message
//

void LogWindow(HWND hwnd) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL) {
		// We'll silently fail so we don't spam debug console
		return;
	}

	auto v = pThreadData->windows->find(hwnd);
	if (v != pThreadData->windows->end()) {
		return;
	}

	if (!TraceLoggingProviderEnabled(g_Provider, WINEVENT_LEVEL_INFO, READ_KEYWORD)) {
		return;
	}

	// First time we've seen this window. Collect details about it and log an event about it

	WINDOWCONSTANTDATA *w = new WINDOWCONSTANTDATA(hwnd);

	(*pThreadData->windows)[hwnd] = w;

	TraceLoggingWrite(g_Provider, EVENT_WINDOW,
		TraceLoggingLevel(TRACE_LEVEL_INFORMATION),
		TraceLoggingKeyword(READ_KEYWORD),
		//TraceLoggingValue(EVENT_WINDOW, "eventType"),
		TraceLoggingValue((UINT64)hwnd, "hwnd"),
		TraceLoggingValue(w->pid, "pid"),
		TraceLoggingValue(w->tid, "tid"),
		TraceLoggingValue((UINT64)w->hwndOwner, "hwndOwner"),
		TraceLoggingValue((UINT64)w->hwndParent, "hwndParent"),
		TraceLoggingValue(w->className.c_str(), "className"),
		TraceLoggingValue(w->realClassName.c_str(), "realClassName")
	);
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