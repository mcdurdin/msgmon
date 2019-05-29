// LogTraceMonitor.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "MsgMon.h"

#define MAX_DESCRIPTORS 21

// {082E6CC6-239C-4B96-9475-159AA241B4AB}
//static const GUID guid_EtwProviderId =
//{ 0x82e6cc6, 0x239c, 0x4b96,{ 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab } };

#define PLATFORM_X86	1
#define PLATFORM_X64	2

#define MODE_POST		1
#define MODE_SEND		2
#define MODE_RETURN		3

//
// System-wide data
//

struct SHAREDDATA {
	HHOOK     
		hhookGetMessage,
		hhookCallWndProc,
		hhookCallWndProcRet;
};

typedef SHAREDDATA *PSHAREDDATA;

static HANDLE hFileMapping = NULL;
static PSHAREDDATA pSharedData = NULL;

//
// Process-level data
//

struct PROCESSDATA {
	WCHAR szProcessPath[MAX_PATH];
	DWORD cchProcessPath;
};

static PROCESSDATA g_processData = { NULL };
static DWORD dwTlsIndex = TLS_OUT_OF_INDEXES;

//
// Thread-level data structures
//

struct THREADDATA {
	REGHANDLE etwRegHandle;
	BOOL inProc;
	// WINDOWCLASSNAMES
};

typedef THREADDATA *PTHREADDATA;

//
// Thread and process initialization
//

BOOL InitProcess() {
	if (GetModuleFileName(NULL, g_processData.szProcessPath, MAX_PATH) == 0) {
		OutputDebugError(L"InitProcess", L"GetModuleFileName");
		return FALSE;
	}

	g_processData.cchProcessPath = (DWORD) wcslen(g_processData.szProcessPath);

	if ((dwTlsIndex = TlsAlloc()) == TLS_OUT_OF_INDEXES) {
		OutputDebugError(L"InitProcess", L"TlsAlloc");
		return FALSE;
	}

	hFileMapping = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, sizeof(SHAREDDATA), L"S4SLogTraceMonitor");
	if (hFileMapping == 0) {
		OutputDebugError(L"InitProcess", L"CreateFileMapping");
		UninitProcess();
		return FALSE;
	}

	pSharedData = (PSHAREDDATA)MapViewOfFile(hFileMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	if (!pSharedData) {
		OutputDebugError(L"InitProcess", L"CreateFileMapping");
		UninitProcess();
		return FALSE;
	}

	if (!InitThread()) {
		UninitProcess();
		return FALSE;
	}

	return TRUE;
}

BOOL UninitProcess() {
	if (pSharedData != NULL) {
		UnmapViewOfFile(pSharedData);
		pSharedData = NULL;
	}
	if (hFileMapping != NULL) {
		CloseHandle(hFileMapping);
		hFileMapping = NULL;
	}

	if (dwTlsIndex != TLS_OUT_OF_INDEXES) {
		TlsFree(dwTlsIndex);
		dwTlsIndex = TLS_OUT_OF_INDEXES;
	}

	return TRUE;
}

//
// Thread initialization
//

BOOL InitThread() {
	if (dwTlsIndex == TLS_OUT_OF_INDEXES) {
		OutputDebugError(L"InitThread", L"dwTlsIndex == TLS_OUT_OF_INDEXES"); 
		return FALSE;
	}

	PTHREADDATA pThreadData = new THREADDATA;

	if (!TlsSetValue(dwTlsIndex, pThreadData)) {
		delete pThreadData;
		OutputDebugError(L"InitThread", L"TlsSetValue");
		return FALSE;
	}

	DWORD dwErr = EventRegister(&ProviderGuid, NULL, NULL, &pThreadData->etwRegHandle);
	if (dwErr != ERROR_SUCCESS) {
		pThreadData->etwRegHandle = NULL;
		OutputDebugError(L"InitThread", L"EventRegister", dwErr);
		UninitThread();
		return FALSE;
	}

	return TRUE;
}

BOOL UninitThread() {
	PTHREADDATA pThreadData = (PTHREADDATA)TlsGetValue(dwTlsIndex);

	if (pThreadData != NULL) {
		if (pThreadData->etwRegHandle != NULL) {
			DWORD dwErr = EventUnregister(pThreadData->etwRegHandle);
			if (dwErr != ERROR_SUCCESS) {
				OutputDebugError(L"UninitThread", L"EventUnregister", dwErr);
				return FALSE;
			}
		}

		TlsSetValue(dwTlsIndex, NULL);
		delete pThreadData;
		return TRUE;
	}

	return FALSE;
}

PTHREADDATA ThreadData() {
	if (dwTlsIndex == TLS_OUT_OF_INDEXES)
		return NULL;
	return (PTHREADDATA)TlsGetValue(dwTlsIndex);
}

//
//  Log helper functions
//

/*PWCHAR GetMessageName(DWORD message) {
	if (message < MAX_WINDOWMESSAGENAMES) {
		return szWindowsMessageNames[message];
	}
	return NULL;
}*/

//
// Log a message
//

void Log(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData == NULL || pThreadData->etwRegHandle == NULL) {
		// We'll silently fail so we don't spam debug console
		return;
	}

	GUITHREADINFO gti;
	gti.cbSize = sizeof(gti);
	if (!GetGUIThreadInfo(GetCurrentThreadId(), &gti)) {
		// TODO: consider logging failure
		memset(&gti, 0, sizeof(GUITHREADINFO));
	}

	DWORD pid = GetCurrentProcessId();
	DWORD tid = GetCurrentThreadId();
	DWORD tickCount = GetTickCount();
	HKL activeHKL = GetKeyboardLayout(0);

	WCHAR szDetail[768]; // TODO: get rid of this evil; pass detail as structured data
	if (!GetMessageDetail(hwnd, message, wParam, lParam, lResult, szDetail)) {
		szDetail[0] = 0;
	}

	// TODO: Cache class names?
	WCHAR wszClassName[32];
	int cchClassName = GetClassName(hwnd, wszClassName, 32);
	wszClassName[cchClassName] = 0;

	// This allows us to use appropriate message types for EM_, BM_ etc
	WCHAR wszRealClassName[32];
	int cchRealClassName = RealGetWindowClass(hwnd, wszRealClassName, 32);
	wszRealClassName[cchRealClassName] = 0;

	EVENT_DATA_DESCRIPTOR Descriptors[MAX_DESCRIPTORS];

#ifdef _WIN64
	DWORD platform = PLATFORM_X64;
#else
	DWORD platform = PLATFORM_X86;
#endif

	// These parameters are 64 bit on 64 bit Windows, 32 bit otherwise
	UINT64 wParam64 = (UINT64)wParam;
	UINT64 lParam64 = (UINT64)lParam;
	UINT64 lResult64 = (UINT64)lResult;

	// These must match the manifest template in logtrace.man

	// Event metadata
	EventDataDescCreate(&Descriptors[0], &platform, sizeof(DWORD)); // <data name = "Platform" inType = "Platform" / >
	EventDataDescCreate(&Descriptors[1], g_processData.szProcessPath, (ULONG)(g_processData.cchProcessPath + 1) * sizeof(WCHAR)); //  <data name = "Process" inType = "win:UnicodeString" / >
	EventDataDescCreate(&Descriptors[2], &pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[3], &tid, sizeof(DWORD)); //  <data name = "TID" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[4], &tickCount, sizeof(DWORD)); //  <data name = "TickCount" inType = "win:UInt32" / >

	// GUI info
	EventDataDescCreate(&Descriptors[5], (PDWORD)&gti.hwndFocus, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[6], (PDWORD)&gti.hwndActive, sizeof(DWORD)); //  <data name = "ActiveHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[7], (PDWORD)&gti.hwndCapture, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[8], (PDWORD)&gti.hwndCaret, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[9], (PDWORD)&gti.hwndMenuOwner, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[10], (PDWORD)&gti.hwndMoveSize, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
	//gti.flags, gti.rcCaret

	// Keyboard info
	EventDataDescCreate(&Descriptors[11], (PDWORD)&activeHKL, sizeof(DWORD)); //  <data name = "ActiveHKL" inType = "win:UInt32" / >

	// Message raw values
	EventDataDescCreate(&Descriptors[12], (PDWORD)&hwnd, sizeof(DWORD)); //  <data name = "hwnd" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[13], (PDWORD)&message, sizeof(DWORD)); //  <data name = "message" inType = "win:UInt32" / >
	EventDataDescCreate(&Descriptors[14], (PUINT64)&wParam64, sizeof(UINT64)); //  <data name = "wParam" inType = "win:UInt64" / >
	EventDataDescCreate(&Descriptors[15], (PUINT64)&lParam64, sizeof(UINT64)); //  <data name = "lParam" inType = "win:UInt64" / >
	EventDataDescCreate(&Descriptors[16], (PUINT64)&lResult64, sizeof(UINT64)); //  <data name = "lResult64" inType = "win:UInt64" / >

	// Message detail
	EventDataDescCreate(&Descriptors[17], wszClassName, (ULONG)(cchClassName + 1) * sizeof(WCHAR)); //  <data name = "hwndClassName" inType = "win:UnicodeString" / >
	EventDataDescCreate(&Descriptors[18], wszRealClassName, (ULONG)(cchRealClassName + 1) * sizeof(WCHAR)); //  <data name = "hwndClassName" inType = "win:UnicodeString" / >
	EventDataDescCreate(&Descriptors[19], (PDWORD)&mode, sizeof(DWORD)); //  <data name = "mode" inType = "Mode" / >
	EventDataDescCreate(&Descriptors[20], szDetail, (ULONG)(wcslen(szDetail) + 1) * sizeof(WCHAR)); //  <data name = "mode" inType = "Mode" / >

	DWORD dwErr = EventWrite(pThreadData->etwRegHandle, &MsgMonEvent, (ULONG)MAX_DESCRIPTORS, &Descriptors[0]);
	if (dwErr != ERROR_SUCCESS) {
		OutputDebugError(L"Log", L"EventWrite");
	}
}

//
// Hook management
//


BOOL InHook() {
	PTHREADDATA pThreadData = ThreadData();
	return (pThreadData == NULL || pThreadData->inProc);
}

void SetInHook(BOOL f) {
	PTHREADDATA pThreadData = ThreadData();
	if (pThreadData) pThreadData->inProc = f;
}

LRESULT CALLBACK GetMessageHook(INT nCode, WPARAM wParam, LPARAM lParam) {
	if (nCode == HC_ACTION && !InHook()) {
		SetInHook(TRUE);
		PMSG p = PMSG(lParam);
		Log(MODE_POST, p->hwnd, p->message, p->wParam, p->lParam, 0);
		SetInHook(FALSE);
	}

	if (pSharedData && pSharedData->hhookGetMessage) {
		return CallNextHookEx(pSharedData->hhookGetMessage, nCode, wParam, lParam);
	}
	return 0;
}

LRESULT CALLBACK CallWndProcHook(INT nCode, WPARAM wParam, LPARAM lParam) {
	if (nCode == HC_ACTION && !InHook()) {
		SetInHook(TRUE);
		PCWPSTRUCT p = PCWPSTRUCT(lParam);
		Log(MODE_SEND, p->hwnd, p->message, p->wParam, p->lParam, 0);
		SetInHook(FALSE);
	}

	if (pSharedData && pSharedData->hhookCallWndProc) {
		return CallNextHookEx(pSharedData->hhookCallWndProc, nCode, wParam, lParam);
	}
	return 0;
}


LRESULT CALLBACK CallWndProcRetHook(INT nCode, WPARAM wParam, LPARAM lParam) {
	if (nCode == HC_ACTION) {
		PCWPRETSTRUCT p = PCWPRETSTRUCT(lParam);
		Log(MODE_RETURN, p->hwnd, p->message, p->wParam, p->lParam, p->lResult);
	}

	if (pSharedData && pSharedData->hhookCallWndProcRet) {
		return CallNextHookEx(pSharedData->hhookCallWndProcRet, nCode, wParam, lParam);
	}
	return 0;
}

///
/// Exports
///

// https://stackoverflow.com/a/41910450/1836776
#define EXPORT comment(linker, "/EXPORT:" __FUNCTION__ "=" __FUNCDNAME__)

extern "C" BOOL CALLBACK BeginLog() {
	#pragma EXPORT

	if (!pSharedData) {
		return FALSE;
	}

#define DEBUG_LOCALHOOKS
#ifdef DEBUG_LOCALHOOKS
	HMODULE hModule = 0;
	DWORD tid = GetCurrentThreadId();
#else
	HMODULE hModule = g_hModule;
	DWORD tid = 0;
#endif

	pSharedData->hhookGetMessage = SetWindowsHookEx(WH_GETMESSAGE, &GetMessageHook, hModule, tid);
	pSharedData->hhookCallWndProc = SetWindowsHookEx(WH_CALLWNDPROC, &CallWndProcHook, hModule, tid);
	pSharedData->hhookCallWndProcRet = SetWindowsHookEx(WH_CALLWNDPROCRET, &CallWndProcRetHook, hModule, tid);

	return pSharedData->hhookGetMessage && pSharedData->hhookCallWndProc && pSharedData->hhookCallWndProcRet;
}

extern "C" BOOL CALLBACK EndLog() {
	#pragma EXPORT

	if (!pSharedData) {
		return FALSE;
	}

	UnhookWindowsHookEx(pSharedData->hhookGetMessage);
	UnhookWindowsHookEx(pSharedData->hhookCallWndProc);
	UnhookWindowsHookEx(pSharedData->hhookCallWndProcRet);
	memset(pSharedData, 0, sizeof(SHAREDDATA));
	return TRUE;
}

void OutputDebugError(PWCHAR functionName, PWCHAR target) {
	OutputDebugError(functionName, target, GetLastError());
}

void OutputDebugError(PWCHAR functionName, PWCHAR target, DWORD dwErr) {
	WCHAR buf[MAX_PATH], wszErr[MAX_PATH];
	if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, dwErr, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), wszErr, MAX_PATH, NULL) == 0) {
		wszErr[0] = 0;
	}
	swprintf_s(buf, L"LogTraceMonitor::%s %s failed with %d: %s", functionName, target, dwErr, wszErr);
	OutputDebugString(buf);
}
