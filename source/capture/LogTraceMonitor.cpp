// LogTraceMonitor.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

//#define DEBUG_LOCALHOOKS

static HANDLE hFileMapping = NULL;
static PSHAREDDATA pSharedData = NULL;
static PROCESSDATA g_processData = { NULL };
static DWORD dwTlsIndex = TLS_OUT_OF_INDEXES;

// Define the GUID to use in TraceLoggingProviderRegister 
// {082E6CC6-239C-4B96-9475-159AA241B4AB}
TRACELOGGING_DEFINE_PROVIDER(
	g_Provider,
	"MsgMon",
	(0x082e6cc6, 0x239c, 0x4b96, 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab));


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

	TLG_STATUS status = TraceLoggingRegisterEx(g_Provider, NULL, NULL);
	if (!SUCCEEDED(status)) {
		OutputDebugError(L"InitProcess", L"TraceLoggingRegisterEx", status);
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
	TraceLoggingUnregister(g_Provider);

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

	PTHREADDATA pThreadData = new THREADDATA();

	if (!TlsSetValue(dwTlsIndex, pThreadData)) {
		delete pThreadData;
		OutputDebugError(L"InitThread", L"TlsSetValue");
		return FALSE;
	}

	/*DWORD dwErr = EventRegister(&ProviderGuid, NULL, NULL, &pThreadData->etwRegHandle);
	if (dwErr != ERROR_SUCCESS) {
		pThreadData->etwRegHandle = NULL;
		OutputDebugError(L"InitThread", L"EventRegister", dwErr);
		UninitThread();
		return FALSE;
	}*/

	return TRUE;
}

BOOL UninitThread() {
	PTHREADDATA pThreadData = (PTHREADDATA)TlsGetValue(dwTlsIndex);

	if (pThreadData != NULL) {
		/*if (pThreadData->etwRegHandle != NULL) {
			DWORD dwErr = EventUnregister(pThreadData->etwRegHandle);
			if (dwErr != ERROR_SUCCESS) {
				OutputDebugError(L"UninitThread", L"EventUnregister", dwErr);
				return FALSE;
			}
		}*/

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

PPROCESSDATA ProcessData() {
	return &g_processData;
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

/*
CRITICAL_SECTION g_InitialisationCS;

DWORD WINAPI ShutdownThreadProc(LPVOID lpParameter) {
	DWORD ownerProcessId = (DWORD)lpParameter;
	HANDLE hOwnerProcess = OpenProcess(SYNCHRONIZE, FALSE, ownerProcessId);
	if(hOwnerProcess != NULL) {
		WaitForSingleObject(hOwnerProcess, INFINITE);
		PostMessage()
}

void InitialiseHook() {
	EnterCriticalSection(&g_InitialisationCS);
	if (!g_Initialised) {
		if (pSharedData && GetCurrentProcessId() != pSharedData->ownerProcessId) {
			HANDLE hThread = CreateThread(NULL, 0, ShutdownThreadProc, (LPVOID) pSharedData->ownerProcessId, 0, NULL);
			CloseHandle(hThread);
		}
		g_Initialised = TRUE;
	}

	LeaveCriticalSection(&g_InitialisationCS);
*/

LRESULT CALLBACK GetMessageHook(INT nCode, WPARAM wParam, LPARAM lParam) {
	if (nCode == HC_ACTION && !InHook()) {
		SetInHook(TRUE);
		/*if (!g_Initialised) {
			InitialiseHook();
		}*/
		PMSG p = PMSG(lParam);
		LogMessage(MODE_POST, p->hwnd, p->message, p->wParam, p->lParam, 0);
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
		LogMessage(MODE_SEND, p->hwnd, p->message, p->wParam, p->lParam, 0);
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
		LogMessage(MODE_RETURN, p->hwnd, p->message, p->wParam, p->lParam, p->lResult);
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
	pSharedData->ownerProcessId = GetCurrentProcessId();

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
