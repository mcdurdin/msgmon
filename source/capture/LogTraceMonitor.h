#pragma once

#include <unordered_map>

// This must be kept in sync with number of descriptors used below and in .man
#define MAX_DESCRIPTORS_MESSAGE 19
#define MAX_DESCRIPTORS_WINDOW 7

// {082E6CC6-239C-4B96-9475-159AA241B4AB}
//static const GUID guid_EtwProviderId =
//{ 0x82e6cc6, 0x239c, 0x4b96,{ 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab } };

// MESSAGE maps
#define PLATFORM_X86	1
#define PLATFORM_X64	2

#define MODE_POST		1
#define MODE_SEND		2
#define MODE_RETURN		3

// WINDOW maps?


//
// System-wide data
//

struct SHAREDDATA {
	HHOOK
		hhookGetMessage,
		hhookCallWndProc,
		hhookCallWndProcRet;
	DWORD
		ownerProcessId;
};

typedef SHAREDDATA *PSHAREDDATA;

//
// Process-level data
//

struct PROCESSDATA {
	WCHAR szProcessPath[MAX_PATH];
	DWORD cchProcessPath;
};

typedef PROCESSDATA *PPROCESSDATA;

//
// Thread-level data structures
//

class WINDOWCONSTANTDATA {
public:
	HWND hwnd;
	DWORD pid, tid;
	HWND hwndParent, hwndOwner;
	std::wstring className;
	std::wstring realClassName;
	WINDOWCONSTANTDATA(HWND hwnd);
};

typedef WINDOWCONSTANTDATA *PWINDOWCONSTANTDATA;

struct WINDOWEVENTDATA {
	HWND hwnd;
	RECT wndRect;
	//std::wstring caption;
};

class THREADDATA {
public:
	REGHANDLE etwRegHandle;
	BOOL inProc;
	std::unordered_map<HWND, PWINDOWCONSTANTDATA> *windows;
	THREADDATA() {
		windows = new std::unordered_map<HWND, PWINDOWCONSTANTDATA>;
	}
	~THREADDATA() {
		for (auto& it: *windows) {
			delete it.second;
		}
		delete windows;
	}
	// WINDOWCLASSNAMES
};

typedef THREADDATA *PTHREADDATA;


//#define MAX_WINDOWMESSAGENAMES 1003
//
//extern PWCHAR szWindowsMessageNames[];

extern HMODULE g_hModule;

BOOL InitThread();
BOOL UninitThread();
BOOL InitProcess();
BOOL UninitProcess();

//PWCHAR GetMessageName(DWORD message);
//void GetWindowReference(HWND hwnd, PWCHAR buf);

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PWCHAR szDetail);

void OutputDebugError(PWCHAR functionName, PWCHAR target);
void OutputDebugError(PWCHAR functionName, PWCHAR target, DWORD dwErr);



void LogMessage(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult);
void LogWindow(HWND hwnd);

PTHREADDATA ThreadData();
PPROCESSDATA ProcessData();