#pragma once

#include <unordered_map>


// Forward-declare the g_hMyComponentProvider variable that you will use for tracing in this component
TRACELOGGING_DECLARE_PROVIDER(g_Provider);

// {082E6CC6-239C-4B96-9475-159AA241B4AB}
//static const GUID guid_EtwProviderId =
//{ 0x82e6cc6, 0x239c, 0x4b96,{ 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab } };

#define EVENT_MESSAGE "Message"
#define EVENT_WINDOW   "Window"
#define EVENT_PROCESS  "Process"

#define READ_KEYWORD 0x1

//#ifndef TRACE_LEVEL_INFORMATION
#define TRACE_LEVEL_INFORMATION 4
//#endif

//extern const int v_TRACE_LEVEL_INFORMATION;

// PLATFORM maps
#define PLATFORM_X86	1
#define PLATFORM_X64	2

// MESSAGE maps
#define MODE_POST		1
#define MODE_SEND		2
#define MODE_RETURN		3

// WINDOW maps

#define MAX_DETAIL_BUFFER_SIZE 768

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
  // When adding new fields, these must be updated in:
  //   * constructor
  //   * operator==
  //   * LogWindow()
  //   * MsgMon.System.Data.Window.pas::TMMWindow
	HWND hwnd;
  DWORD pid, tid;
	HWND hwndParent, hwndOwner;

  //RECT wndRect;
  //std::wstring caption;

  std::wstring className;
  std::wstring realClassName;

  WINDOWCONSTANTDATA(HWND hwnd);
  bool operator==(const WINDOWCONSTANTDATA& w) const;
};

typedef WINDOWCONSTANTDATA *PWINDOWCONSTANTDATA;

class THREADDATA {
public:
	//REGHANDLE etwRegHandle;
	BOOL inProc;
	BOOL processLogged;
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

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PBYTE pbDetail, int *nDetail);

void OutputDebugError(PWCHAR functionName, PWCHAR target);
void OutputDebugError(PWCHAR functionName, PWCHAR target, DWORD dwErr);



void LogMessage(DWORD mode, HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult);
void LogWindow(HWND hwnd, BOOL recordStateChanges);
void LogProcess();

PTHREADDATA ThreadData();
PPROCESSDATA ProcessData();

#include "../shared/mmlog.h"