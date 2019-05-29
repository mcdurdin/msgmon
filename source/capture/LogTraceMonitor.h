#pragma once


#define MAX_WINDOWMESSAGENAMES 1003

extern PWCHAR szWindowsMessageNames[];

extern HMODULE g_hModule;

BOOL InitThread();
BOOL UninitThread();
BOOL InitProcess();
BOOL UninitProcess();

PWCHAR GetMessageName(DWORD message);
void GetWindowReference(HWND hwnd, PWCHAR buf);

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PWCHAR szDetail);

void OutputDebugError(PWCHAR functionName, PWCHAR target);
void OutputDebugError(PWCHAR functionName, PWCHAR target, DWORD dwErr);
