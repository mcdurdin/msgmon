// dllmain.cpp : Defines the entry point for the DLL application.
#include "stdafx.h"

HMODULE g_hModule;

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                     )
{
    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
		g_hModule = hModule;
		InitProcess();
		break;
    case DLL_THREAD_ATTACH:
		InitThread();
		break;
    case DLL_THREAD_DETACH:
		//UninitThread();
		break;
    case DLL_PROCESS_DETACH:
		UninitProcess();
        break;
    }
    return TRUE;
}

