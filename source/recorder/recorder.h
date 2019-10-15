#pragma once

#include <evntrace.h>
#include <evntcons.h>
#include <tdh.h>
#include <stdio.h>

// Define the GUID to use in TraceLoggingProviderRegister 
// {082E6CC6-239C-4B96-9475-159AA241B4AB}
DEFINE_GUID(
  g_Provider,
  0x082e6cc6, 0x239c, 0x4b96, 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab);

// Capture

BOOL Capture(wchar_t *eventName, wchar_t *logfile, BOOL overwrite);

// Store

#ifndef _WIN64

#include <unordered_map>
#include "sqlite3.h"

extern std::unordered_map<std::wstring, sqlite3_stmt*> tables;
extern sqlite3 *db;
extern PTRACE_EVENT_INFO pInfo;
extern DWORD pInfoBufferSize;
extern PBYTE propertyBuf;
extern ULONG currentPropertyBufSize, propertyBufSize;


BOOL Store(wchar_t *logfile, wchar_t *database, BOOL overwrite);
BOOL Check(int value);
BOOL GetEventProperties(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PWSTR &propertyName);

// Store - Functionality for stack traces

BOOL CreateImageLoadTable();
void RecordImageLoad(PEVENT_RECORD event);

#endif
