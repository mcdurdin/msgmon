// msgmon.recorder.cpp : This file contains the 'main' function. Program execution begins and ends there.
//
#include "pch.h"
#define INITGUID
#include <iostream>
#include <windows.h>
#include <evntrace.h>
#include <evntcons.h>
#include "sqlite3.h"
#include <tdh.h>
#include <unordered_map>

BOOL CreateDatabase();
BOOL LoadTrace(PWCHAR filename);
BOOL CommitTransaction();
BOOL FreeStatement();
BOOL CloseDatabase();

BOOL CreateTable(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info);
BOOL InsertRecord(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info);

TRACEHANDLE hTrace = INVALID_PROCESSTRACE_HANDLE;

#define DATABASE_FILE L"c:\\temp\\msgmon.db"
//#define LOGFILE ""

sqlite3 *db = NULL;
sqlite3_stmt *stmt = NULL;

int wmain(int argc, wchar_t *argv[])
{
  DWORD status;

  if (!CreateDatabase()) return 1;

  PWCHAR logfile = (argc == 1 ? NULL : argv[1]);
  if (!LoadTrace(logfile)) return 2;

  status = ProcessTrace(&hTrace, 1, NULL, NULL);
  if (status != ERROR_SUCCESS) {
    return 3;
  }

  CloseTrace(hTrace);

  CommitTransaction();
  FreeStatement();
  CloseDatabase();
}

// Define the GUID to use in TraceLoggingProviderRegister 
// {082E6CC6-239C-4B96-9475-159AA241B4AB}
DEFINE_GUID(
  g_Provider,
  0x082e6cc6, 0x239c, 0x4b96, 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab);

LONGLONG n = 0;

PTRACE_EVENT_INFO pInfo = NULL;
DWORD pInfoBufferSize = 0;

void WINAPI ERC(PEVENT_RECORD event) {
  if (!IsEqualGUID(event->EventHeader.ProviderId, g_Provider)) {
    return;
  }

  if (++n % 10000 == 0) {
    // This may not be necessary for offline trace but will be for
    // realtime trace, in the future
    sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL);
    sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL);
    std::cout << n << std::endl;
  }

  //TdhGetEventInformation(EventRecord, 0, nil, pInfo, @bufferSize);
  sqlite3_bind_int(stmt, 1, event->EventHeader.ProcessId);
  sqlite3_bind_int(stmt, 2, event->EventHeader.ThreadId);
  sqlite3_step(stmt);
  sqlite3_reset(stmt);
  //

  DWORD status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  if (status == ERROR_INSUFFICIENT_BUFFER) {
    if (pInfo) free(pInfo);
    pInfo = (TRACE_EVENT_INFO *)malloc(pInfoBufferSize);
    status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  }

  if (status != ERROR_SUCCESS) return;

  if (pInfo->TaskNameOffset == 0) return;

  PWSTR pTask = (PWSTR)((PBYTE)(pInfo)+pInfo->TaskNameOffset);
  if(wcscmp(pTask, L"Message") == 0) {
    if (!CreateTable(pTask, event, pInfo)) return;
    if (!InsertRecord(pTask, event, pInfo)) return;
  }
  else if (wcscmp(pTask, L"Window") == 0) {

    if (!CreateTable(pTask, event, pInfo)) return;
    if (!InsertRecord(pTask, event, pInfo)) return;
    //std::cout << buf << std::endl;
  }
  else if (wcscmp(pTask, L"Process") == 0) {

  }
  //GetEventInformation(EventRecord, Info);
  //end;
  //  EventRecord.
}
//end;

//
// Maintain a map of created tables and prepared statements
//

BOOL Check(int value) {
  if (value != SQLITE_OK) {
    // TODO LOG
    
    std::cout << "sqlite3 failed with code: " << value << ", message: " << sqlite3_errmsg(db) << std::endl;
    return FALSE;
  }
  return TRUE;
}

std::unordered_map<PWSTR, sqlite3_stmt*> tables;

BOOL CreateTable(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info) {
  auto stmt = tables.find(tableName);
  if (stmt != tables.end()) {
    return TRUE;
  }

  size_t bufSize = info->TopLevelPropertyCount * 24 + 256; // TODO: LAZY!
  char *buf = new char[bufSize], *stmtbuf = new char[bufSize], *p = buf;

  sprintf_s(buf, bufSize, "CREATE TABLE %ws (", tableName);
  sprintf_s(stmtbuf, bufSize, "INSERT INTO %ws VALUES (", tableName);

  for (ULONG i = 0; i < pInfo->TopLevelPropertyCount; i++) {
    PROPERTY_DATA_DESCRIPTOR pdd = { 0 };
    BYTE databuf[256];
    PWCHAR propertyName = (PWCHAR)((PBYTE)(pInfo)+pInfo->EventPropertyInfoArray[i].NameOffset);
    pdd.ArrayIndex = ULONG_MAX;
    pdd.PropertyName = (ULONGLONG)(propertyName);// L"hwnd";
    DWORD status = TdhGetProperty(event, 0, NULL, 1, &pdd, 256, (PBYTE)databuf);
    if (status != ERROR_SUCCESS) return FALSE;

    if (i > 0) strcat_s(buf, bufSize, ", ");
    p = strchr(buf, 0);
    sprintf_s(p, bufSize - (p - buf), "%ws", propertyName);
    //strcat(buf, propertyName);

    if (i > 0) strcat_s(stmtbuf, bufSize, ", ?"); else strcat_s(stmtbuf, bufSize, "?");

    switch (pInfo->EventPropertyInfoArray[i].nonStructType.InType) {
    case TDH_INTYPE_UNICODESTRING:
      strcat_s(buf, bufSize, " TEXT");
      break;
    case TDH_INTYPE_INT32:
    case TDH_INTYPE_UINT32:
    case TDH_INTYPE_INT64:
    case TDH_INTYPE_UINT64:
      strcat_s(buf, bufSize, " INTEGER");
      break;
    default:
      std::wcout << propertyName << " has type " << pInfo->EventPropertyInfoArray[i].nonStructType.InType << std::endl;
      return FALSE;
    }
  }

  strcat_s(buf, bufSize, ")");
  strcat_s(stmtbuf, bufSize, ")");

  // create the table (we'll do this outside a transaction so it's available instantly to the consumer
  if (!Check(sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL))) return FALSE;
  if (!Check(sqlite3_exec(db, buf, NULL, NULL, NULL))) return FALSE;
  if (!Check(sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL))) return FALSE;

  sqlite3_stmt *new_stmt;
  if (!Check(sqlite3_prepare_v2(db, stmtbuf, -1, &new_stmt, NULL))) return FALSE;
  tables[tableName] = new_stmt;
  return TRUE;
}

BOOL InsertRecord(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info) {
  auto stmt = tables.find(tableName);
  if (stmt == tables.end()) {
    return FALSE;
  }

  for (ULONG i = 0; i < pInfo->TopLevelPropertyCount; i++) {
    PROPERTY_DATA_DESCRIPTOR pdd = { 0 };
    BYTE buf[256];
    PWCHAR propertyName = (PWCHAR)((PBYTE)(pInfo)+pInfo->EventPropertyInfoArray[i].NameOffset);
    pdd.ArrayIndex = ULONG_MAX;
    pdd.PropertyName = (ULONGLONG)(propertyName);// L"hwnd";
    DWORD status = TdhGetProperty(event, 0, NULL, 1, &pdd, 256, (PBYTE)buf);
    if (status != ERROR_SUCCESS) return FALSE;
    switch (pInfo->EventPropertyInfoArray[i].nonStructType.InType) {
    case TDH_INTYPE_UNICODESTRING:
      sqlite3_bind_text16(stmt->second, i + 1, buf, -1, NULL);
      break;
    case TDH_INTYPE_INT32:
    case TDH_INTYPE_UINT32:
      sqlite3_bind_int(stmt->second, i + 1, *(int *)buf);
      break;
    case TDH_INTYPE_INT64:
    case TDH_INTYPE_UINT64:
      sqlite3_bind_int64(stmt->second, i + 1, *(sqlite3_int64 *)buf);
      break;
    default:
      std::wcout << propertyName << " has type " << pInfo->EventPropertyInfoArray[i].nonStructType.InType << std::endl;
      return FALSE;
    }
  }

  if (sqlite3_step(stmt->second) != SQLITE_DONE) return FALSE;
  if (!Check(sqlite3_reset(stmt->second))) return FALSE;
  return TRUE;
}

BOOL LoadTrace(PWCHAR filename) {
  EVENT_TRACE_LOGFILE trace = { 0 };
  WCHAR session[] = L"MsgMon_Session";

  if (filename != NULL) {
    trace.LogFileName = filename;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD;
  }
  else {
    trace.LoggerName = session;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD | PROCESS_TRACE_MODE_REAL_TIME;
  }

  trace.EventRecordCallback = ERC;

  hTrace = OpenTrace(&trace);
  if (hTrace == INVALID_PROCESSTRACE_HANDLE) {
    std::cout << GetLastError() << std::endl;
    return FALSE;
  }

  return TRUE;
}

const char *table_create_statement =
"CREATE TABLE messages ("
" PID INT,"
" TID INT,"
" hwndFocus INT,"
" hwndActive INT,"
" hwndCapture INT,"
" hwndCaret INT,"
" hwndMenuOwner INT,"
" hwndMoveSize INT,"
" activeHKL INT,"
" hwnd INT,"
" message INT,"
" wParam INT64,"
" lParam INT64,"
" lResult INT64,"
" mode INT,"
" detail TEXT)";

const char *message_insert = 
  "INSERT INTO messages"
  "(PID, TID, hwndFocus, hwndActive, hwndCapture, hwndCaret, hwndMenuOwner, hwndMoveSize, "
  "activeHKL, hwnd, message, wParam, lParam, lResult, mode, detail) "
  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, "
  "?, ?, ?, ?, ?, ?, ?, ?)";

BOOL CreateDatabase() {
  DeleteFile(DATABASE_FILE);
  if (!Check(sqlite3_open16(DATABASE_FILE, &db))) return FALSE;
  if (!Check(sqlite3_exec(db, table_create_statement, NULL, NULL, NULL))) return FALSE;
  if (!Check(sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL))) return FALSE;

  if (!Check(sqlite3_prepare_v2(db, message_insert, -1, &stmt, NULL))) return FALSE;

  return TRUE;
}

BOOL CommitTransaction() {
  sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL);
  return TRUE;
}

BOOL FreeStatement() {
  sqlite3_finalize(stmt);
  return TRUE;
}

BOOL CloseDatabase() {
  sqlite3_close(db);
  return TRUE;
}

