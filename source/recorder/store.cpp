// msgmon.recorder.cpp : This file contains the 'main' function. Program execution begins and ends there.
//
#include "pch.h"
#include "recorder.h"
#include "sqlite3.h"

BOOL CreateDatabase(PCTSTR szPath, BOOL fOverwrite);
BOOL LoadTrace(PTSTR szPath);
BOOL CommitTransaction();
BOOL BeginTransaction();
BOOL FreeStatements();
BOOL CloseDatabase();

BOOL CreateTable(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int *rowCounter);
BOOL InsertRecord(int index, PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info);
void WINAPI EventRecordCallback(PEVENT_RECORD event);

sqlite3 *db = NULL;

LONGLONG recordNum = 0;
TRACEHANDLE hTrace = INVALID_PROCESSTRACE_HANDLE;
PTRACE_EVENT_INFO pInfo = NULL;
DWORD pInfoBufferSize = 0;
int msgIndex = 0, windowIndex = 0, processIndex = 0;

// Maintain a map of created tables and prepared statements
std::unordered_map<std::wstring, sqlite3_stmt*> tables;

BOOL Store(wchar_t *logfile, wchar_t *database, BOOL overwrite) {
  DWORD status;

  if (!CreateDatabase(database, overwrite)) {
    MMLogError(L"CreateDatabase failed");
    return FALSE;
  }

  if (!LoadTrace(logfile)) {
    MMLogError(L"LoadTrace failed");
    return FALSE;
  }

  int repeat = 0;

  MMLogInfo(L"Starting to process trace from %s into %s", logfile ? logfile : L"realtime session", database);
  
  do {
    status = ProcessTrace(&hTrace, 1, NULL, NULL);
    if (status != ERROR_SUCCESS) {
      MMLogError(L"ProcessTrace is not ready (received error %d). Waiting 0.1 seconds");
      Sleep(100);
      if (++repeat > 20) break;
    }
  } while (status != ERROR_SUCCESS);

  CloseTrace(hTrace);

  MMLogInfo(L"Finishing trace");

  CommitTransaction();
  FreeStatements();

  CloseDatabase();

  return TRUE;
}

BOOL LoadTrace(PTSTR szPath) {
  EVENT_TRACE_LOGFILE trace = { 0 };
  TCHAR session[] = SESSION_NAME;

  if (szPath != NULL) {
    trace.LogFileName = szPath;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD;
  }
  else {
    trace.LoggerName = session;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD | PROCESS_TRACE_MODE_REAL_TIME;
  }

  trace.EventRecordCallback = EventRecordCallback;

  hTrace = OpenTrace(&trace);
  if (hTrace == INVALID_PROCESSTRACE_HANDLE) {
    MMLogError(L"OpenTrace failed with error %d", GetLastError());
    return FALSE;
  }

  return TRUE;
}


void WINAPI EventRecordCallback(PEVENT_RECORD event) {
  if (!IsEqualGUID(event->EventHeader.ProviderId, g_Provider)) {
    return;
  }

  if (++recordNum % 10000 == 0) {
    // This may not be necessary for offline trace but will be for
    // realtime trace, in the future
    if (!CommitTransaction()) return;
    if (!BeginTransaction()) return;
    MMShowInfo(L"... %d records", recordNum);
  }

  DWORD status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  if (status == ERROR_INSUFFICIENT_BUFFER) {
    if (pInfo) free(pInfo);
    pInfo = (TRACE_EVENT_INFO *)malloc(pInfoBufferSize);
    status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  }

  if (status != ERROR_SUCCESS) {
    MMLogError(L"Failed to get event information with error %d", status);
    return;
  }

  if (pInfo->TaskNameOffset == 0) {
    MMLogError(L"Event has no task name");
    return;
  }

  PWSTR pTask = (PWSTR)((PBYTE)(pInfo)+pInfo->TaskNameOffset);
  if (wcscmp(pTask, L"Message") == 0) {
    if (!CreateTable(pTask, event, pInfo, &msgIndex)) {
      MMLogError(L"Failed to create Message table");
      return;
    }
    if (!InsertRecord(msgIndex++, pTask, event, pInfo)) {
      MMLogError(L"Failed to insert Message record");
      return;
    }
  }
  else if (wcscmp(pTask, L"Window") == 0) {
    if (!CreateTable(pTask, event, pInfo, &windowIndex)) {
      MMLogError(L"Failed to create Window table");
      return;
    }
    if (!InsertRecord(windowIndex++, pTask, event, pInfo)) {
      MMLogError(L"Failed to insert Window record");
      return;
    }
  }
  else if (wcscmp(pTask, L"Process") == 0) {
    if (!CreateTable(pTask, event, pInfo, &processIndex)) {
      MMLogError(L"Failed to create Process table");
      return;
    }
    if (!InsertRecord(processIndex++, pTask, event, pInfo)) {
      MMLogError(L"Failed to insert Process record");
      return;
    }
  }
  else {
    MMLogError(L"Received an event with an unexpected name '%s'", pTask);
    return;
  }
}

BOOL Check(int value) {
  if (value != SQLITE_OK) {
    MMLogError(L"sqlite3 failed with code: %d, message: %S", value, sqlite3_errmsg(db));
    return FALSE;
  }
  return TRUE;
}

PBYTE propertyBuf = NULL;
ULONG currentPropertyBufSize = 0, propertyBufSize = 0;

BOOL GetEventProperties(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PWSTR &propertyName) {
  propertyName = (PWCHAR)((PBYTE)(pInfo)+pInfo->EventPropertyInfoArray[propertyIndex].NameOffset);
  PROPERTY_DATA_DESCRIPTOR pdd = { 0 };
  pdd.ArrayIndex = ULONG_MAX;
  pdd.PropertyName = (ULONGLONG)(propertyName);

  ULONG sz;
  DWORD status = TdhGetPropertySize(event, 0, NULL, 1, &pdd, &sz);
  if (status != ERROR_SUCCESS) {
    MMLogError(L"Table %s failed to get property size with error %d", tableName, status);
    return FALSE;
  }

  if (sz == 0) {
    return TRUE;
  }
  else {

    if (sz + 2 > propertyBufSize) {
      if (propertyBuf) delete propertyBuf;
      propertyBufSize = sz + 2; // add trailing WCHAR nul
      propertyBuf = new BYTE[propertyBufSize];
    }
    status = TdhGetProperty(event, 0, NULL, 1, &pdd, propertyBufSize, propertyBuf);
    // Always nul terminate (even if not a string!)
    propertyBuf[sz] = 0;
    propertyBuf[sz + 1] = 0;
    if (status != ERROR_SUCCESS) {
      MMLogError(L"Table %s failed to get property with error %d", tableName, status);
      return FALSE;
    }
    return TRUE;
  }
}


BOOL CreateTable(PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int *rowCounter) {
  auto stmt = tables.find(tableName);
  if (stmt != tables.end()) {
    return TRUE;
  }

  size_t bufSize = info->TopLevelPropertyCount * 24 + 256; // TODO: LAZY!
  char *buf = new char[bufSize], *stmtbuf = new char[bufSize], *p = buf;
  // TODO: free memory

  sprintf_s(buf, bufSize, "CREATE TABLE IF NOT EXISTS \"%ws\" (row INTEGER", tableName);
  sprintf_s(stmtbuf, bufSize, "INSERT INTO \"%ws\" VALUES (?", tableName);

  for (ULONG i = 0; i < pInfo->TopLevelPropertyCount; i++) {
    PWSTR propertyName;
    if (!GetEventProperties(tableName, event, info, i, propertyName)) return FALSE;

    PWCHAR pdot = wcsstr(propertyName, L".Length");
    if (pdot) {
      // We assume this is a property for the length of the next field
      // We'll use the field but we won't be inserting into the database
      continue;
    }

    p = strchr(buf, 0);
    sprintf_s(p, bufSize - (p - buf), ", %ws", propertyName);

    strcat_s(stmtbuf, bufSize, ", ?");

    switch (pInfo->EventPropertyInfoArray[i].nonStructType.InType) {
    case TDH_INTYPE_UNICODESTRING:
      strcat_s(buf, bufSize, " TEXT");
      break;
    case TDH_INTYPE_INT16:
    case TDH_INTYPE_UINT16:
    case TDH_INTYPE_INT32:
    case TDH_INTYPE_UINT32:
    case TDH_INTYPE_INT64:
    case TDH_INTYPE_UINT64:
      strcat_s(buf, bufSize, " INTEGER");
      break;
    case TDH_INTYPE_BINARY:
      strcat_s(buf, bufSize, " BLOB");
      break;
    default:
      MMLogError(L"Table has a property %s with unexpected type %d", propertyName, pInfo->EventPropertyInfoArray[i].nonStructType.InType);
      return FALSE;
    }
  }

  strcat_s(buf, bufSize, ")");
  strcat_s(stmtbuf, bufSize, ")");

  // create the table (we'll do this outside a transaction so it's available instantly to the consumer)
  if (!Check(sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL))) return FALSE;
  if (!Check(sqlite3_exec(db, buf, NULL, NULL, NULL))) return FALSE;

  sprintf_s(buf, bufSize, "CREATE UNIQUE INDEX IF NOT EXISTS ix_%ws_row ON \"%ws\" (row)", tableName, tableName);
  if (!Check(sqlite3_exec(db, buf, NULL, NULL, NULL))) return FALSE;

  if (!Check(sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL))) return FALSE;

  char rowstmtbuf[256];
  sprintf_s(rowstmtbuf, "SELECT COALESCE(MAX(row)+1,0) FROM \"%ws\"", tableName);
  sqlite3_stmt *stmt_get_row;
  if (!Check(sqlite3_prepare_v2(db, rowstmtbuf, -1, &stmt_get_row, NULL))) return FALSE;

  if (sqlite3_step(stmt_get_row) == SQLITE_ROW) {
    *rowCounter = sqlite3_column_int(stmt_get_row, 0);
  }
  else {
    *rowCounter = 0;
  }

  sqlite3_stmt *new_stmt;
  if (!Check(sqlite3_prepare_v2(db, stmtbuf, -1, &new_stmt, NULL))) return FALSE;
  tables[tableName] = new_stmt;
  return TRUE;
}

BOOL InsertRecord(int index, PWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info) {
  auto stmt = tables.find(tableName);
  if (stmt == tables.end()) {
    return FALSE;
  }

  // TODO: Add stack data
  
  sqlite3_bind_int(stmt->second, 1, index);

  for (ULONG i = 0, col = 2; i < pInfo->TopLevelPropertyCount; i++) {
    PWSTR propertyName;
    if (!GetEventProperties(tableName, event, info, i, propertyName)) return FALSE;

    PWCHAR pdot = wcsstr(propertyName, L".Length");
    if (pdot) {
      // We assume this is a property for the length of the next field
      // We'll use the field but we won't be inserting into the database
      currentPropertyBufSize = *(int *)propertyBuf;
      continue;
    }

    switch (pInfo->EventPropertyInfoArray[i].nonStructType.InType) {
    case TDH_INTYPE_UNICODESTRING:
      if (!Check(sqlite3_bind_text16(stmt->second, col, propertyBuf, -1, NULL))) return FALSE;
      break;
    case TDH_INTYPE_INT16:
    case TDH_INTYPE_UINT16:
      if (!Check(sqlite3_bind_int(stmt->second, col, *(int16_t *)propertyBuf))) return FALSE;
      break;
    case TDH_INTYPE_INT32:
    case TDH_INTYPE_UINT32:
      if (!Check(sqlite3_bind_int(stmt->second, col, *(int *)propertyBuf))) return FALSE;
      break;
    case TDH_INTYPE_INT64:
    case TDH_INTYPE_UINT64:
      if (!Check(sqlite3_bind_int64(stmt->second, col, *(sqlite3_int64 *)propertyBuf))) return FALSE;
      break;
    case TDH_INTYPE_BINARY:
      if(currentPropertyBufSize > 0)
        if (!Check(sqlite3_bind_blob(stmt->second, col, propertyBuf, currentPropertyBufSize, SQLITE_STATIC))) return FALSE;
      break;
    default:
      MMLogError(L"Table has a property %s with unexpected type %d", propertyName, pInfo->EventPropertyInfoArray[i].nonStructType.InType);
      return FALSE;
    }

    col++;
  }

  int sstatus = sqlite3_step(stmt->second);
  if (sstatus != SQLITE_DONE) {
    MMLogError(L"Table %s [%d] failed to insert with code %d", tableName, index, sstatus);
    return FALSE;
  }
  if (!Check(sqlite3_reset(stmt->second))) {
    //std::wcout << "Table " << tableName << "[" << index << "] failed to reset with code " << status << std::endl;
    return FALSE;
  }
  return TRUE;
}


BOOL FileExists(PCTSTR szPath)
{
  // https://devblogs.microsoft.com/oldnewthing/?p=24713
  // https://stackoverflow.com/a/6218957/1836776

  DWORD dwAttrib = GetFileAttributes(szPath);

  return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
    !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
}

BOOL CreateDatabase(PCTSTR szPath, BOOL fOverwrite) {
  if (FileExists(szPath)) {
    if (fOverwrite) {
      if (!DeleteFile(szPath)) {
        return FALSE;
      }
    }
  }

  if (!Check(sqlite3_open16(szPath, &db))) return FALSE;
  if (!Check(sqlite3_exec(db, "PRAGMA ENCODING=utf8;", NULL, NULL, NULL))) return FALSE;
  if (!BeginTransaction()) return FALSE;
  return TRUE;
}

BOOL CommitTransaction() {
  return Check(sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL));
}

BOOL BeginTransaction() {
  return Check(sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL));
}

BOOL FreeStatements() {
  for (auto element : tables) {
    sqlite3_finalize(element.second);
    element.second = NULL;
  }
  tables.clear();
  return TRUE;
}

BOOL CloseDatabase() {
  sqlite3_close(db);
  return TRUE;
}
