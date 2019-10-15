// store_imageload.cpp : Record image load events for stack tracing
//

#include "pch.h"

BOOL InsertImageLoadRecord(PWCHAR filename, ULONG pid, ULONG checksum, ULONG timedatestamp, ULONG64 imagesize, ULONG64 imagebase);

BOOL GetWStrProperty(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PWCHAR *value, int *valueLength);
BOOL GetUInt64Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT64 value);
BOOL GetUInt32Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT32 value);

BOOL GetPointer64Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT64 value);
BOOL GetPointer32Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT32 value);


void RecordImageLoad(PEVENT_RECORD event) {
  // uint32 ImageBase;
  // uint32 ImageSize;
  // uint32 ProcessId;
  // uint32 ImageCheckSum;
  // uint32 TimeDateStamp;
  // uint32 Reserved0;
  // uint32 DefaultBase;
  // uint32 Reserved1;
  // uint32 Reserved2;
  // uint32 Reserved3;
  // uint32 Reserved4;
  // string FileName;

  if (pInfo->EventDescriptor.Opcode != 3 && pInfo->EventDescriptor.Opcode != 10) return;
//  if (wcscmp((PWSTR)((PBYTE)(pInfo)+pInfo->OpcodeNameOffset), L"DCStart") != 0) {
    // Not interested in anything other than image loads
    //return;
  //}
  
  UINT64 imagebase, imagesize;
  UINT32 imagebase32, imagesize32, pid, checksum, timedatestamp;
  PWCHAR filename;
  int filenameLength;

  if (event->EventHeader.Flags & EVENT_HEADER_FLAG_32_BIT_HEADER) {
    if (!GetPointer32Property(MMEVENTNAME_IMAGE_L, event, pInfo, 0, L"ImageBase", &imagebase32)) return;
    imagebase = (UINT64) imagebase32;
    if (!GetPointer32Property(MMEVENTNAME_IMAGE_L, event, pInfo, 1, L"ImageSize", &imagesize32)) return;
    imagesize = (UINT64) imagesize32;
  }
  else {
    if (!GetPointer64Property(MMEVENTNAME_IMAGE_L, event, pInfo, 0, L"ImageBase", &imagebase)) return;
    if (!GetPointer64Property(MMEVENTNAME_IMAGE_L, event, pInfo, 1, L"ImageSize", &imagesize)) return;
  }

  if (!GetUInt32Property(MMEVENTNAME_IMAGE_L, event, pInfo, 2, L"ProcessId", &pid)) return;
  if (!GetUInt32Property(MMEVENTNAME_IMAGE_L, event, pInfo, 3, L"ImageChecksum", &checksum)) return;
  if (!GetUInt32Property(MMEVENTNAME_IMAGE_L, event, pInfo, 4, L"TimeDateStamp", &timedatestamp)) return;
  if( !GetWStrProperty(MMEVENTNAME_IMAGE_L, event, pInfo, 13, L"FileName", &filename, &filenameLength)) return;

  if (!InsertImageLoadRecord(filename, pid, checksum, timedatestamp, imagesize, imagebase)) {
    MMLogError(L"Failed to insert record %s pid=%d", filename, pid);
  }

  delete filename;
}

BOOL CreateImageLoadTable() {
  auto stmt = tables.find(MMEVENTNAME_IMAGE_L);
  if (stmt != tables.end()) {
    return TRUE;
  }

  char buf[256], stmtbuf[256], *p = buf;

  sprintf_s(buf, "CREATE TABLE IF NOT EXISTS \"%ws\" (filename TEXT, pid INTEGER, checksum INTEGER, timedatestamp INTEGER, imagesize INTEGER, imagebase INTEGER)", MMEVENTNAME_IMAGE_L);
  sprintf_s(stmtbuf, "INSERT INTO \"%ws\" VALUES (?, ?, ?, ?, ?, ?)", MMEVENTNAME_IMAGE_L);

  // create the table (we'll do this outside a transaction so it's available instantly to the consumer)
  if (!Check(sqlite3_exec(db, "COMMIT TRANSACTION;", NULL, NULL, NULL))) return FALSE;
  if (!Check(sqlite3_exec(db, buf, NULL, NULL, NULL))) return FALSE;

  //sprintf_s(buf, "CREATE UNIQUE INDEX IF NOT EXISTS ix_%ws_event_id ON \"%ws\" (event_id)", MMEVENTNAME_EVENT_L, MMEVENTNAME_EVENT_L);
  //if (!Check(sqlite3_exec(db, buf, NULL, NULL, NULL))) return FALSE;

  if (!Check(sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL))) return FALSE;

  sqlite3_stmt *new_stmt;
  if (!Check(sqlite3_prepare_v2(db, stmtbuf, -1, &new_stmt, NULL))) return FALSE;
  tables[MMEVENTNAME_IMAGE_L] = new_stmt;
  return TRUE;
}


BOOL InsertImageLoadRecord(PWCHAR filename, ULONG pid, ULONG checksum, ULONG timedatestamp, ULONG64 imagesize, ULONG64 imagebase) {
  auto stmt = tables.find(MMEVENTNAME_IMAGE_L);
  if (stmt == tables.end()) {
    return FALSE;
  }

  // TODO: Add stack data

  sqlite3_bind_text16(stmt->second, 1, filename, -1, NULL);
  sqlite3_bind_int(stmt->second, 2, pid);
  sqlite3_bind_int(stmt->second, 3, checksum);
  sqlite3_bind_int(stmt->second, 4, timedatestamp);
  sqlite3_bind_int64(stmt->second, 5, imagesize);
  sqlite3_bind_int64(stmt->second, 6, imagebase);

  int sstatus = sqlite3_step(stmt->second);

  if (sstatus != SQLITE_DONE) {
    MMLogError(L"Table %s failed to insert with code %d", MMEVENTNAME_IMAGE_L, sstatus);
    return FALSE;
  }
  if (!Check(sqlite3_reset(stmt->second))) {
    return FALSE;
  }
  return TRUE;
}




BOOL GetUInt64Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT64 value) {
  PWSTR propertyName;

  if (!GetEventProperties(MMEVENTNAME_IMAGE_L, event, pInfo, propertyIndex, propertyName)) {
    return FALSE;
  }
  if (wcscmp(propertyName, expectedPropertyName) != 0) {
    MMLogError(L"Expected %s, got %s", expectedPropertyName, propertyName);
    return FALSE;
  }
  if (pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType != TDH_INTYPE_UINT64) {
    MMLogError(L"Expected %s to be UINT64, got %d", expectedPropertyName, pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType);
    return FALSE;
  }

  *value = *(PUINT64)propertyBuf;
  return TRUE;
}

BOOL GetUInt32Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT32 value) {
  PWSTR propertyName;

  if (!GetEventProperties(MMEVENTNAME_IMAGE_L, event, pInfo, propertyIndex, propertyName)) {
    return FALSE;
  }
  if (wcscmp(propertyName, expectedPropertyName) != 0) {
    MMLogError(L"Expected %s, got %s", expectedPropertyName, propertyName);
    return FALSE;
  }
  if (pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType != TDH_INTYPE_UINT32) {
    MMLogError(L"Expected %s to be UINT32, got %d", expectedPropertyName, pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType);
    return FALSE;
  }

  *value = *(PUINT32)propertyBuf;
  return TRUE;
}

BOOL GetPointer64Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT64 value) {
  PWSTR propertyName;

  if (!GetEventProperties(MMEVENTNAME_IMAGE_L, event, pInfo, propertyIndex, propertyName)) {
    return FALSE;
  }
  if (wcscmp(propertyName, expectedPropertyName) != 0) {
    MMLogError(L"Expected %s, got %s", expectedPropertyName, propertyName);
    return FALSE;
  }
  if (pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType != TDH_INTYPE_POINTER) {
    MMLogError(L"Expected %s to be POINTER, got %d", expectedPropertyName, pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType);
    return FALSE;
  }

  *value = *(PUINT64)propertyBuf;
  return TRUE;
}

BOOL GetPointer32Property(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PUINT32 value) {
  PWSTR propertyName;

  if (!GetEventProperties(MMEVENTNAME_IMAGE_L, event, pInfo, propertyIndex, propertyName)) {
    return FALSE;
  }
  if (wcscmp(propertyName, expectedPropertyName) != 0) {
    MMLogError(L"Expected %s, got %s", expectedPropertyName, propertyName);
    return FALSE;
  }
  if (pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType != TDH_INTYPE_POINTER) {
    MMLogError(L"Expected %s to be POINTER, got %d", expectedPropertyName, pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType);
    return FALSE;
  }

  *value = *(PUINT32)propertyBuf;
  return TRUE;
}


BOOL GetWStrProperty(PCWSTR tableName, PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PCWSTR expectedPropertyName, PWCHAR *value, int *valueLength) {
  PWSTR propertyName;
  if (!GetEventProperties(MMEVENTNAME_IMAGE_L, event, pInfo, propertyIndex, propertyName)) return FALSE;
  if (wcscmp(propertyName, expectedPropertyName) != 0) {
    MMLogError(L"Expected %s, got %s", expectedPropertyName, propertyName);
    return FALSE;
  }
  
  if (pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType != TDH_INTYPE_UNICODESTRING) {
    MMLogError(L"Expected %s to be UNICODESTRING, got %d", expectedPropertyName, pInfo->EventPropertyInfoArray[propertyIndex].nonStructType.InType);
    return FALSE;
  }
  *valueLength = wcslen((PWCHAR)propertyBuf) + 1;
  *value = new WCHAR[*valueLength];
  wcscpy_s(*value, *valueLength, (PWCHAR)propertyBuf);
  return TRUE;
}
