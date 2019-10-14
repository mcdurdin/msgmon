#include <windows.h>
#include <stdio.h>
#include <stdarg.h>
#include "./mmlog.h"

void MMLog(int level, const char *file, int line, const wchar_t *message, ...)
{
  wchar_t fmtbuf[256], outbuf[300];

  if (level < 0 || level > 2) level = 0;
  const wchar_t *state[] = { L"Info", L"Warning", L"Error" };

  va_list vars;
  va_start(vars, message);
  _vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, message, vars);  // I2248   // I3547
  fmtbuf[255] = 0;

  wsprintfW(outbuf, L"[MMLog%s(%S:%d)] %s\n", state[level], file, line, fmtbuf);

  if (MMLogging()) {
    OutputDebugString(outbuf);
  }

#ifndef _USRDLL
  outbuf[wcslen(outbuf) - 1] = 0; // remove final \n
  _putws(outbuf);
#endif
}

inline BOOL MMLogging() {
//#ifdef _DEBUG
  return TRUE;
//#else
//  return FALSE;
//#endif
}