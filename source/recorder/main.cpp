// msgmon.recorder.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"

#include <initguid.h>
#include "recorder.h"

enum cmdline_action { ACTION_UNKNOWN, ACTION_USAGE, ACTION_CAPTURE, ACTION_STORE };
enum cmdline_mode { MODE_UNKNOWN, MODE_FROMFILE, MODE_REALTIME };
struct {
  wchar_t *logfile, *database;
  BOOL overwrite;
  cmdline_mode mode;
  cmdline_action action;
  wchar_t *eventName;
} cmdline = { NULL };

BOOL ParseCommandLine(int argc, wchar_t *argv[]);
void PrintUsage();

int wmain(int argc, wchar_t *argv[])
{
  if (!ParseCommandLine(argc, argv)) {
    puts("Unknown parameter.");
    PrintUsage();
    return 1;
  }

  switch (cmdline.action) {
  case ACTION_USAGE:
    PrintUsage();
    return 0; // not an error
  case ACTION_CAPTURE:
    if (cmdline.eventName == NULL) {
      puts("Event name is required.");
      return 3;
    }
    return Capture(cmdline.eventName, cmdline.logfile, cmdline.overwrite) ? 0 : 1;
#ifndef _WIN64
  case ACTION_STORE:
    return Store(cmdline.logfile, cmdline.database, cmdline.overwrite) ? 0 : 1;
#endif
  }

  return 2; // unknown action
}

BOOL ParseCommandLine(int argc, wchar_t *argv[]) {
  int i = 1;
  cmdline.action = ACTION_UNKNOWN;

  while (i < argc) {
    if (i < argc - 1 && (_wcsicmp(argv[i], L"-l") == 0 || _wcsicmp(argv[i], L"--logfile") == 0)) {
      cmdline.logfile = argv[i + 1];
      cmdline.mode = MODE_FROMFILE;
      i += 2;
    }
    else if (i < argc - 1 && (_wcsicmp(argv[i], L"-d") == 0 || _wcsicmp(argv[i], L"--database") == 0)) {
      cmdline.database = argv[i + 1];
      i += 2;
    }
    else if (_wcsicmp(argv[i], L"-f") == 0 || _wcsicmp(argv[i], L"--overwrite") == 0) {
      cmdline.overwrite = TRUE;
      i++;
    }
    else if (_wcsicmp(argv[i], L"-?") == 0 || _wcsicmp(argv[i], L"--help") == 0) {
      cmdline.action = ACTION_USAGE;
      return TRUE;
    }
    else if (i < argc - 1 && (_wcsicmp(argv[i], L"-e") == 0 || _wcsicmp(argv[i], L"--event") == 0)) {
      cmdline.eventName = argv[i + 1]; // Handle = (HANDLE)(INT64)_wtoi(argv[i + 1]);
      i += 2;
    }
    else if(_wcsicmp(argv[i], L"capture") == 0){
      cmdline.action = ACTION_CAPTURE;
      i++;
    }
    else if (_wcsicmp(argv[i], L"store") == 0) {
      cmdline.action = ACTION_STORE;
      i++;
    }
    else {
      return FALSE;
    }
  }
  return cmdline.action != ACTION_UNKNOWN;
}

void PrintUsage() {
#ifdef _WIN64
  puts("Usage: msgmon.recorder.x64 capture [-e handle] [-?]");
  puts("");
  puts("  capture: Runs an event trace, writing to trace created by 32-bit host");
  puts("  -e, --event        specifies an event handle that can be set to finish the trace");
  puts("  -?, --help         Print usage");
#else
  puts("Usage: msgmon.recorder.x86 capture|store [-e handle] [-d databasePath] [-l logFilePath] [-f] [-?]");
  puts("");
  puts("  capture: Runs an event trace, writing either realtime or to log file");
  puts("  -e, --event        specifies an event handle that can be set to finish the trace");
  puts("  -l, --logfile      Output .etl event log file; if not specified, writes to a realtime session");

  puts("");
  puts("  store: Takes a event log trace and converts it to .db format for use with msgmon");
  puts("  -d, --database     Output .db database file, required");
  puts("  -l, --logfile      .etl event log file; if not specified, reads from current realtime session until session completes");

  puts("");
  puts("  Common parameters:");
  puts("  -?, --help         Print usage");
  puts("  -f, --overwrite    Deletes existing output file; if not specified, appends to it");
#endif
}
