#pragma once

#define SESSION_NAME TEXT("MsgMon_Session")

// Define the GUID to use in TraceLoggingProviderRegister 
// {082E6CC6-239C-4B96-9475-159AA241B4AB}
DEFINE_GUID(
  g_Provider,
  0x082e6cc6, 0x239c, 0x4b96, 0x94, 0x75, 0x15, 0x9a, 0xa2, 0x41, 0xb4, 0xab);


BOOL Store(wchar_t *logfile, wchar_t *database, BOOL overwrite);
BOOL Capture(wchar_t *eventName, wchar_t *logfile, BOOL overwrite, BOOL x86only);
