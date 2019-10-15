#pragma once

#define __FILENAME__ (strrchr("\\" __FILE__, '\\') + 1)

#define MMLogError(message,...)   MMLog(2, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define MMLogWarning(message,...) if(MMLogging()) MMLog(1, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define MMLogInfo(message,...)    if(MMLogging()) MMLog(0, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define MMShowInfo(message,...)   MMLog(0, __FILENAME__, __LINE__, (message), __VA_ARGS__)
void MMLog(int level, const char *file, int line, const wchar_t *message, ...);
inline BOOL MMLogging();


void hexStr2(PBYTE data, int len, wchar_t *out); // TODO: into own shared file