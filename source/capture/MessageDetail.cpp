
#include "stdafx.h"

void FlagToFlagNames(DWORD flags, const PWCHAR *ppszFlagNames, int nFlagNames, PWCHAR pszBuf, size_t nBuf) {
	//TODO: buffer management

	wsprintf(pszBuf, L"%8.8x", flags);
	pszBuf = wcschr(pszBuf, 0);

	if (flags == 0) {
		return;
	}

	wcscpy(pszBuf, L" {");
	pszBuf = wcschr(pszBuf, 0);

	for (int i = 0, n = 1; i < nFlagNames; i++, n <<= 1) {
		if (flags & n) {
			if (nBuf > wcslen(ppszFlagNames[i]) + 2) {
				wcscpy(pszBuf, ppszFlagNames[i]);
				wcscat(pszBuf, L" ");
				pszBuf = wcschr(pszBuf, 0);
			}
		}
	}

	wcscpy(pszBuf, L"}");
}

BOOL Detail_WindowPosChanged(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PBYTE pbDetail, int *nDetail) {
  if (sizeof(WINDOWPOS) > MAX_DETAIL_BUFFER_SIZE) return FALSE;

  PWINDOWPOS wp = PWINDOWPOS(lParam);

  memcpy(pbDetail, wp, sizeof(WINDOWPOS));
  switch ((int) wp->hwndInsertAfter) {
    case HWND_NOTOPMOST:
    case HWND_TOP:
    case HWND_BOTTOM:
    case HWND_TOPMOST:
      break;
    default:
      LogWindow(wp->hwndInsertAfter);
  }
  LogWindow(wp->hwnd);
  *nDetail = sizeof(WINDOWPOS);
  return TRUE;
}

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PBYTE pbDetail, int *nDetail) {
	switch (message) {
	case WM_WINDOWPOSCHANGING:
	case WM_WINDOWPOSCHANGED:
		return Detail_WindowPosChanged(hwnd, message, wParam, lParam, lResult, pbDetail, nDetail);
	}

	return FALSE;
}
