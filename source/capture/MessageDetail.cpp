
#include "stdafx.h"

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
      LogWindow(wp->hwndInsertAfter, FALSE);
  }
  LogWindow(wp->hwnd, FALSE);
  *nDetail = sizeof(WINDOWPOS);
  return TRUE;
}

BOOL Detail_Create(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PBYTE pbDetail, int *nDetail) {
	if (sizeof(CREATESTRUCTW) > MAX_DETAIL_BUFFER_SIZE) return FALSE;

	LPCREATESTRUCTW wp = LPCREATESTRUCTW(lParam);

	memcpy(pbDetail, wp, sizeof(CREATESTRUCTW));
	if(wp->hwndParent != NULL) LogWindow(wp->hwndParent, FALSE);
	*nDetail = sizeof(CREATESTRUCTW);
	return TRUE;
}

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PBYTE pbDetail, int *nDetail) {
	switch (message) {
	case WM_CREATE:
		return Detail_Create(hwnd, message, wParam, lParam, lResult, pbDetail, nDetail);
	case WM_WINDOWPOSCHANGING:
	case WM_WINDOWPOSCHANGED:
		return Detail_WindowPosChanged(hwnd, message, wParam, lParam, lResult, pbDetail, nDetail);
	}

	return FALSE;
}
