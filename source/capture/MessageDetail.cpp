
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

BOOL Detail_WindowPosChanged(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PWCHAR szDetail) {
	wsprintf(szDetail, L"");

	const PWCHAR pszFlagNames[] = {
		L"SWP_NOSIZE",
		L"SWP_NOMOVE",
		L"SWP_NOZORDER",
		L"SWP_NOREDRAW",
		L"SWP_NOACTIVATE",
		L"SWP_FRAMECHANGED",
		L"SWP_SHOWWINDOW",
		L"SWP_HIDEWINDOW",
		L"SWP_NOCOPYBITS",
		L"SWP_NOOWNERZORDER",
		L"SWP_NOSENDCHANGING"
	};

	PWINDOWPOS p = PWINDOWPOS(lParam);
	WCHAR szHWNDInsertAfter[48] = L"", szHWND[48] = L"";

	if (p->flags & SWP_NOZORDER) {
		wcscpy(szHWNDInsertAfter, L"N/A");
	}
	else {
		switch ((DWORD_PTR) p->hwndInsertAfter) {
		case HWND_BOTTOM:
			wcscpy(szHWNDInsertAfter, L"HWND_BOTTOM");
			break;
		case HWND_NOTOPMOST:
			wcscpy(szHWNDInsertAfter, L"HWND_NOTOPMOST");
			break;
		case HWND_TOP:
			wcscpy(szHWNDInsertAfter, L"HWND_TOP");
			break;
		case HWND_TOPMOST:
			wcscpy(szHWNDInsertAfter, L"HWND_TOPMOST");
			break;
		default:
			wsprintf(szHWNDInsertAfter, L"%x", p->hwndInsertAfter);//TODO
		}
	}

	//GetWindowReference(p->hwnd, szHWND);
	wsprintf(szHWND, L"%x", p->hwnd);//TODO

	wsprintf(szDetail, L"hwndInsertAfter=%s hwnd=%s x=%d y=%d cx=%d cy=%d flags=",
		szHWNDInsertAfter, szHWND,
		p->flags & SWP_NOMOVE ? p->x : 0,
		p->flags & SWP_NOMOVE ? p->y : 0,
		p->flags & SWP_NOSIZE ? p->cx : 0,
		p->flags & SWP_NOSIZE ? p->cy : 0);

	FlagToFlagNames(p->flags, pszFlagNames, _countof(pszFlagNames), wcschr(szDetail, 0), 256 - wcslen(szDetail));

	return TRUE;
}

BOOL GetMessageDetail(HWND hwnd, DWORD message, WPARAM wParam, LPARAM lParam, LRESULT lResult, PWCHAR szDetail) {
	switch (message) {
	case WM_WINDOWPOSCHANGING:
	case WM_WINDOWPOSCHANGED:
		return Detail_WindowPosChanged(hwnd, message, wParam, lParam, lResult, szDetail);
	}

	return FALSE;
}
