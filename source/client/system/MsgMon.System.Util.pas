unit MsgMon.System.Util;

interface

type
  TMMUtil = class
    class procedure FlushLibrary;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Messages,
  Winapi.Tlhelp32,
  Winapi.Windows;

class procedure TMMUtil.FlushLibrary;
var
  h: THandle;
  te: TThreadEntry32;
  hd, hdcurrent: HDESK;
begin
  // A better way of doing may be to make each loaded process have a thread
  // waiting on this process handle. Then after the process exits, the thread
  // does a PostThreadMessage and then FreeLibraryAndExitThread
  // e.g. https://stackoverflow.com/a/25597741/1836776

  hdcurrent := GetThreadDesktop(GetCurrentThreadID);

  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    FillChar(te, sizeof(te), 0);
    te.dwSize := sizeof(te);
    if Thread32First(h, te) then
    begin
      repeat
        if te.dwSize >= 12 then // see https://devblogs.microsoft.com/oldnewthing/20060223-14/?p=32173
        begin
          hd := GetThreadDesktop(te.th32ThreadID);
          if (hd <> 0) and (hd = hdcurrent) then
          begin
            if not PostThreadMessage(te.th32ThreadID, WM_NULL, 0, 0) then
            begin
              OutputDebugString(Pchar(format('Unable to post to %d::%d -- (%d) %s', [te.th32OwnerProcessID, te.th32ThreadID, GetLastError, SysErrorMessage(GetLastError)])));
            end;
          end;
        end;
      until not Thread32Next(h, te);
    end;
    CloseHandle(h);
  end;
end;

end.
