// Pulled from https://github.com/keymanapp/keyman/ utilexecute.pas

unit MsgMon.System.ExecProcess;

interface

uses
  WinApi.ShellApi,
  WinApi.Windows;

type
  TExecProcess = class sealed
    type
      TCallbackEvent = procedure(var Cancelled: Boolean) of object;
      TCallbackWaitEvent = procedure(hProcess: THandle; var Waiting, Cancelled: Boolean) of object;
    class function Console(const cmdline, curdir: string; var FLogText: string; var PID, ExitCode: Integer; FOnCallback: TCallbackEvent = nil): Boolean; overload; static;   // I3631
    class function Console(const cmdline, curdir: string; var FLogText: string; FOnCallback: TCallbackEvent = nil): Boolean; overload; static;   // I3631
    class function WaitForProcess(const cmdline, curdir: string; ShowWindow: Integer = SW_SHOWNORMAL; FOnCallback: TCallbackWaitEvent = nil): Boolean; overload; static;
    class function WaitForProcess(const cmdline, curdir: string; var EC: Cardinal; ShowWindow: Integer = SW_SHOWNORMAL; FOnCallback: TCallbackWaitEvent = nil): Boolean; overload; static;
    class function Shell(Handle: HWND; const process, curdir: string; const parameters: string = ''; ShowWindow: Integer = SW_SHOWNORMAL; const Verb: string = 'open'): Boolean; static;
    class function URL(const url: string): Boolean;
  end;

implementation

uses
  System.SysUtils;

class function TExecProcess.Console(const cmdline, curdir: string; var FLogText: string; FOnCallback: TCallbackEvent = nil): Boolean;
var
  pid, ec: Integer;
begin
  Result := TExecProcess.Console(cmdline, curdir, FLogText, pid, ec, FOnCallback);   // I3631
end;

class function TExecProcess.Console(const cmdline, curdir: string; var FLogText: string; var PID, ExitCode: Integer; FOnCallback: TCallbackEvent = nil): Boolean;   // I3631
var
  si: TStartupInfo;
  b, ec: DWord;
  cmdlinebuf: string;
  buf: array[0..512] of ansichar;  // I3310
  SecAttrs: TSecurityAttributes;
  hsoutread, hsoutwrite: THandle;
  hsinread, hsinwrite: THandle;
  pi: TProcessInformation;
  n: Integer;
  FCancelled: Boolean;
begin
  Result := False;
  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsoutread, hsoutwrite, @SecAttrs, 0) then Exit;

  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsinread, hsinwrite, @SecAttrs, 0) then
  begin
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    Exit;
  end;

  { See support.microsoft.com kb 190351 }

  PID := 0;
  n := 0;
  FLogText := '';

  SetLength(cmdlinebuf, Length(cmdline));   // I4170
  StrCopy(PChar(cmdlinebuf), PChar(cmdline));   // I4170

  //Sreen.Cursor := crHourglass;
  try
    si.cb := SizeOf(TStartupInfo);
    si.lpReserved := nil;
    si.lpDesktop := nil;
    si.lpTitle := nil;
    si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    si.wShowWindow := SW_HIDE;
    si.cbReserved2 := 0;
    si.lpReserved2 := nil;
    si.hStdInput := hsinread;
    si.hStdOutput := hsoutwrite;
    si.hStdError := hsoutwrite;

    if CreateProcess(nil, PChar(cmdlinebuf),   // I4170
      nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PChar(curdir), si, pi) then
    try
      PID := pi.dwProcessId;

      if GetExitCodeProcess(pi.hProcess, ec) then
      begin
        while ec = STILL_ACTIVE do
        begin
          Sleep(20);
          Inc(n);
          if n = 30 then
          begin
            if Assigned(FOnCallback) then
            begin
              FCancelled := False;
              FOnCallback(FCancelled);
              if FCancelled then
              begin
                SetLastError(ERROR_CANCELLED);
                Exit;
              end;
            end;
            n := 0;
          end;
          PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
          if b > 0 then
          begin
            ReadFile(hsoutread, buf, High(buf), b, nil);
            FLogText := FLogText + String(Copy(buf, 1, b));
          end;
          if not GetExitCodeProcess(pi.hProcess, ec) then ec := 0;
        end;
      end;

      repeat
        PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
        if b > 0 then
        begin
          ReadFile(hsoutread, buf, High(buf), b, nil);
          FLogText := FLogText + String(Copy(buf, 1, b));
        end;
      until b = 0;

      ExitCode := ec;   // I3631

      Result := True;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    CloseHandle(hsinread);
    CloseHandle(hsinwrite);
    //Screen.Cursor := crDefault;
  end;
end;

class function TExecProcess.Shell(Handle: HWND; const process, curdir, parameters: string;
  ShowWindow: Integer; const Verb: string): Boolean;
begin
  Result := ShellExecute(Handle, PChar(Verb), Pchar(process), PChar(parameters), Pchar(curdir), ShowWindow) > 32;
end;

class function TExecProcess.URL(const url: string): Boolean;
begin
  Result := ShellExecute(GetActiveWindow, nil, PChar(url), nil, nil, SW_SHOW) >= 32;
end;

class function TExecProcess.WaitForProcess(const cmdline, curdir: string; ShowWindow: Integer; FOnCallback: TCallbackWaitEvent): Boolean;
var
  ec: Cardinal;
begin
  Result := WaitForProcess(cmdline, curdir, ec, ShowWindow, FOnCallback);
end;

class function TExecProcess.WaitForProcess(const cmdline, curdir: string; var EC: Cardinal; ShowWindow: Integer; FOnCallback: TCallbackWaitEvent): Boolean;
var
  Waiting: Boolean;
  si: TStartupInfoW;
  pi: TProcessInformation;
  Cancelled: Boolean;
  buf: PChar;
begin
  Result := False;

  Waiting := True;
  si.cb := SizeOf(TStartupInfo);
  si.lpReserved := nil;
  si.lpDesktop := nil;
  si.lpTitle := nil;
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  si.cbReserved2 := 0;
  si.lpReserved2 := nil;

  EC := $FFFFFFFF;

  buf := AllocMem((Length(cmdline)+1)*sizeof(Char));   // I4983
  try
    StrPCopy(buf, cmdline);   // I4983
    if CreateProcess(nil, buf,   // I4983
            nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PWideChar(curdir), si, pi) then
    try
      while Waiting do
      begin
        if Assigned(FOnCallback) then
        begin
          Cancelled := False;
          FOnCallback(pi.hProcess, Waiting, Cancelled);
          if Cancelled then
          begin
            SetLastError(ERROR_CANCELLED);
            Exit;
          end;
        end
        else
        begin
          WaitForSingleObject(pi.hProcess, INFINITE);
          Waiting := False;
        end;
      end;

      if not GetExitCodeProcess(pi.hProcess, EC) then
        EC := $FFFFFFFF;

      Result := True;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    FreeMem(buf);   // I4983
  end;
end;

end.
