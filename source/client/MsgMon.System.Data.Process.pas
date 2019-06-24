unit MsgMon.System.Data.Process;

interface

uses
  System.Generics.Collections,
  Winapi.Windows;

type
  TMMProcess = class
    base: Integer;
    pid: DWORD;
    platform_: DWORD;
    processPath, commandLine: string;
    processName: string;
    constructor Create(pid, platform_: DWORD; const processPath, commandLine: string; ABase: Integer);
  end;

  TMMProcesses = class(TObjectList<TMMProcess>)
    function FromBase(ABase: Integer): TMMProcess;
  end;

  TMMProcessDictionary = class(TObjectDictionary<DWORD,TMMProcesses>)
  end;

implementation

uses
  System.SysUtils;

{ TMsgMonProcess }

constructor TMMProcess.Create(pid, platform_: DWORD; const processPath, commandLine: string; ABase: Integer);
var
  name, value: string;
  valueInt: Int64;
begin
  inherited Create;

  Self.pid := pid;
  Self.platform_ := platform_;
  Self.processPath := processPath;
  Self.commandLine := commandLine;
  base := ABase;

  processName := ExtractFileName(processPath);
end;

{ TMsgMonProcesses }

function TMMProcesses.FromBase(ABase: Integer): TMMProcess;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].base <= ABase then
      Exit(Items[i]);
  if Count = 0 then
    Exit(nil);
  Result := Items[0];
end;

end.

