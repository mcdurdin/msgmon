unit MsgMon.System.Data.Process;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  MsgMon.System.Data.Thread;

const
  PLATFORM_X86 = 1;
  PLATFORM_X64 = 2;

type
  TMMProcess = class
    base: Integer;
    pid: DWORD;
    platform_: DWORD;
    processPath, commandLine: string;
    processName: string;
  private
    FThreads: TMMThreads;
  public
    constructor Create(pid, platform_: DWORD; const processPath, commandLine: string; ABase: Integer);
    destructor Destroy; override;
    property Threads: TMMThreads read FThreads;
    function Render(IncludePID: Boolean): string;
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
begin
  inherited Create;

  FThreads := TMMThreads.Create;

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

destructor TMMProcess.Destroy;
begin
  FreeAndNil(FThreads);
  inherited Destroy;
end;

function TMMProcess.Render(IncludePID: Boolean): string;
begin
  Result := processName;

  if IncludePID then
    Result := Result + ' ['+IntToStr(pid)+']';
end;

end.

