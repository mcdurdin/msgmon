unit MsgMon.System.Data.Process;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Thread;

const
  PLATFORM_X86 = 1;
  PLATFORM_X64 = 2;

type
  TMMProcess = class(TMMEvent)
    platform_: DWORD;
    pidOwner: DWORD;
    processPath, commandLine: string;
    processName: string;
  private
//    FThreads: TMMThreads;
  public
    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;
      const stack: string;

      pidOwner: Integer; // TODO: rename or eliminate
      platform_: DWORD;
      const processPath,
      commandLine: string);
    destructor Destroy; override;
//    property Threads: TMMThreads read FThreads;
    function Render(IncludePID: Boolean): string;
  end;

  TMMProcesses = class(TObjectList<TMMProcess>)
  end;

  TMMProcessDictionary = class(TObjectDictionary<DWORD,TMMProcess>)
  end;

implementation

{ TMsgMonProcess }

constructor TMMProcess.Create(
  timestamp: Int64;
  pid,
  tid: Integer;
  event_id: Int64;
  const stack: string;

  pidOwner: Integer;
  platform_: DWORD;
  const processPath,
  commandLine: string);
begin
  inherited Create(timestamp, pid, tid, event_id, stack);

//  FThreads := TMMThreads.Create;

  Self.pidOwner := pidOwner;
  Self.platform_ := platform_;
  Self.processPath := processPath;
  Self.commandLine := commandLine;

  processName := ExtractFileName(processPath);
end;

destructor TMMProcess.Destroy;
begin
//  FreeAndNil(FThreads);
  inherited Destroy;
end;

function TMMProcess.Render(IncludePID: Boolean): string;
begin
  Result := processName;

  if IncludePID then
    Result := Result + ' ['+IntToStr(pid)+']';
end;

end.

