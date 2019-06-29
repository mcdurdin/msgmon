unit MsgMon.System.ExecConsoleProcess;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TRunConsoleApp = class(TThread)
  private
    FResult: Boolean;
    FCommandLine: string;
    FCurrentDirectory: string;
    FLogText: string;
    FExitCode: Integer;
    FLogFile: string;
    FLastError: Integer;
    FApp: string;
  protected
    procedure Execute; override;
  public
    class function Run(App, CommandLine, CurrentDirectory, LogFile: string): TRunConsoleApp;
    property RunResult: Boolean read FResult;
    property App: string read FApp write FApp;
    property CommandLine: string read FCommandLine write FCommandLine;
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property LogText: string read FLogText;
    property LogFile: string read FLogFile write FLogFile;
    property ExitCode: Integer read FExitCode;
    property LastError: Integer read FLastError;
  end;

implementation

uses
  MsgMon.System.ExecProcess;

{ TRunConsoleApp }

procedure TRunConsoleApp.Execute;
var
  str: TStringList;
begin
  FResult := TExecProcess.Console(FCommandLine, FCurrentDirectory, FLogText, FExitCode);

  if not FResult then
    FLastError := GetLastError;

  if FLogFile <> '' then
  begin
    str := TStringList.Create;
    try
      str.Text := FLogText;
      str.Insert(0, 'Command Line: '+FCommandLine);
      str.Insert(1, 'Current Directory: '+FCurrentDirectory);
      str.Insert(1, 'Result: '+BoolToStr(FResult, True));
      str.Insert(2, 'Exit Code: '+IntToStr(FExitCode));
      str.Insert(3, '');
      str.SaveToFile(FLogFile, TEncoding.UTF8);
    finally
      str.Free;
    end;
  end;
end;

class function TRunConsoleApp.Run(App, CommandLine, CurrentDirectory,
  LogFile: string): TRunConsoleApp;
var
  r: TRunConsoleApp;
begin
  r := TRunConsoleApp.Create(True);
  r.App := App;
  r.CommandLine := CommandLine;
  r.CurrentDirectory := CurrentDirectory;
  r.LogFile := LogFile;
  r.Start;
  Result := r;
end;

end.
