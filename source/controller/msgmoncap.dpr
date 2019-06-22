program msgmoncap;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  mm in 'mm.pas',
  SQLite3 in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3.pas',
  SQLite3Utils in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3Utils.pas',
  SQLite3Wrap in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3Wrap.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
