program msgmon.x64host;

uses
  Vcl.Forms,
  msgmon.System.x64hostMainForm in 'msgmon.System.x64hostMainForm.pas' {msgmonx64Host};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tmsgmonx64Host, msgmonx64Host);
  Application.Run;
end.
