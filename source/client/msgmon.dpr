program msgmon;

uses
  Vcl.Forms,
  MsgMon.UI.MainForm in 'MsgMon.UI.MainForm.pas' {Form1},
  MsgMon.System.ExecProcess in 'MsgMon.System.ExecProcess.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
