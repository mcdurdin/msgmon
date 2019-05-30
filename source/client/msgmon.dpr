program msgmon;

uses
  Vcl.Forms,
  MsgMon.UI.MainForm in 'MsgMon.UI.MainForm.pas' {Form1},
  MsgMon.System.ExecProcess in 'MsgMon.System.ExecProcess.pas',
  MsgMon.System.Data.Message in 'MsgMon.System.Data.Message.pas',
  MsgMon.System.Data.Window in 'MsgMon.System.Data.Window.pas',
  MsgMon.System.Data.Process in 'MsgMon.System.Data.Process.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
