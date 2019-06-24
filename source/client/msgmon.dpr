program msgmon;

uses
  Vcl.Forms,
  MsgMon.UI.MainForm in 'MsgMon.UI.MainForm.pas' {MMMainForm},
  MsgMon.System.ExecProcess in 'MsgMon.System.ExecProcess.pas',
  MsgMon.System.Data.Message in 'MsgMon.System.Data.Message.pas',
  MsgMon.System.Data.Window in 'MsgMon.System.Data.Window.pas',
  MsgMon.System.Data.Process in 'MsgMon.System.Data.Process.pas',
  MsgMon.System.Data.MessageName in 'MsgMon.System.Data.MessageName.pas',
  MsgMon.System.Data.Column in 'MsgMon.System.Data.Column.pas',
  MsgMon.System.Data.Context in 'MsgMon.System.Data.Context.pas',
  MsgMon.UI.FilterForm in 'MsgMon.UI.FilterForm.pas' {MMFilterForm},
  MsgMon.System.Data.Filter in 'MsgMon.System.Data.Filter.pas',
  MsgMon.System.Data.Session in 'MsgMon.System.Data.Session.pas',
  SQLite3 in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3.pas',
  SQLite3Utils in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3Utils.pas',
  SQLite3Wrap in '..\..\tools\sqlite\SQLite3-Delphi-FPC-master\SQLite3-Delphi-FPC-master\Source\SQLite3Wrap.pas',
  MsgMon.Data.Database in 'MsgMon.Data.Database.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
//  TStyleManager.TrySetStyle('Windows10');
  Application.CreateForm(TMMMainForm, MMMainForm);
  Application.Run;
end.
