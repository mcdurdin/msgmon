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
  SQLite3 in '..\ext\sqlite\delphi\Source\SQLite3.pas',
  SQLite3Utils in '..\ext\sqlite\delphi\Source\SQLite3Utils.pas',
  SQLite3Wrap in '..\ext\sqlite\delphi\Source\SQLite3Wrap.pas',
  MsgMon.Data.Database in 'MsgMon.Data.Database.pas',
  MsgMon.System.ExecConsoleProcess in 'MsgMon.System.ExecConsoleProcess.pas',
  MsgMon.System.Util in 'MsgMon.System.Util.pas',
  MsgMon.System.Data.MessageDetail in 'MsgMon.System.Data.MessageDetail.pas',
  MsgMon.UI.DisplayColumnForm in 'MsgMon.UI.DisplayColumnForm.pas' {MMDisplayColumnsForm},
  MsgMon.System.Data.VKeyNames in 'MsgMon.System.Data.VKeyNames.pas',
  MsgMon.UI.WindowTreeFrame in 'MsgMon.UI.WindowTreeFrame.pas' {MMWindowTreeFrame},
  MsgMon.System.Data.Thread in 'MsgMon.System.Data.Thread.pas',
  MsgMon.UI.DetailRenderToGrid in 'MsgMon.UI.DetailRenderToGrid.pas',
  Vcl.Themes,
  Vcl.Styles,
  MsgMon.System.Data.Search in 'MsgMon.System.Data.Search.pas',
  MsgMon.System.Data.Event in 'MsgMon.System.Data.Event.pas',
  MsgMon.System.ContextViewTypes in 'MsgMon.System.ContextViewTypes.pas',
  MsgMon.UI.ProgressForm in 'MsgMon.UI.ProgressForm.pas' {MMProgressForm},
  MsgMon.System.ProgressManager in 'MsgMon.System.ProgressManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMMMainForm, MMMainForm);
  Application.Run;
end.
