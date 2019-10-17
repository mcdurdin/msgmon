program msgmon;

uses
  Vcl.Forms,
  MsgMon.UI.MainForm in 'ui\MsgMon.UI.MainForm.pas' {MMMainForm},
  MsgMon.System.ExecProcess in 'system\MsgMon.System.ExecProcess.pas',
  MsgMon.System.Data.Message in 'data\MsgMon.System.Data.Message.pas',
  MsgMon.System.Data.Window in 'data\MsgMon.System.Data.Window.pas',
  MsgMon.System.Data.Process in 'data\MsgMon.System.Data.Process.pas',
  MsgMon.System.Data.MessageName in 'data\MsgMon.System.Data.MessageName.pas',
  MsgMon.System.Data.Column in 'data\MsgMon.System.Data.Column.pas',
  MsgMon.System.Data.Context in 'data\MsgMon.System.Data.Context.pas',
  MsgMon.UI.FilterForm in 'ui\MsgMon.UI.FilterForm.pas' {MMFilterForm},
  MsgMon.System.Data.Filter in 'data\MsgMon.System.Data.Filter.pas',
  MsgMon.System.Data.Session in 'data\MsgMon.System.Data.Session.pas',
  SQLite3 in '..\ext\sqlite\delphi\Source\SQLite3.pas',
  SQLite3Utils in '..\ext\sqlite\delphi\Source\SQLite3Utils.pas',
  SQLite3Wrap in '..\ext\sqlite\delphi\Source\SQLite3Wrap.pas',
  MsgMon.Data.Database in 'data\MsgMon.Data.Database.pas',
  MsgMon.System.ExecConsoleProcess in 'system\MsgMon.System.ExecConsoleProcess.pas',
  MsgMon.System.Util in 'system\MsgMon.System.Util.pas',
  MsgMon.System.Data.MessageDetail in 'data\MsgMon.System.Data.MessageDetail.pas',
  MsgMon.UI.DisplayColumnForm in 'ui\MsgMon.UI.DisplayColumnForm.pas' {MMDisplayColumnsForm},
  MsgMon.System.Data.VKeyNames in 'data\MsgMon.System.Data.VKeyNames.pas',
  MsgMon.UI.WindowTreeFrame in 'ui\MsgMon.UI.WindowTreeFrame.pas' {MMWindowTreeFrame},
  MsgMon.System.Data.Thread in 'data\MsgMon.System.Data.Thread.pas',
  MsgMon.UI.DetailRenderToGrid in 'ui\MsgMon.UI.DetailRenderToGrid.pas',
  Vcl.Themes,
  Vcl.Styles,
  MsgMon.System.Data.Search in 'data\MsgMon.System.Data.Search.pas',
  MsgMon.System.Data.Event in 'data\MsgMon.System.Data.Event.pas',
  MsgMon.System.ContextViewTypes in 'system\MsgMon.System.ContextViewTypes.pas',
  MsgMon.UI.ProgressForm in 'ui\MsgMon.UI.ProgressForm.pas' {MMProgressForm},
  MsgMon.System.ProgressManager in 'system\MsgMon.System.ProgressManager.pas',
  MsgMon.System.Data.Image in 'data\MsgMon.System.Data.Image.pas',
  MsgMon.System.StackRenderer in 'system\MsgMon.System.StackRenderer.pas',
  MsgMon.UI.StackViewFrame in 'ui\MsgMon.UI.StackViewFrame.pas' {MMStackViewFrame},
  MsgMon.Win.DbgHelp in 'system\MsgMon.Win.DbgHelp.pas',
  MsgMon.System.Settings in 'system\MsgMon.System.Settings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMMMainForm, MMMainForm);
  Application.Run;
end.
