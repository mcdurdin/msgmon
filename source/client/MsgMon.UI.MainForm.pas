unit MsgMon.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
  private
{$IFNDEF WIN64}
    x64Thread: Cardinal;
{$ENDIF}
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
{$IFDEF WIN64}
  LibraryName = '..\..\..\x64\Debug\LogTraceMonitor.dll';
{$ELSE}
  LibraryName = '..\..\..\Debug\LogTraceMonitor.dll';
{$ENDIF}

function BeginLog: BOOL; stdcall; external LibraryName;
function EndLog: BOOL; stdcall; external LibraryName;

//procedure BeginLog; stdcall; external '..\..\LogTracemonitor\Win32\Debug\LogTraceMonitor.dll';
//procedure EndLog; stdcall; external '..\..\LogTracemonitor\Win32\Debug\LogTraceMonitor.dll';

procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := False;
{$IFDEF WIN64}
  if Msg.message = WM_USER+100 then
  begin
    Close;
    Handled := True;
  end;
{$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  app: string;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
{$IFDEF WIN64}
  Caption := 'Log Trace (x64)';
  ChangeWindowMessageFilter(WM_USER+100, MSGFLT_ADD);
{$ELSE}
  app := '..\..\Win64\Debug\LogTrace.exe';

{$IFDEF RunX64}
  FillChar(si, SizeOf(TStartupInfo), 0);
  FillChar(pi, Sizeof(TProcessInformation), 0);

  si.cb := SizeOf(TStartupInfo);
  if not CreateProcess(PChar(app), nil, nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
  begin
    ShowMessage('Unable to start x64 process');
    Exit;
  end;

  CloseHandle(pi.hProcess);
  x64Thread := pi.hThread;
{$ENDIF}

  Caption := 'Log Trace (x86)';
{$ENDIF}

  BeginLog;
end;

procedure TForm1.FormDestroy(Sender: TObject);
{$IFNDEF WIN64}
var
  hwnd: THandle;
{$ENDIF}
begin
{$IFNDEF WIN64}
{$IFDEF RunX64}
  hwnd := FindWindow('TForm1', 'Log Trace (x64)');
  PostMessage(hwnd, WM_USER+100, 0, 0);
  CloseHandle(x64Thread);
{$ENDIF}
{$ENDIF}

  EndLog;
end;

end.
