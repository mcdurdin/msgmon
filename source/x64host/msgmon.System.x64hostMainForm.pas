unit msgmon.System.x64hostMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts;

type
  Tmsgmonx64Host = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  msgmonx64Host: Tmsgmonx64Host;

implementation

const
  LibraryName = 'msgmon.capture.x64.dll';

function BeginLog: BOOL; stdcall; external LibraryName;
function EndLog: BOOL; stdcall; external LibraryName;

{$R *.dfm}

procedure Tmsgmonx64Host.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := False;
  if Msg.message = WM_USER+100 then
  begin
    Close;
    Handled := True;
  end;
end;

procedure Tmsgmonx64Host.FormCreate(Sender: TObject);
begin
  ChangeWindowMessageFilter(WM_USER+100, MSGFLT_ADD);
  BeginLog;
end;

procedure Tmsgmonx64Host.FormDestroy(Sender: TObject);
begin
  EndLog;
end;

end.
