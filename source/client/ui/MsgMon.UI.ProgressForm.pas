unit MsgMon.UI.ProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  MsgMon.System.ProgressManager;

type
  TMMProgressForm = class(TForm, IProgressUI)
    progress: TProgressBar;
    cmdCancel: TButton;
    lblMessage: TLabel;
    procedure cmdCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCallback: TProgressCallback;
    FCancelled: Boolean;
    LastYield: Cardinal;
    procedure WMUser(var Message: TMessage); message WM_USER;
  protected
    { IProgressUI }
    procedure SetCanCancel(const Value: Boolean);
    procedure SetMax(const Value: Integer);
    procedure SetMessage(const Value: string);
    procedure SetPosition(const Value: Integer);
    procedure SetTitle(const Value: string);
    function GetCancelled: Boolean;
    function GetCanCancel: Boolean;
    function GetMax: Integer;
    function GetMessage: string;
    function GetPosition: Integer;
    function GetTitle: string;
    procedure Yield;
  public
    class function Execute(
      Owner: TComponent;
      Callback: TProgressCallback): Boolean;

    property Callback: TProgressCallback read FCallback write FCallback;
    property Message: string read GetMessage write SetMessage;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition;
    property CanCancel: Boolean read GetCanCancel write SetCanCancel;
    property Cancelled: Boolean read GetCancelled;
  end;

implementation

{$R *.dfm}

{ TfrmProgress }

procedure TMMProgressForm.cmdCancelClick(Sender: TObject);
begin
  FCancelled := True;
  cmdCancel.Enabled := False;
  cmdCancel.Update;
  lblMessage.Caption := 'Cancelling...';
  lblMessage.Update;
  progress.Style := pbstMarquee;
  progress.Update;
end;

class function TMMProgressForm.Execute(Owner: TComponent;
  Callback: TProgressCallback): Boolean;
var
  frm: TMMProgressForm;
begin
  frm := TMMProgressForm.Create(Owner);
  try
    frm.Callback := Callback;
    Result := frm.ShowModal = mrOk;
  finally
    frm.Free;
  end;
end;

procedure TMMProgressForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER, 0, 0);
end;

function TMMProgressForm.GetCanCancel: Boolean;
begin
  Result := cmdCancel.Enabled;
end;

function TMMProgressForm.GetCancelled: Boolean;
begin
  Result := FCancelled;
end;

function TMMProgressForm.GetMax: Integer;
begin
  Result := progress.Max;
end;

function TMMProgressForm.GetMessage: string;
begin
  Result := lblMessage.Caption;
end;

function TMMProgressForm.GetPosition: Integer;
begin
  Result := progress.Position;
end;

function TMMProgressForm.GetTitle: string;
begin
  Result := Caption;
end;

procedure TMMProgressForm.SetCanCancel(const Value: Boolean);
begin
  cmdCancel.Enabled := Value;
  cmdCancel.Update;
end;

procedure TMMProgressForm.SetMax(const Value: Integer);
begin
  progress.Max := Value;
  progress.Update;
end;

procedure TMMProgressForm.SetMessage(const Value: string);
begin
  lblMessage.Caption := Value;
  lblMessage.Update;
end;

procedure TMMProgressForm.SetPosition(const Value: Integer);
begin
  progress.Position := Value;
end;

procedure TMMProgressForm.SetTitle(const Value: string);
begin
  Caption := Value;
end;

procedure TMMProgressForm.WMUser(var Message: TMessage);
begin
  Update;
  FCallback(Self);
  if FCancelled then
    ModalResult := mrCancel
  else
    ModalResult := mrOk;
end;

procedure TMMProgressForm.Yield;
begin
  if GetTickCount = LastYield then
    Exit;
  Application.ProcessMessages;
  LastYield := GetTickCount;
end;

end.
