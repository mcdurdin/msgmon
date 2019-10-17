unit MsgMon.UI.StackViewFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  MsgMon.Data.Database,
  MsgMon.System.Data.Image,
  MsgMon.System.StackRenderer,
  MsgMon.System.Data.Message;

type
  TMMStackViewFrame = class(TForm)
    gridStack: TStringGrid;
    panCommand: TPanel;
    cmdProperties: TButton;
    cmdCopyStack: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    db: TMMDatabase;
    FSystemModules: TMMImages;
    FStackRenderer: TStackRenderer;
    procedure SettingsChange(Sender: TObject);
    procedure UpdateStatus(const msg: string);
    procedure StackRendererEvent(Sender: TObject; const Message: string);
    { Private declarations }
  public
    { Public declarations }
    procedure SetDatabase(Adb: TMMDatabase);
    procedure CloseDatabase;
    procedure UpdateView(m: TMMMessage);
  end;

implementation

uses
  MsgMon.UI.MainForm,
  MsgMon.System.Settings;

{$R *.dfm}

procedure TMMStackViewFrame.SetDatabase(Adb: TMMDatabase);
begin
  db := Adb;
  if Assigned(db) then
    FSystemModules := db.LoadImages(0);
end;

function FormatMemoryAddress(r: UInt64): string;
begin
  Result :=
    '0x'+
    IntToHex((r and $FFFFFFFF00000000) shr 32, 8) +
    '''' +
    IntToHex((r and $00000000FFFFFFFF), 8);
end;

procedure TMMStackViewFrame.UpdateStatus(const msg: string);
begin
  if msg = '' then
  begin
    if FStackRenderer.DbghelpLoaded
      then lblStatus.Caption := 'Symbol resolution enabled'
      else lblStatus.Caption := 'Symbol resolution disabled';
  end
  else
  begin
    lblStatus.Caption := msg;
    MMMainForm.Log(llDebug, msg); //TODO Cleanup MMMainForm reference
  end;
  lblStatus.Update;
end;

procedure TMMStackViewFrame.UpdateView(m: TMMMessage);
var
  images: TMMImages;
  sr: TStackRows;
  r: TStackRow;
  i: Integer;
begin
  if not Assigned(m) or not Assigned(db) then
    Exit;

  images := db.LoadImages(m.pid);
  try
    // TODO: Move this into a separate thread to avoid blocking ui thread
    sr := FStackRenderer.Render(m.stack, images, FSystemModules);
    gridStack.RowCount := Length(sr)+1;
    for i := 0 to High(sr) do
    begin
      r := sr[i];
      if r.IsKernel
        then gridStack.Cells[0, i+1] := 'K '+IntToStr(r.Frame)
        else gridStack.Cells[0, i+1] := 'U '+IntToStr(r.Frame);
      gridStack.Cells[1, i+1] := r.Module;
      gridStack.Cells[2, i+1] := r.Location;
      gridStack.Cells[3, i+1] := FormatMemoryAddress(r.Address);
      gridStack.Cells[4, i+1] := r.Path;
    end;
    if gridStack.RowCount > 1 then
      gridStack.FixedRows := 1;

  finally
    images.Free;
  end;
end;

procedure TMMStackViewFrame.CloseDatabase;
begin
  db := nil;
  FreeAndNil(FSystemModules);
end;

procedure TMMStackViewFrame.FormCreate(Sender: TObject);
begin
  FStackRenderer := TStackRenderer.Create(
    TMMSettings.Instance.DbghelpPath,
    TMMSettings.Instance.SymbolPath);
  FStackRenderer.OnEvent := StackRendererEvent;

  TMMSettings.Instance.Notification(SettingsChange);
  gridStack.ColWidths[0] := 40;
  gridStack.ColWidths[1] := 148;
  gridStack.ColWidths[2] := 320;
  gridStack.ColWidths[3] := 128;
  gridStack.ColWidths[4] := 480;
  gridStack.Cells[0, 0] := 'Frame';
  gridStack.Cells[1, 0] := 'Module';
  gridStack.Cells[2, 0] := 'Location';
  gridStack.Cells[3, 0] := 'Address';
  gridStack.Cells[4, 0] := 'Path';
  UpdateStatus('');
end;

procedure TMMStackViewFrame.SettingsChange(Sender: TObject);
begin
  FreeAndNil(FStackRenderer);
  FStackRenderer := TStackRenderer.Create(
    TMMSettings.Instance.DbghelpPath,
    TMMSettings.Instance.SymbolPath);
end;

procedure TMMStackViewFrame.StackRendererEvent(Sender: TObject;
  const Message: string);
begin
  UpdateStatus(Message);
end;

procedure TMMStackViewFrame.FormDestroy(Sender: TObject);
begin
  TMMSettings.Instance.RemoveNotification(SettingsChange);
  FreeAndNil(FStackRenderer);
end;

end.
