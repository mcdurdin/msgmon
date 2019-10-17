unit MsgMon.UI.StackViewFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  MsgMon.Data.Database,
  MsgMon.System.Data.Image,
  MsgMon.System.StackBase,
  MsgMon.System.PlainStackRenderer,
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
    FPlainStackRenderer: TPlainStackRenderer;
    FStackRenderer: TStackRenderer;
    FCurrentEventID: Int64;
    procedure SettingsChange(Sender: TObject);
    procedure UpdateStatus(const msg: string);
    procedure StackRendererEvent(Sender: TObject; const Message: string);
    procedure CreateStackRenderer;
    procedure RenderReady(Sender: TObject; event_id: Int64; sr: TStackRows);
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
  FreeAndNil(FStackRenderer);

  db := Adb;

  if Assigned(db) then
    CreateStackRenderer;
end;

procedure TMMStackViewFrame.CreateStackRenderer;
var
  FSystemModules: TMMImages;
begin
  FSystemModules := db.LoadImages(0);
  try
    FStackRenderer := TStackRenderer.Create(
      TMMSettings.Instance.DbghelpPath,
      TMMSettings.Instance.SymbolPath,
      FSystemModules);
    FStackRenderer.OnEvent := StackRendererEvent;

    FPlainStackRenderer := TPlainStackRenderer.Create(
      FSystemModules);
  finally
    FSystemModules.Free;
  end;

  UpdateStatus('');
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
  if not Assigned(FStackRenderer) then
    lblStatus.Caption := 'No data'
  else if msg = '' then
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
  if not Assigned(m) or not Assigned(db) or not Assigned(FStackRenderer) then
  begin
    gridStack.RowCount := 1;
    Exit;
  end;

  images := db.LoadImages(m.pid);
  try
    FCurrentEventID := m.event_id;
    // TODO: Move this into a separate thread to avoid blocking ui thread
//    FStackRenderer.Cancel;

    // Get a plain, symbol free trace with our naive renderer first.
    sr := FPlainStackRenderer.Render(m.stack, images);
    RenderReady(Self, m.event_id, sr);

    // Start looking up symbols on our background stack renderer thread

    // Cancels current render, starts a new one.
//    FStackRenderer.StartRender(m.event_id, m.stack, images);
  finally
    images.Free;
  end;
end;

procedure TMMStackViewFrame.CloseDatabase;
begin
  db := nil;
  FreeAndNil(FStackRenderer);
  FreeAndNil(FPlainStackRenderer);
end;

procedure TMMStackViewFrame.FormCreate(Sender: TObject);
begin
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
  FreeAndNil(FPlainStackRenderer);
  CreateStackRenderer;
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
  FreeAndNil(FPlainStackRenderer);
end;

procedure TMMStackViewFrame.RenderReady(Sender: TObject; event_id: Int64; sr: TStackRows);
var
  i: Integer;
  r: TStackRow;
begin
  if event_id <> FCurrentEventID then
    // Stack trace is late to the party, ignore it
    Exit;

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
end;

end.
