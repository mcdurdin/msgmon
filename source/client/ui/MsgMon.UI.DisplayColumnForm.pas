unit MsgMon.UI.DisplayColumnForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Contnrs,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Session;

type
  TMMDisplayColumnsForm = class(TForm)
    lbAvailableColumns: TListBox;
    lbVisibleColumns: TListBox;
    cmdRemove: TButton;
    cmdAdd: TButton;
    cmdMoveUp: TButton;
    cmdMoveDown: TButton;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblAvailableColumns: TLabel;
    lblVisibleColumns: TLabel;
    cmdReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdAddClick(Sender: TObject);
    procedure cmdRemoveClick(Sender: TObject);
    procedure lbAvailableColumnsClick(Sender: TObject);
    procedure lbVisibleColumnsClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdResetClick(Sender: TObject);
  private
    FDisplayColumns: TMMColumns;
    FAllColumns: TMMColumns;
    FSession: TMMSession;
    procedure EnableControls;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; ASession: TMMSession); reintroduce;
  end;

implementation

{$R *.dfm}

procedure TMMDisplayColumnsForm.cmdAddClick(Sender: TObject);
begin
  lbVisibleColumns.Items.AddObject(
    lbAvailableColumns.Items[lbAvailableColumns.ItemIndex],
    lbAvailableColumns.Items.Objects[lbAvailableColumns.ItemIndex]);
  lbAvailableColumns.Items.Delete(lbAvailableColumns.ItemIndex);
  EnableControls;
end;

procedure TMMDisplayColumnsForm.cmdOKClick(Sender: TObject);
var
  c: TMMColumn;
  i: Integer;
begin
  FDisplayColumns.Clear;
  for i := 0 to lbVisibleColumns.Items.Count - 1 do
  begin
    c := FAllColumns.FindClassName(TMMColumnClass(lbVisibleColumns.Items.Objects[i]).ClassName);
    Assert(Assigned(c));
    FDisplayColumns.Add(c.Clone);
  end;
  ModalResult := mrOk;
end;

procedure TMMDisplayColumnsForm.cmdRemoveClick(Sender: TObject);
begin
  lbAvailableColumns.Items.AddObject(
    lbVisibleColumns.Items[lbVisibleColumns.ItemIndex],
    lbVisibleColumns.Items.Objects[lbVisibleColumns.ItemIndex]);
  lbVisibleColumns.Items.Delete(lbVisibleColumns.ItemIndex);
  EnableControls;
end;

procedure TMMDisplayColumnsForm.cmdResetClick(Sender: TObject);
var
  i: Integer;
begin
  lbAvailableColumns.Clear;
  lbVisibleColumns.Clear;
  for i := 0 to FColumnClasses.Count - 1 do
    if FDefaultColumnClasses.Find(FColumnClasses[i].ClassName) = nil then
      lbAvailableColumns.Items.AddObject(FColumnClasses[i].Caption, TObject(FColumnClasses[i]));

  for i := 0 to FDefaultColumnClasses.Count - 1 do
    lbVisibleColumns.Items.AddObject(FDefaultColumnClasses[i].Caption, TObject(FDefaultColumnClasses[i]));

  EnableControls;
end;

constructor TMMDisplayColumnsForm.Create(AOwner: TComponent;
  ASession: TMMSession);
begin
  inherited Create(AOwner);
  FSession := ASession;
  FDisplayColumns := ASession.displayColumns;
  FAllColumns := ASession.allColumns;
end;

procedure TMMDisplayColumnsForm.FormCreate(Sender: TObject);
var
  c: TMMColumn;
begin
  for c in FAllColumns do
  begin
    if FDisplayColumns.FindClassName(c.ClassName) = nil
      then lbAvailableColumns.Items.AddObject(c.Caption, TObject(c.ClassType))
      else lbVisibleColumns.Items.AddObject(c.Caption, TObject(c.ClassType));
  end;
  EnableControls;
end;

procedure TMMDisplayColumnsForm.lbAvailableColumnsClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TMMDisplayColumnsForm.lbVisibleColumnsClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TMMDisplayColumnsForm.EnableControls;
begin
  cmdAdd.Enabled := lbAvailableColumns.ItemIndex >= 0;
  cmdRemove.Enabled := lbVisibleColumns.ItemIndex >= 0;
  cmdMoveUp.Enabled := lbVisibleColumns.ItemIndex > 0;
  cmdMoveDown.Enabled := (lbVisibleColumns.ItemIndex >= 0) and
    (lbVisibleColumns.ItemIndex < lbVisibleColumns.Items.Count - 1);
  cmdOk.Enabled := lbVisibleColumns.Items.Count > 0;
end;

end.
