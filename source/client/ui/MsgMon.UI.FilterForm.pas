unit MsgMon.UI.FilterForm;

// TODO: Group filters by column, so we can see all related filters together

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,

  MsgMon.System.Data.Column,
  MsgMon.System.Data.Filter,
  MsgMon.System.Data.Session;

type
  TMMFilterForm = class(TForm)
    cbColumn: TComboBox;
    cbRelation: TComboBox;
    cbValue: TComboBox;
    cbAction: TComboBox;
    lblTitle: TLabel;
    Label2: TLabel;
    lvFilters: TListView;
    cmdReset: TButton;
    cmdAdd: TButton;
    cmdRemove: TButton;
    cmdOK: TButton;
    cmdCancel: TButton;
    cmdApply: TButton;
    StatusBar1: TStatusBar;
    procedure cmdAddClick(Sender: TObject);
    procedure cmdRemoveClick(Sender: TObject);
  private
    FFilters: TMMFilters;
    procedure FillFilterView;
  public
    constructor Create(AOwner: TComponent; const ACaption: string; AFilters: TMMFilters); reintroduce;
  end;

implementation

{$R *.dfm}

{ TMMFilterForm }

procedure TMMFilterForm.cmdAddClick(Sender: TObject);
var
  f: TMMFilter;
begin
  if cbColumn.ItemIndex < 0 then
    Exit;

  if cbRelation.ItemIndex < 0 then
    Exit;

  if cbAction.ItemIndex < 0 then
    Exit;

//  if cbValue.Text = '' then
//    Exit;

  f := TMMFilter.Create;
  f.column := cbColumn.Items.Objects[cbColumn.ItemIndex] as TMMColumn;
  f.relation := TMMFilterRelation(cbRelation.Items.Objects[cbRelation.ItemIndex]);
  f.value := cbValue.Text;
  f.action := TMMFilterAction(cbAction.Items.Objects[cbAction.ItemIndex]);
  FFilters.Add(f);
  FillFilterView;
end;

procedure TMMFilterForm.cmdRemoveClick(Sender: TObject);
begin
  if lvFilters.Selected = nil then
    Exit;

  FFilters.Remove(lvFilters.Selected.Data);
  FillFilterView;
end;

constructor TMMFilterForm.Create(AOwner: TComponent; const ACaption: string; AFilters: TMMFilters);
var
  c: TMMColumn;
  r: TMMFilterRelation;
  a: TMMFilterAction;
begin
  inherited Create(AOwner);
  Caption := ACaption;

  FFilters := AFilters;

  // Prepare lookups

  for c in FFilters.Columns do
    cbColumn.Items.AddObject(c.Caption, c);

  for r := Low(TMMFilterRelation) to High(TMMFilterRelation) do
    cbRelation.Items.AddObject(MMFilterRelationName[r], Pointer(r));

  for a := Low(TMMFilterAction) to High(TMMFilterAction) do
    cbAction.Items.AddObject(MMFilterActionName[a], Pointer(a));

  cbColumn.ItemIndex := 0;
  cbRelation.ItemIndex := 0;
  cbAction.ItemIndex := 0;

  FillFilterView;
end;

procedure TMMFilterForm.FillFilterView;
var
  item: TListItem;
  filter: TMMFilter;
begin
  lvFilters.Items.Clear;
  for filter in FFilters do
  begin
    item := lvFilters.Items.Add;
    item.Data := filter;
    item.Caption := filter.column.Caption;
    item.SubItems.Add(MMFilterRelationName[filter.relation]);
    item.SubItems.Add(filter.value);
    item.SubItems.Add(MMFilterActionName[filter.action]);
  end;
end;

end.
