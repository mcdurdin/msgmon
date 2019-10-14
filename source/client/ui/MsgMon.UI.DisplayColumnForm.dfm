object MMDisplayColumnsForm: TMMDisplayColumnsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Display Columns'
  ClientHeight = 313
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    570
    313)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAvailableColumns: TLabel
    Left = 8
    Top = 5
    Width = 84
    Height = 13
    Caption = 'Available &columns'
    FocusControl = lbAvailableColumns
  end
  object lblVisibleColumns: TLabel
    Left = 288
    Top = 5
    Width = 70
    Height = 13
    Caption = '&Visible columns'
    FocusControl = lbVisibleColumns
  end
  object lbAvailableColumns: TListBox
    Left = 8
    Top = 24
    Width = 193
    Height = 249
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbAvailableColumnsClick
  end
  object lbVisibleColumns: TListBox
    Left = 288
    Top = 24
    Width = 193
    Height = 249
    ItemHeight = 13
    TabOrder = 3
    OnClick = lbVisibleColumnsClick
  end
  object cmdRemove: TButton
    Left = 207
    Top = 143
    Width = 75
    Height = 25
    Caption = '< &Remove'
    TabOrder = 1
    OnClick = cmdRemoveClick
  end
  object cmdAdd: TButton
    Left = 207
    Top = 112
    Width = 75
    Height = 25
    Caption = '&Add >'
    TabOrder = 2
    OnClick = cmdAddClick
  end
  object cmdMoveUp: TButton
    Left = 487
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Move &up'
    TabOrder = 4
  end
  object cmdMoveDown: TButton
    Left = 487
    Top = 143
    Width = 75
    Height = 25
    Caption = 'Move &down'
    TabOrder = 5
  end
  object cmdOK: TButton
    Left = 405
    Top = 279
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 7
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 486
    Top = 279
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object cmdReset: TButton
    Left = 8
    Top = 279
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'R&eset'
    TabOrder = 6
    OnClick = cmdResetClick
  end
end
