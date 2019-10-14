object MMFilterForm: TMMFilterForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Message Monitor Filter'
  ClientHeight = 373
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    719
    373)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 201
    Height = 13
    Caption = 'Display entries matching these conditions:'
  end
  object Label2: TLabel
    Left = 608
    Top = 27
    Width = 22
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'then'
    ExplicitLeft = 524
  end
  object cbColumn: TComboBox
    Left = 8
    Top = 24
    Width = 115
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
  end
  object cbRelation: TComboBox
    Left = 129
    Top = 24
    Width = 80
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbValue: TComboBox
    Left = 215
    Top = 24
    Width = 387
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object cbAction: TComboBox
    Left = 636
    Top = 24
    Width = 75
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object lvFilters: TListView
    Left = 8
    Top = 82
    Width = 703
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Column'
        Width = 128
      end
      item
        Caption = 'Relation'
        Width = 80
      end
      item
        Caption = 'Value'
        Width = 200
      end
      item
        Caption = 'Action'
        Width = 80
      end>
    RowSelect = True
    TabOrder = 7
    ViewStyle = vsReport
  end
  object cmdReset: TButton
    Left = 8
    Top = 51
    Width = 75
    Height = 25
    Caption = 'R&eset'
    TabOrder = 4
  end
  object cmdAdd: TButton
    Left = 555
    Top = 51
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 5
    OnClick = cmdAddClick
  end
  object cmdRemove: TButton
    Left = 636
    Top = 51
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    TabOrder = 6
    OnClick = cmdRemoveClick
  end
  object cmdOK: TButton
    Left = 474
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object cmdCancel: TButton
    Left = 555
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object cmdApply: TButton
    Left = 636
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 10
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 354
    Width = 719
    Height = 19
    Panels = <>
  end
end
