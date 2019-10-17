object MMStackViewFrame: TMMStackViewFrame
  Left = 0
  Top = 0
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'MMStackViewFrame'
  ClientHeight = 338
  ClientWidth = 651
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gridStack: TStringGrid
    Left = 0
    Top = 0
    Width = 651
    Height = 299
    Align = alClient
    DefaultRowHeight = 16
    FixedCols = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 0
  end
  object panCommand: TPanel
    Left = 0
    Top = 299
    Width = 651
    Height = 39
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      651
      39)
    object lblStatus: TLabel
      Left = 8
      Top = 11
      Width = 474
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'status'
    end
    object cmdCopyStack: TButton
      Left = 569
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Copy Stack'
      TabOrder = 0
      OnClick = cmdCopyStackClick
    end
  end
end
