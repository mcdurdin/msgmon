object MMWindowTreeFrame: TMMWindowTreeFrame
  Left = 0
  Top = 0
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'MMWindowTreeFrame'
  ClientHeight = 338
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 215
    Width = 651
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 338
  end
  object tvWindows: TTreeView
    Left = 0
    Top = 0
    Width = 651
    Height = 215
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RightClickSelect = True
    TabOrder = 0
    OnChange = tvWindowsChange
  end
  object gridDetails: TStringGrid
    Left = 0
    Top = 218
    Width = 651
    Height = 120
    Align = alBottom
    ColCount = 3
    DefaultRowHeight = 16
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 1
    OnDblClick = gridDetailsDblClick
  end
end
