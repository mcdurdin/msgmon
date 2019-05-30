object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Message Monitor (x86)'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 635
    Height = 29
    AutoSize = True
    Bands = <
      item
        Control = cmdStartStopTrace
        ImageIndex = -1
        Width = 101
      end
      item
        Break = False
        Control = cmdClear
        ImageIndex = -1
        Width = 99
      end
      item
        Break = False
        ImageIndex = -1
        Width = 421
      end>
    object cmdStartStopTrace: TButton
      Left = 11
      Top = 0
      Width = 88
      Height = 25
      Caption = '&Start Trace'
      TabOrder = 0
      OnClick = cmdStartStopTraceClick
    end
    object cmdClear: TButton
      Left = 116
      Top = 0
      Width = 86
      Height = 25
      Caption = '&Clear'
      TabOrder = 1
    end
  end
  object lvMessages: TListView
    Left = 0
    Top = 29
    Width = 635
    Height = 251
    Align = alClient
    Columns = <
      item
        Caption = 'PID'
      end
      item
        Caption = 'TID'
      end
      item
        Caption = 'hwnd'
      end
      item
        Caption = 'msg'
        Width = 150
      end
      item
        Caption = 'wParam'
      end
      item
        Caption = 'lParam'
      end
      item
        Caption = 'details'
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = lvMessagesData
  end
  object statusBar: TStatusBar
    Left = 0
    Top = 280
    Width = 635
    Height = 19
    Panels = <>
  end
end
