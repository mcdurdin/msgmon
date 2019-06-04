object MMMainForm: TMMMainForm
  Left = 0
  Top = 0
  Caption = 'Message Monitor'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = menu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    635
    299)
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
        Width = 181
      end
      item
        Break = False
        Control = cmdClear
        ImageIndex = -1
        Width = 107
      end
      item
        Break = False
        Control = cmdFlushLibraries
        ImageIndex = -1
        Width = 105
      end
      item
        Break = False
        ImageIndex = -1
        Width = 224
      end>
    object cmdStartStopTrace: TButton
      Left = 11
      Top = 0
      Width = 168
      Height = 25
      Caption = '&Start Trace'
      TabOrder = 0
    end
    object cmdClear: TButton
      Left = 196
      Top = 0
      Width = 94
      Height = 25
      Caption = '&Clear'
      TabOrder = 1
    end
    object cmdFlushLibraries: TButton
      Left = 307
      Top = 0
      Width = 92
      Height = 25
      Caption = 'Flush libs'
      TabOrder = 2
      OnClick = cmdFlushLibrariesClick
    end
  end
  object lvMessages: TListView
    Left = 0
    Top = 29
    Width = 635
    Height = 251
    Align = alClient
    Columns = <>
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
    Panels = <
      item
        Width = 250
      end>
  end
  object progress: TProgressBar
    Left = 309
    Top = 282
    Width = 277
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Visible = False
  end
  object menu: TMainMenu
    Left = 312
    Top = 152
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuFileOpen: TMenuItem
        Caption = '&Open...'
        Enabled = False
        ShortCut = 16463
        OnClick = mnuFileOpenClick
      end
      object mnuFileSave: TMenuItem
        Caption = '&Save...'
        Enabled = False
        ShortCut = 16467
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuFileCaptureEvents: TMenuItem
        Caption = '&Capture Events'
        ShortCut = 16453
        OnClick = mnuFileCaptureEventsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuFileExitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuEditCopy: TMenuItem
        Caption = '&Copy'
        Enabled = False
        ShortCut = 16451
      end
      object mnuEditFind: TMenuItem
        Caption = '&Find...'
        Enabled = False
        ShortCut = 16454
      end
      object mnuEditFindHighlight: TMenuItem
        Caption = 'Find &Highlight'
        Enabled = False
        ShortCut = 115
      end
      object mnuEditFindBookmark: TMenuItem
        Caption = 'Find &Bookmark'
        Enabled = False
        ShortCut = 117
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuEditAutoScroll: TMenuItem
        Caption = '&Auto Scroll'
        Enabled = False
        ShortCut = 16449
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuEditClearDisplay: TMenuItem
        Caption = 'C&lear Display'
        ShortCut = 16472
        OnClick = mnuEditClearDisplayClick
      end
    end
    object mnuMessage: TMenuItem
      Caption = '&Message'
      Enabled = False
    end
    object mnuFilter: TMenuItem
      Caption = 'Fi&lter'
      object mnuFilterFilter: TMenuItem
        Caption = '&Filter...'
        ShortCut = 16460
        OnClick = mnuFilterFilterClick
      end
      object mnuFilterResetFilter: TMenuItem
        Caption = 'Reset Filter'
        ShortCut = 16466
        OnClick = mnuFilterResetFilterClick
      end
      object mnuFilterLoad: TMenuItem
        Caption = 'Load Filter'
        Enabled = False
      end
      object mnuFilterSave: TMenuItem
        Caption = 'Save Filter...'
        Enabled = False
      end
      object mnuFilterOrganize: TMenuItem
        Caption = 'Organize Filters...'
        Enabled = False
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuFilterDropFilteredEvents: TMenuItem
        Caption = 'Drop Filtered Events'
        Enabled = False
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuFilterHighlight: TMenuItem
        Caption = 'Highlight...'
        Enabled = False
        ShortCut = 16456
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuHelpAbout: TMenuItem
        Caption = '&About...'
        OnClick = mnuHelpAboutClick
      end
    end
  end
end
