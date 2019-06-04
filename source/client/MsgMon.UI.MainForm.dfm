object MMMainForm: TMMMainForm
  Left = 0
  Top = 0
  Caption = 'Message Monitor'
  ClientHeight = 357
  ClientWidth = 701
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
    701
    357)
  PixelsPerInch = 96
  TextHeight = 13
  object splitterDetail: TSplitter
    Left = 0
    Top = 181
    Width = 701
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 6
    ExplicitWidth = 633
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 701
    Height = 29
    AutoSize = True
    Bands = <
      item
        Control = cmdStartStopTrace
        ImageIndex = -1
        Width = 186
      end
      item
        Break = False
        Control = cmdClear
        ImageIndex = -1
        Width = 113
      end
      item
        Break = False
        Control = cmdFlushLibraries
        ImageIndex = -1
        Width = 111
      end
      item
        Break = False
        ImageIndex = -1
        Width = 273
      end>
    ExplicitWidth = 635
    object cmdStartStopTrace: TButton
      Left = 11
      Top = 0
      Width = 173
      Height = 25
      Caption = '&Start Trace'
      TabOrder = 0
    end
    object cmdClear: TButton
      Left = 201
      Top = 0
      Width = 100
      Height = 25
      Caption = '&Clear'
      TabOrder = 1
    end
    object cmdFlushLibraries: TButton
      Left = 318
      Top = 0
      Width = 98
      Height = 25
      Caption = 'Flush libs'
      TabOrder = 2
      OnClick = cmdFlushLibrariesClick
    end
  end
  object lvMessages: TListView
    Left = 0
    Top = 29
    Width = 701
    Height = 152
    Align = alClient
    Columns = <>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = mnuItem
    TabOrder = 1
    ViewStyle = vsReport
    OnData = lvMessagesData
    OnSelectItem = lvMessagesSelectItem
    ExplicitWidth = 635
    ExplicitHeight = 251
  end
  object statusBar: TStatusBar
    Left = 0
    Top = 338
    Width = 701
    Height = 19
    Panels = <
      item
        Width = 250
      end>
    ExplicitTop = 280
    ExplicitWidth = 635
  end
  object progress: TProgressBar
    Left = 309
    Top = 340
    Width = 343
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Visible = False
    ExplicitTop = 282
    ExplicitWidth = 277
  end
  object panDetail: TPanel
    Left = 0
    Top = 184
    Width = 701
    Height = 154
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 701
      Height = 154
      ActivePage = tabMessageDetail
      Align = alClient
      TabOrder = 0
      object tabMessageDetail: TTabSheet
        Caption = 'Message Details'
        ExplicitLeft = 8
        DesignSize = (
          693
          126)
        object lblParentWindow: TLabel
          Left = 13
          Top = 16
          Width = 71
          Height = 13
          Caption = 'Parent window'
        end
        object lblOwnerWindow: TLabel
          Left = 13
          Top = 43
          Width = 71
          Height = 13
          Caption = 'Owner window'
        end
        object lblMessageDetail: TLabel
          Left = 242
          Top = 5
          Width = 71
          Height = 13
          Caption = 'Message detail'
        end
        object editParentWindow: TEdit
          Left = 90
          Top = 13
          Width = 135
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
        object editOwnerWindow: TEdit
          Left = 90
          Top = 40
          Width = 135
          Height = 21
          ReadOnly = True
          TabOrder = 1
        end
        object memoMessageDetail: TMemo
          Left = 242
          Top = 24
          Width = 448
          Height = 99
          Anchors = [akLeft, akTop, akRight, akBottom]
          ReadOnly = True
          TabOrder = 2
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Call Stack'
        ImageIndex = 1
        DesignSize = (
          693
          126)
        object memoCallStack: TMemo
          Left = 0
          Top = 0
          Width = 693
          Height = 126
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
        end
      end
    end
  end
  object menu: TMainMenu
    Left = 112
    Top = 104
    object mnuFile: TMenuItem
      Caption = '&File'
      OnClick = mnuFileClick
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
      OnClick = mnuMessageClick
      object mnuMessageViewDetailPane: TMenuItem
        Caption = 'View &Detail Pane'
        OnClick = mnuMessageViewDetailPaneClick
      end
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
  object mnuItem: TPopupMenu
    OnPopup = mnuItemPopup
    Left = 40
    Top = 112
    object mnuPopupFilterInclude: TMenuItem
      Caption = '&Include '#39'<>'#39
      OnClick = mnuPopupFilterIncludeClick
    end
    object mnuPopupFilterExclude: TMenuItem
      Caption = 'E&xclude '#39'<>'#39
      OnClick = mnuPopupFilterExcludeClick
    end
    object mnuPopupCopy: TMenuItem
      Caption = '&Copy '#39'<>'#39
      OnClick = mnuPopupCopyClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object mnuPopupFilterEdit: TMenuItem
      Caption = 'Edit Filter '#39'<>'#39'...'
      OnClick = mnuPopupFilterEditClick
    end
  end
end
