object MMMainForm: TMMMainForm
  Left = 0
  Top = 0
  Caption = 'Message Monitor'
  ClientHeight = 357
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = menu
  OldCreateOrder = False
  WindowState = wsMaximized
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
  object Splitter1: TSplitter
    Left = 333
    Top = 25
    Height = 156
    Align = alRight
    ExplicitLeft = 360
    ExplicitTop = 160
    ExplicitHeight = 100
  end
  object statusBar: TStatusBar
    Left = 0
    Top = 338
    Width = 701
    Height = 19
    Panels = <
      item
        Width = 250
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object progress: TProgressBar
    Left = 253
    Top = 340
    Width = 144
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    Visible = False
  end
  object panDetail: TPanel
    Left = 0
    Top = 184
    Width = 701
    Height = 154
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 701
      Height = 154
      ActivePage = tabCallStack
      Align = alClient
      TabOrder = 0
      object tabMessageDetail: TTabSheet
        Caption = 'Message Details'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object gridMessageDetails: TStringGrid
          Left = 0
          Top = 0
          Width = 693
          Height = 126
          Align = alClient
          ColCount = 3
          DefaultRowHeight = 16
          RowCount = 2
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
          TabOrder = 0
          OnClick = gridMessageDetailsClick
          OnDblClick = gridMessageDetailsDblClick
          OnDrawCell = gridMessageDetailsDrawCell
        end
      end
      object tabCallStack: TTabSheet
        Caption = 'Call Stack'
        ImageIndex = 1
      end
      object TabSheet1: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memoLog: TMemo
          Left = 0
          Top = 0
          Width = 693
          Height = 126
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object gridMessages: TDrawGrid
    Left = 0
    Top = 25
    Width = 333
    Height = 156
    Align = alClient
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goColMoving, goRowSelect]
    PopupMenu = mnuItem
    TabOrder = 3
    OnClick = gridMessagesClick
    OnColumnMoved = gridMessagesColumnMoved
    OnDblClick = gridMessagesDblClick
    OnDrawCell = gridMessagesDrawCell
  end
  object panWindowTree: TPanel
    Left = 336
    Top = 25
    Width = 365
    Height = 156
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
  end
  object panTop: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    object Shape1: TShape
      Left = 0
      Top = 24
      Width = 701
      Height = 1
      Align = alBottom
      ExplicitTop = -40
    end
    object panSearch1: TPanel
      Left = 185
      Top = 0
      Width = 88
      Height = 24
      Align = alLeft
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = '(no highlight)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnDblClick = panSearchDblClick
    end
    object panSearch2: TPanel
      Tag = 1
      Left = 273
      Top = 0
      Width = 88
      Height = 24
      Align = alLeft
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = '(no highlight)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clFuchsia
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnDblClick = panSearchDblClick
    end
    object panSearch3: TPanel
      Tag = 2
      Left = 361
      Top = 0
      Width = 88
      Height = 24
      Align = alLeft
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = '(no highlight)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnDblClick = panSearchDblClick
    end
    object panSearch4: TPanel
      Tag = 3
      Left = 449
      Top = 0
      Width = 88
      Height = 24
      Align = alLeft
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = '(no highlight)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnDblClick = panSearchDblClick
    end
    object panToolbar: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 24
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 4
      Visible = False
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
        ShortCut = 16463
        OnClick = mnuFileOpenClick
      end
      object mnuFileSave: TMenuItem
        Caption = '&Save...'
        Enabled = False
        ShortCut = 16467
        OnClick = mnuFileSaveClick
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
      object mnuMessageSelectColumns: TMenuItem
        Caption = 'Select &columns...'
        OnClick = mnuMessageSelectColumnsClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuMessageSearchContext1: TMenuItem
        Caption = 'Search context &1'
        ShortCut = 24625
        OnClick = panSearchDblClick
      end
      object mnuMessageSearchContext2: TMenuItem
        Tag = 1
        Caption = 'Search context &2'
        ShortCut = 24626
        OnClick = panSearchDblClick
      end
      object mnuMessageSearchContext3: TMenuItem
        Tag = 2
        Caption = 'Search context &3'
        ShortCut = 24627
        OnClick = panSearchDblClick
      end
      object mnuMessageSearchContext4: TMenuItem
        Tag = 3
        Caption = 'Search context &4'
        ShortCut = 24628
        OnClick = panSearchDblClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mnuMessageFind: TMenuItem
        Caption = '&Find...'
        ShortCut = 16454
        OnClick = mnuMessageFindClick
      end
      object mnuMessageFindPrevious: TMenuItem
        Caption = 'Find &Previous'
        ShortCut = 32806
        OnClick = mnuMessageFindPreviousClick
      end
      object mnuMessageFindNext: TMenuItem
        Caption = 'Find &Next'
        ShortCut = 32808
        OnClick = mnuMessageFindNextClick
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
        ShortCut = 16456
        OnClick = mnuFilterHighlightClick
      end
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      OnClick = mnuToolsClick
      object cmdToolsEnableStackTraces: TMenuItem
        Caption = 'Enable &stack traces'
        OnClick = cmdToolsEnableStackTracesClick
      end
      object cmdToolsDisplayStackTraces: TMenuItem
        Caption = '&Display stack traces'
        OnClick = cmdToolsDisplayStackTracesClick
      end
      object cmdToolsSymbolPath: TMenuItem
        Caption = '&Symbol path...'
        OnClick = cmdToolsSymbolPathClick
      end
      object cmdToolsDbghelpPath: TMenuItem
        Caption = 'Dbghelp &path...'
        OnClick = cmdToolsDbghelpPathClick
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
    object mnuPopupCopyRows: TMenuItem
      Caption = 'Copy &selected row(s)'
      OnClick = mnuPopupCopyRowsClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object mnuPopupFilterEdit: TMenuItem
      Caption = 'Edit Filter '#39'<>'#39'...'
      OnClick = mnuPopupFilterEditClick
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'db'
    Filter = 'Message Monitor Database Files (*.db)|*.db|All files (*.*)|*.*'
    Left = 448
    Top = 64
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'db'
    Filter = 'Message Monitor Database Files (*.db)|*.db|All files (*.*)|*.*'
    Left = 536
    Top = 64
  end
  object tmrUpdateWindowTree: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrUpdateWindowTreeTimer
    Left = 344
    Top = 192
  end
  object dlgFind: TFindDialog
    Options = [frDown, frHideUpDown]
    OnFind = dlgFindFind
    Left = 352
    Top = 200
  end
  object dlgLocateDbghelp: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'dbghelp.dll'
        FileMask = 'dbghelp.dll'
      end
      item
        DisplayName = 'All files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoFileMustExist]
    Left = 360
    Top = 208
  end
end
