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
    Top = 0
    Height = 181
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
      ActivePage = tabMessageDetail
      Align = alClient
      TabOrder = 0
      object tabMessageDetail: TTabSheet
        Caption = 'Message Details'
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
      object TabSheet1: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
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
    Top = 0
    Width = 333
    Height = 181
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
    Top = 0
    Width = 365
    Height = 181
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
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
      object mnuMessageSelectColumns: TMenuItem
        Caption = 'Select &columns...'
        OnClick = mnuMessageSelectColumnsClick
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
end
