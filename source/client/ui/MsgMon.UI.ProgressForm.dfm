object MMProgressForm: TMMProgressForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'MMProgressForm'
  ClientHeight = 105
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    298
    105)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 8
    Top = 12
    Width = 282
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblMessage'
  end
  object progress: TProgressBar
    Left = 8
    Top = 40
    Width = 281
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cmdCancel: TButton
    Left = 112
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 1
    OnClick = cmdCancelClick
  end
end
