object wegLibNGSettingsDialog: TwegLibNGSettingsDialog
  Left = 204
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 169
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = refreshDialog
  PixelsPerInch = 96
  TextHeight = 13
  object lblGuideDirectory: TLabel
    Left = 8
    Top = 8
    Width = 113
    Height = 13
    Caption = '&Default Guide Directory:'
    FocusControl = edtGuideDirectory
  end
  object edtGuideDirectory: TEdit
    Left = 8
    Top = 24
    Width = 385
    Height = 21
    TabOrder = 0
  end
  object btnGuideDirectory: TButton
    Left = 400
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnGuideDirectoryClick
  end
  object btnFont: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Text &Font'
    TabOrder = 2
    OnClick = btnFontClick
  end
  object pnlFont: TPanel
    Left = 88
    Top = 56
    Width = 337
    Height = 25
    BevelInner = bvLowered
    Caption = 'pnlFont'
    TabOrder = 3
    OnDblClick = btnFontClick
  end
  object cbUseColour: TCheckBox
    Left = 8
    Top = 96
    Width = 241
    Height = 17
    Caption = '&Use Colour'
    TabOrder = 4
  end
  object cbOEMToANSI: TCheckBox
    Left = 8
    Top = 120
    Width = 241
    Height = 17
    Caption = 'Convert OEM To &ANSI'
    TabOrder = 5
  end
  object cbSingleClick: TCheckBox
    Left = 8
    Top = 144
    Width = 241
    Height = 17
    Caption = '&Single Click to Jump'
    TabOrder = 6
  end
  object btnOk: TButton
    Left = 350
    Top = 104
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 350
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object fdGuideFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Options = [fdFixedPitchOnly]
    Left = 304
    Top = 88
  end
end
