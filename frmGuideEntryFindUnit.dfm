object frmGuideEntryFind: TfrmGuideEntryFind
  Left = 321
  Top = 216
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 93
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = refreshDialog
  PixelsPerInch = 96
  TextHeight = 13
  object lblFind: TLabel
    Left = 8
    Top = 12
    Width = 23
    Height = 13
    Caption = 'F&ind:'
    FocusControl = edtFind
  end
  object edtFind: TEdit
    Left = 40
    Top = 8
    Width = 265
    Height = 21
    TabOrder = 0
    OnChange = refreshDialog
  end
  object cbCaseSensitive: TCheckBox
    Left = 8
    Top = 40
    Width = 145
    Height = 17
    Caption = '&Case Sensitive'
    TabOrder = 1
  end
  object cbRegularExpression: TCheckBox
    Left = 8
    Top = 64
    Width = 145
    Height = 17
    Caption = '&Regular Expression'
    TabOrder = 2
  end
  object btnFind: TButton
    Left = 230
    Top = 34
    Width = 75
    Height = 25
    Caption = '&Find'
    Default = True
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 230
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'C&ancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
end
