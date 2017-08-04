object frmBookmarkName: TfrmBookmarkName
  Left = 329
  Top = 275
  BorderStyle = bsDialog
  Caption = 'Bookmark Caption'
  ClientHeight = 88
  ClientWidth = 312
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
  object lblCaption: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'C&aption:'
  end
  object edtCaption: TEdit
    Left = 8
    Top = 24
    Width = 297
    Height = 21
    TabOrder = 0
    OnChange = refreshDialog
  end
  object btnOk: TButton
    Left = 152
    Top = 56
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 232
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
