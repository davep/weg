object frmGuideCredits: TfrmGuideCredits
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Credits for'
  ClientHeight = 222
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 412
    Top = 192
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object memCredits: TMemo
    Left = 5
    Top = 8
    Width = 482
    Height = 177
    Cursor = crArrow
    Lines.Strings = (
      'No credits available for this guide.')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnEnter = memCreditsEnter
  end
end
