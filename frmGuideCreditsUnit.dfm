object frmGuideCredits: TfrmGuideCredits
  Left = 192
  Top = 107
  Width = 500
  Height = 324
  BorderIcons = [biSystemMenu]
  Caption = 'Credits for'
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
  object lblFileNameLabel: TLabel
    Left = 18
    Top = 8
    Width = 50
    Height = 13
    Caption = 'File Name:'
  end
  object lblFileName: TLabel
    Left = 72
    Top = 8
    Width = 54
    Height = 13
    Caption = 'lblFileName'
  end
  object lblFileLocationLabel: TLabel
    Left = 5
    Top = 24
    Width = 63
    Height = 13
    Caption = 'File Location:'
  end
  object lblFileLocation: TLabel
    Left = 72
    Top = 24
    Width = 67
    Height = 13
    Caption = 'lblFileLocation'
  end
  object lblFileSizeLabel: TLabel
    Left = 26
    Top = 40
    Width = 42
    Height = 13
    Caption = 'File Size:'
  end
  object lblFileSize: TLabel
    Left = 72
    Top = 40
    Width = 46
    Height = 13
    Caption = 'lblFileSize'
  end
  object lblTimeStampLabel: TLabel
    Left = 9
    Top = 56
    Width = 59
    Height = 13
    Caption = 'Time Stamp:'
  end
  object lblTimeStamp: TLabel
    Left = 72
    Top = 56
    Width = 63
    Height = 13
    Caption = 'lblTimeStamp'
  end
  object btnOK: TButton
    Left = 412
    Top = 267
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object memCredits: TMemo
    Left = 5
    Top = 80
    Width = 482
    Height = 180
    Cursor = crArrow
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'No credits available for this guide.')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnEnter = memCreditsEnter
  end
  object btnCopy: TButton
    Left = 5
    Top = 267
    Width = 75
    Height = 25
    Caption = '&Copy'
    TabOrder = 2
    OnClick = btnCopyClick
  end
end
