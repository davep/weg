object wegLibNGColoursDialog: TwegLibNGColoursDialog
  Left = 224
  Top = 149
  BorderStyle = bsDialog
  Caption = 'Colours'
  ClientHeight = 278
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbDOSColours: TGroupBox
    Left = 8
    Top = 8
    Width = 177
    Height = 233
    Caption = '&DOS Colour Map'
    TabOrder = 2
    object lblNormal: TLabel
      Left = 57
      Top = 16
      Width = 33
      Height = 13
      Caption = 'Normal'
    end
    object lblBright: TLabel
      Left = 124
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Bright'
    end
    object lblBlack: TLabel
      Left = 8
      Top = 38
      Width = 27
      Height = 13
      Caption = '&Black'
      FocusControl = pnlNormalBlack
    end
    object lblBlue: TLabel
      Left = 14
      Top = 62
      Width = 21
      Height = 13
      Caption = 'B&lue'
      FocusControl = pnlNormalBlue
    end
    object lblGreen: TLabel
      Left = 6
      Top = 86
      Width = 29
      Height = 13
      Caption = '&Green'
      FocusControl = pnlNormalGreen
    end
    object lblCyan: TLabel
      Left = 11
      Top = 110
      Width = 24
      Height = 13
      Caption = 'C&yan'
      FocusControl = pnlNormalCyan
    end
    object lblRed: TLabel
      Left = 15
      Top = 134
      Width = 20
      Height = 13
      Caption = '&Red'
      FocusControl = pnlNormalRed
    end
    object lblViolet: TLabel
      Left = 9
      Top = 158
      Width = 26
      Height = 13
      Caption = '&Violet'
      FocusControl = pnlNormalViolet
    end
    object lblBrown: TLabel
      Left = 5
      Top = 182
      Width = 30
      Height = 13
      Caption = 'Bro&wn'
      FocusControl = pnlNormalBrown
    end
    object lblWhite: TLabel
      Left = 7
      Top = 206
      Width = 28
      Height = 13
      Caption = '&White'
      FocusControl = pnlNormalWhite
    end
    object pnlNormalBlack: TPanel
      Left = 41
      Top = 32
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 0
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightBlack: TPanel
      Tag = 8
      Left = 105
      Top = 32
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 1
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalBlue: TPanel
      Tag = 1
      Left = 41
      Top = 56
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 2
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightBlue: TPanel
      Tag = 9
      Left = 105
      Top = 56
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 3
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalGreen: TPanel
      Tag = 2
      Left = 41
      Top = 80
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 4
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightGreen: TPanel
      Tag = 10
      Left = 105
      Top = 80
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 5
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalCyan: TPanel
      Tag = 3
      Left = 41
      Top = 104
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 6
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightCyan: TPanel
      Tag = 11
      Left = 105
      Top = 104
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 7
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalRed: TPanel
      Tag = 4
      Left = 41
      Top = 128
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 8
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightRed: TPanel
      Tag = 12
      Left = 105
      Top = 128
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 9
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalViolet: TPanel
      Tag = 5
      Left = 41
      Top = 152
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 10
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightViolet: TPanel
      Tag = 13
      Left = 105
      Top = 152
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 11
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalBrown: TPanel
      Tag = 6
      Left = 41
      Top = 176
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 12
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightBrown: TPanel
      Tag = 14
      Left = 105
      Top = 176
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 13
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlNormalWhite: TPanel
      Tag = 7
      Left = 41
      Top = 200
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 14
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
    object pnlBrightWhite: TPanel
      Tag = 15
      Left = 105
      Top = 200
      Width = 65
      Height = 25
      BevelInner = bvLowered
      Caption = 'Click Here'
      TabOrder = 15
      TabStop = True
      OnClick = pnlDOSMapClick
      OnEnter = pnlDOSMapEnter
      OnExit = pnlDOSMapExit
    end
  end
  object btnOK: TButton
    Left = 328
    Top = 248
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 406
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbNGColours: TGroupBox
    Left = 192
    Top = 8
    Width = 289
    Height = 233
    Caption = 'Nor&ton Guide Colours'
    TabOrder = 3
    object lblNormalGuide: TLabel
      Left = 21
      Top = 37
      Width = 33
      Height = 13
      Caption = '&Normal'
      FocusControl = cbNormalText
    end
    object lblText: TLabel
      Left = 106
      Top = 16
      Width = 21
      Height = 13
      Caption = 'Text'
    end
    object lblBackground: TLabel
      Left = 199
      Top = 16
      Width = 58
      Height = 13
      Caption = 'Background'
    end
    object lblBold: TLabel
      Left = 33
      Top = 69
      Width = 21
      Height = 13
      Caption = '&Bold'
      FocusControl = cbBoldText
    end
    object lblUnderline: TLabel
      Left = 9
      Top = 133
      Width = 45
      Height = 13
      Caption = '&Underline'
      FocusControl = cbUnderlineText
    end
    object lblReverse: TLabel
      Left = 14
      Top = 101
      Width = 40
      Height = 13
      Caption = '&Reverse'
      FocusControl = cbReverseText
    end
    object lblSelected: TLabel
      Left = 12
      Top = 165
      Width = 42
      Height = 13
      Caption = '&Selected'
      FocusControl = cbSelectedText
    end
    object lblFocused: TLabel
      Left = 13
      Top = 197
      Width = 41
      Height = 13
      Caption = '&Focused'
      FocusControl = cbFocusedText
    end
    object cbNormalText: TComboBox
      Left = 64
      Top = 32
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 0
      OnDrawItem = cbNGColourDrawItem
      Items.Strings = (
        'Black'
        'Blue'
        'Green'
        'Cyan'
        'Red'
        'Violet'
        'Brown'
        'White'
        'Bright Black'
        'Bright Blue'
        'Bright Green'
        'Bright Cyan'
        'Bright Red'
        'Bright Violet'
        'Bright Brown'
        'Bright White')
    end
    object cbNormalBackground: TComboBox
      Left = 176
      Top = 32
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 1
      OnDrawItem = cbNGColourDrawItem
    end
    object cbBoldText: TComboBox
      Left = 64
      Top = 64
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 2
      OnDrawItem = cbNGColourDrawItem
    end
    object cbBoldBackground: TComboBox
      Left = 176
      Top = 64
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 3
      OnDrawItem = cbNGColourDrawItem
    end
    object cbReverseText: TComboBox
      Left = 64
      Top = 96
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 4
      OnDrawItem = cbNGColourDrawItem
    end
    object cbReverseBackground: TComboBox
      Left = 176
      Top = 96
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 5
      OnDrawItem = cbNGColourDrawItem
    end
    object cbUnderlineText: TComboBox
      Left = 64
      Top = 128
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 6
      OnDrawItem = cbNGColourDrawItem
    end
    object cbUnderlineBackground: TComboBox
      Left = 176
      Top = 128
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 7
      OnDrawItem = cbNGColourDrawItem
    end
    object cbSelectedText: TComboBox
      Left = 64
      Top = 160
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 8
      OnDrawItem = cbNGColourDrawItem
    end
    object cbSelectedBackground: TComboBox
      Left = 176
      Top = 160
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 9
      OnDrawItem = cbNGColourDrawItem
    end
    object cbFocusedText: TComboBox
      Left = 64
      Top = 192
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 10
      OnDrawItem = cbNGColourDrawItem
    end
    object cbFocusedBackground: TComboBox
      Left = 176
      Top = 192
      Width = 105
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 11
      OnDrawItem = cbNGColourDrawItem
    end
  end
  object btnDefaults: TButton
    Left = 8
    Top = 248
    Width = 97
    Height = 25
    Caption = 'Reset to Defaults'
    TabOrder = 4
    OnClick = btnDefaultsClick
  end
  object cdDOSMap: TColorDialog
    Ctl3D = True
    Options = [cdAnyColor]
    Left = 168
    Top = 248
  end
end
