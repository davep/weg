object frmGlobalFind: TfrmGlobalFind
  Left = 258
  Top = 129
  Width = 567
  Height = 533
  Caption = 'Expert Guide - Global Finder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tdbTop: TTBDock
    Left = 0
    Top = 0
    Width = 559
    Height = 49
    object tbGlobalFindMenu: TTBToolbar
      Left = 0
      Top = 0
      Caption = 'Global Find Menu'
      CloseButton = False
      FullSize = True
      Images = ilGlobalFind
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      object mnuFind: TTBSubmenuItem
        Caption = '&Find'
        object mnuFindStart: TTBItem
          Action = actFindStart
        end
        object mnuFindStop: TTBItem
          Action = actFindStop
        end
        object mnuFindSplit1: TTBSeparatorItem
        end
        object mnuFindOpen: TTBItem
          Action = actFindOpen
        end
        object mnuFindSplit2: TTBSeparatorItem
        end
        object mnuFindClearResults: TTBItem
          Action = actFindClearResults
        end
        object mnuFindSplit3: TTBSeparatorItem
        end
        object mnuFindClose: TTBItem
          Action = actFindClose
        end
      end
      object mnuOptions: TTBSubmenuItem
        Caption = '&Options'
        object mnuOptionsSearch: TTBSubmenuItem
          Caption = '&Search'
          Hint = 'Decide where to search'
          object mnuOptionsSearchCurrent: TTBItem
            Action = actOptionsSearchCurrentGuide
          end
          object mnuOptionsSearchAllKnown: TTBItem
            Action = actOptionsSearchAllKnownGuides
          end
        end
        object mnuSearchLookIn: TTBSubmenuItem
          Caption = '&Look In'
          Hint = 'Set the type of entry to look at'
          object mnuOptionsLookInShorts: TTBItem
            Action = actOptionsLookInShorts
          end
          object mnuOptionsLookInLongs: TTBItem
            Action = actOptionsLookInLongs
          end
        end
        object mnuOptionsMatchCase: TTBItem
          Action = actOptionsMatchCase
        end
        object mnuOptionsRegexp: TTBItem
          Action = actOptionsRegexpSearch
        end
      end
    end
    object tbGlobalFind: TTBToolbar
      Left = 0
      Top = 23
      Caption = 'Global Find Toolbar'
      CloseButton = False
      DockPos = -16
      DockRow = 1
      Images = ilGlobalFind
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object tbFindStart: TTBItem
        Action = actFindStart
      end
      object tbFindStop: TTBItem
        Action = actFindStop
      end
      object tbFindSep1: TTBSeparatorItem
      end
      object sbFindOpen: TTBItem
        Action = actFindOpen
      end
      object tbFindSep2: TTBSeparatorItem
      end
      object sbFindClose: TTBItem
        Action = actFindClose
      end
    end
    object tbSearchFor: TTBToolWindow
      Left = 114
      Top = 23
      Caption = 'Global Search For'
      CloseButton = False
      ClientAreaHeight = 22
      ClientAreaWidth = 294
      DockPos = 32
      DockRow = 1
      ParentShowHint = False
      Resizable = False
      ShowHint = True
      TabOrder = 2
      object lblSearchFor: TLabel
        Left = 8
        Top = 5
        Width = 55
        Height = 13
        Caption = '&Search For:'
        FocusControl = cbSearchFor
      end
      object cbSearchFor: TComboBox
        Left = 68
        Top = 1
        Width = 221
        Height = 21
        Hint = 'Enter the search term here'
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object tdbLeft: TTBDock
    Left = 0
    Top = 49
    Width = 9
    Height = 397
    Position = dpLeft
  end
  object tdbBottom: TTBDock
    Left = 0
    Top = 478
    Width = 559
    Height = 9
    Position = dpBottom
  end
  object tdbRight: TTBDock
    Left = 550
    Top = 49
    Width = 9
    Height = 397
    Position = dpRight
  end
  object sbGlobalFind: TStatusBar
    Left = 0
    Top = 487
    Width = 559
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 310
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object lbHits: TListBox
    Left = 9
    Top = 49
    Width = 541
    Height = 397
    Align = alClient
    ItemHeight = 13
    ParentShowHint = False
    PopupMenu = popHits
    ShowHint = True
    TabOrder = 5
    OnDblClick = actFindOpenExecute
    OnMouseMove = lbHitsMouseMove
  end
  object pbGuides: TProgressBar
    Left = 0
    Top = 446
    Width = 559
    Height = 16
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 6
  end
  object pbCurrentGuide: TProgressBar
    Left = 0
    Top = 462
    Width = 559
    Height = 16
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 7
  end
  object NGFind: TwegLibNGFind
    NortonGude = NortonGuide
    OnStartSearch = NGFindStartSearch
    OnFinishedSearch = NGFindFinishedSearch
    OnNewGuide = NGFindNewGuide
    OnNewEntry = NGFindNewEntry
    OnHit = NGFindHit
    OnBadRegExp = NGFindBadRegExp
    SearchStyle = [wlfsShorts, wlfsLongs]
    RegExpSearch = False
    MatchCase = False
    Left = 56
    Top = 104
  end
  object alGlobalFind: TActionList
    Images = ilGlobalFind
    OnUpdate = alGlobalFindUpdate
    Left = 24
    Top = 72
    object actFindStart: TAction
      Caption = '&Start'
      Hint = 'Start the global find operation'
      ImageIndex = 1
      ShortCut = 16454
      OnExecute = actFindStartExecute
      OnUpdate = actFindStartUpdate
    end
    object actFindStop: TAction
      Caption = 'S&top'
      Hint = 'Stop the global find operation'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actFindStopExecute
      OnUpdate = actFindStopUpdate
    end
    object actFindOpen: TAction
      Caption = '&Open'
      Hint = 'Open the entry containing the highlighted result'
      ImageIndex = 3
      ShortCut = 13
      OnExecute = actFindOpenExecute
      OnUpdate = actFindOpenUpdate
    end
    object actFindClearResults: TAction
      Caption = '&Clear Results'
      Hint = 'Clear the results'
      OnExecute = actFindClearResultsExecute
      OnUpdate = actFindClearResultsUpdate
    end
    object actFindClose: TAction
      Caption = '&Close'
      Hint = 'Close the global find window'
      ImageIndex = 0
      ShortCut = 16499
      OnExecute = actFindCloseExecute
    end
    object actOptionsSearchCurrentGuide: TAction
      Caption = '&Current Guide'
      Checked = True
      Hint = 'Search the currently focused guide'
      OnExecute = actOptionsSearchCurrentGuideExecute
      OnUpdate = actOptionsSearchCurrentGuideUpdate
    end
    object actOptionsSearchAllKnownGuides: TAction
      Caption = '&All Known Guides'
      Hint = 'Search all known Norton Guides'
      OnExecute = actOptionsSearchAllKnownGuidesExecute
    end
    object actOptionsLookInShorts: TAction
      Caption = '&Short Entries'
      Hint = 'Look in short entries'
      OnExecute = actOptionsLookInShortsExecute
      OnUpdate = actOptionsLookInShortsUpdate
    end
    object actOptionsLookInLongs: TAction
      Caption = '&Long Entries'
      Hint = 'Look in long entries'
      OnExecute = actOptionsLookInLongsExecute
      OnUpdate = actOptionsLookInLongsUpdate
    end
    object actOptionsMatchCase: TAction
      Caption = '&Match Case'
      Hint = 'Match case when searching'
      OnExecute = actOptionsMatchCaseExecute
      OnUpdate = actOptionsMatchCaseUpdate
    end
    object actOptionsRegexpSearch: TAction
      Caption = '&Regular Expression Search'
      Hint = 'Perform regular expression searches'
      OnExecute = actOptionsRegexpSearchExecute
      OnUpdate = actOptionsRegexpSearchUpdate
    end
  end
  object ilGlobalFind: TTBImageList
    Left = 56
    Top = 72
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080000000FF000000FF000000FF000000FF000000FF00000080
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080000000FF000000FF000000FF000000FF000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      80000080800000000000000000000000000000000000000000000000000000FF
      000000FF000000FF00000080000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      80000080800000000000000000000000000000000000000000000080000000FF
      000000FF000000FF0000008000000080000000FF000000FF000000FF000000FF
      000000FF000000FF000000800000000000000000000000000000000080000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000008000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000800000008000000080000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000000000000000FF000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF00FFFF
      FF000000FF000000FF000000FF000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF00000080000000800000008000000080000000FF000000FF
      000000FF000000FF000000FF00000000000000000000000000000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF00000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF0000008000000080000000800000008000000080000000FF
      000000FF000000FF000000FF00000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF00000080000000800000008000000080000000FF000000FF
      000000FF000000FF000000FF00000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000800000008000000080000000FF000000FF000000FF
      000000FF000000FF000000FF00000000000000000000000000000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000000000000000000000000000000000000080000000FF
      000000FF000000FF0000008000000080000000FF000000FF000000FF000000FF
      000000FF000000FF000000800000000000000000000000000000000080000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF00FFFF
      FF000000FF000000FF0000008000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000008000000000000000000000000000000000FF
      000000FF000000FF00000080000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000080000000000000000000000000000000000000000000
      0000000000000080000000FF000000FF000000FF000000FF000000FF00000080
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080000000FF000000FF000000FF000000FF000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFC1FFC1FFFFFFFFFF007F007FFFF
      8003E003E003001F8003C001C001000F8003C001C00100078003800080000003
      80038000800000018003800080000000800380008000001F800380008000001F
      8003C001C001001FC1FEC001C0018FF1E3FEE003E003FFF9FFF5F007F007FF75
      FFF3FC1FFC1FFF8FFFF1FFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object NortonGuide: TwegLibNortonGuide
    OEMToANSI = False
    Settings = frmMain.NGSettings
    Left = 24
    Top = 104
  end
  object popHits: TTBPopupMenu
    Images = ilGlobalFind
    Left = 88
    Top = 72
    object popHitsOpenResult: TTBItem
      Action = actFindOpen
    end
    object popHitsClearResults: TTBItem
      Action = actFindClearResults
    end
  end
end
