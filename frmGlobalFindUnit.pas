{* File.......: frmGlobalFindUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Global finder.
 * Licence....: GNU General Public Licence (see below)
 *
 * WEG - Norton Guide Reader for Windows.
 * Copyright (C) 2003 Dave Pearson <davep@davep.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the license, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *}

Unit frmGlobalFindUnit;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  TB2Dock,
  StdCtrls,
  wegLibNGFind,
  ActnList,
  TB2Item,
  TB2Toolbar,
  ImgList,
  TB2ToolWindow,
  wegLibNortonGuide,
  wegLibNGFindThread, Menus;

Type

  {** Global finder form }
  TfrmGlobalFind = Class( TForm )
    tdbTop: TTBDock;
    tdbLeft: TTBDock;
    tdbBottom: TTBDock;
    tdbRight: TTBDock;
    sbGlobalFind: TStatusBar;
    lbHits: TListBox;
    NGFind: TwegLibNGFind;
    alGlobalFind: TActionList;
    actFindStart: TAction;
    actFindStop: TAction;
    actFindClose: TAction;
    tbGlobalFindMenu: TTBToolbar;
    mnuFind: TTBSubmenuItem;
    mnuFindStart: TTBItem;
    mnuFindStop: TTBItem;
    mnuFindSplit1: TTBSeparatorItem;
    mnuFindClose: TTBItem;
    mnuOptions: TTBSubmenuItem;
    ilGlobalFind: TTBImageList;
    pbGuides: TProgressBar;
    pbCurrentGuide: TProgressBar;
    tbGlobalFind: TTBToolbar;
    tbSearchFor: TTBToolWindow;
    cbSearchFor: TComboBox;
    lblSearchFor: TLabel;
    sbFindClose: TTBItem;
    tbFindStart: TTBItem;
    tbFindStop: TTBItem;
    tbFindSep1: TTBSeparatorItem;
    NortonGuide: TwegLibNortonGuide;
    actOptionsSearchCurrentGuide: TAction;
    actOptionsSearchAllKnownGuides: TAction;
    mnuOptionsSearchCurrent: TTBItem;
    mnuOptionsSearchAllKnown: TTBItem;
    mnuOptionsSearch: TTBSubmenuItem;
    actFindClearResults: TAction;
    mnuFindSplit2: TTBSeparatorItem;
    mnuFindClearResults: TTBItem;
    actOptionsLookInShorts: TAction;
    actOptionsLookInLongs: TAction;
    mnuSearchLookIn: TTBSubmenuItem;
    mnuOptionsLookInShorts: TTBItem;
    mnuOptionsLookInLongs: TTBItem;
    actOptionsRegexpSearch: TAction;
    mnuOptionsRegexp: TTBItem;
    actFindOpen: TAction;
    tbFindSep2: TTBSeparatorItem;
    sbFindOpen: TTBItem;
    mnuFindSplit3: TTBSeparatorItem;
    mnuFindOpen: TTBItem;
    actOptionsMatchCase: TAction;
    mnuOptionsMatchCase: TTBItem;
    popHits: TTBPopupMenu;
    popHitsClearResults: TTBItem;
    popHitsOpenResult: TTBItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actFindCloseExecute(Sender: TObject);
    procedure actFindStartUpdate(Sender: TObject);
    procedure actFindStopUpdate(Sender: TObject);
    procedure actFindStartExecute(Sender: TObject);
    procedure actFindStopExecute(Sender: TObject);
    procedure alGlobalFindUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actOptionsSearchCurrentGuideUpdate(Sender: TObject);
    procedure actOptionsSearchCurrentGuideExecute(Sender: TObject);
    procedure actOptionsSearchAllKnownGuidesExecute(Sender: TObject);
    procedure NGFindFinishedSearch(Sender: TObject);
    procedure NGFindStartSearch(Sender: TObject);
    procedure NGFindNewGuide(Sender: TObject);
    procedure NGFindHit(Sender: TObject; rHit: TwegLibNGFindHit);
    procedure actFindClearResultsUpdate(Sender: TObject);
    procedure actFindClearResultsExecute(Sender: TObject);
    procedure NGFindNewEntry(Sender: TObject);
    procedure actOptionsLookInShortsUpdate(Sender: TObject);
    procedure actOptionsLookInLongsUpdate(Sender: TObject);
    procedure actOptionsLookInShortsExecute(Sender: TObject);
    procedure actOptionsLookInLongsExecute(Sender: TObject);
    procedure actOptionsRegexpSearchUpdate(Sender: TObject);
    procedure actOptionsRegexpSearchExecute(Sender: TObject);
    procedure actFindOpenUpdate(Sender: TObject);
    procedure actFindOpenExecute(Sender: TObject);
    procedure actOptionsMatchCaseUpdate(Sender: TObject);
    procedure actOptionsMatchCaseExecute(Sender: TObject);
    procedure NGFindBadRegExp(Sender: TObject);

  Protected

    {** Array of results }
    aResults : Array Of TwegLibNGFindHit;

    {** Save the state of the window }
    Procedure saveWindowState;
    {** Load the state of the window }
    Procedure loadWindowState;

  End;

Var
  frmGlobalFind : TfrmGlobalFind;

Implementation

Uses
  Registry,
  wegUtils,
  frmMainUnit,
  frmGuideManagerUnit;
  
Const
  {** Registry key for the global finder data }
  REG_FINDER_WINDOW = 'Global Finder';
  {** Key name for the global finder's position data }
  REG_POSITION = 'Position';
  {** Key name for the global finder's toolbar positions }
  REG_TOOLBARS = 'Toolbars';
  {** Key name for the history }
  REG_HISTORY = 'History';
  {** Value name of the search history }
  REG_SEARCH = 'Search';
  {** Key name for the options }
  REG_OPTIONS = 'Options';
  {** Value name for the search current guide option }
  REG_OPTION_SEARCH_CURRENT = 'Search Current Guide';
  {** Value name for the search all known guides option }
  REG_OPTION_SEARCH_ALL = 'Search All Known Guides';
  {** Value name for the long entry search option }
  REG_OPTION_DO_LONGS = 'Look In Long Entries';
  {** Value name for the sort entry search option }
  REG_OPTION_DO_SHORTS = 'Look In Short Entries';
  {** Value name for the regular expression search option }
  REG_OPTION_REGEXP = 'Do Regular Expression';
  {** Value name for the match case option }
  REG_OPTION_MATCH_CASE = 'Match Case';

{$R *.DFM}

/////

Procedure TfrmGlobalFind.saveWindowState;
Var
  oReg : TRegistry;
Begin

  // Only bother saving stuff if we're visible
  If Visible Then
  Begin

    oReg := TRegistry.create();

    With oReg Do
      Try

        // Open the registry key for remembering the window position. Create
        // it if it doesn't exist.
        If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_POSITION ] ), True ) Then
          Try
            // Save the size and location of the window.
            wegSaveWindowState( self, oReg );
          Finally
            // Close the window position/size key.
            closeKey();
          End;

        // Open the registry key for remembering the history. Create it if it
        // doesn't exist.
        If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_HISTORY ] ), True ) THen
          Try
            // Save the search term history.
            writeString( REG_SEARCH, cbSearchFor.Items.CommaText );
          Finally
            // Close the window history key.
            closeKey();
          End;

        // Open the registry key for remembering the options. Create it if it
        // doesn't exist.
        If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_OPTIONS ] ), True ) THen
          Try
            // Save the options.
            writeBool( REG_OPTION_SEARCH_CURRENT, actOptionsSearchCurrentGuide.Checked );
            writeBool( REG_OPTION_SEARCH_ALL,     actOptionsSearchAllKnownGuides.Checked );
            writeBool( REG_OPTION_DO_SHORTS,      wlfsShorts In NGFind.SearchStyle );
            writeBool( REG_OPTION_DO_LONGS,       wlfsLongs  In NGFind.SearchStyle );
            writeBool( REG_OPTION_REGEXP,         NGFind.RegExpSearch );
            writeBool( REG_OPTION_MATCH_CASE,     NGFind.MatchCase );
          Finally
            // Close the window history key.
            closeKey();
          End;

        // Save the toolbar positions.
        TBRegSavePositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_FINDER_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

      Finally
        // Free the registry object.
        free();
      End;

  End;

End;

/////

Procedure TfrmGlobalFind.loadWindowState;
Var
  oReg : TRegistry;
Begin

  oReg := TRegistry.create();

  With oReg Do
    Try

      // Open the registry key for remembering the window position.
      If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_POSITION ] ), False ) Then
        Try
          // Try and load the size and location of the window.
          wegRestoreWindowState( self, oReg );
        Finally
          // Close the window position/size key.
          closeKey();
        End;

      // Open the registry key for remembering the history. 
      If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_HISTORY ] ), False ) THen
        Try
          // Save the search term history.
          Try
            cbSearchFor.Items.CommaText := readString( REG_SEARCH );
          Except
            cbSearchFor.Items.Text := '';
          End;
        Finally
          // Close the window history key.
          closeKey();
        End;

      // Open the registry key for remembering the options.
      If openKey( wegRegistryKey( [ REG_FINDER_WINDOW, REG_OPTIONS ] ), True ) THen
        Try
          // Load the options.
          Try
            actOptionsSearchCurrentGuide.Checked   := readBool( REG_OPTION_SEARCH_CURRENT );
            actOptionsSearchAllKnownGuides.Checked := readBool( REG_OPTION_SEARCH_ALL );
            If readBool( REG_OPTION_DO_SHORTS ) Then
              NGFind.SearchStyle := NGFind.SearchStyle + [ wlfsShorts ]
            Else
              NGFind.SearchStyle := NGFind.SearchStyle - [ wlfsShorts ];
            If readBool( REG_OPTION_DO_LONGS ) Then
              NGFind.SearchStyle := NGFind.SearchStyle + [ wlfsLongs ]
            Else
              NGFind.SearchStyle := NGFind.SearchStyle - [ wlfsLongs ];
            NGFind.RegExpSearch := readBool( REG_OPTION_REGEXP );
            NGFInd.MatchCase    := readBool( REG_OPTION_MATCH_CASE );
          Except
            // GNDN.
          End;
        Finally
          // Close the window history key.
          closeKey();
        End;

      // Load the toolbar positions.
      TBRegLoadPositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_FINDER_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

    Finally
      // Free the registry object.
      free();
    End;

End;

/////

Procedure TfrmGlobalFind.FormShow( Sender : TObject );
Begin

  // Load the window state.
  loadWindowState();

  // Use the guide font for the hits list.
  lbHits.Font := frmMain.NGSettings.GuideFont;
  
End;

/////

Procedure TfrmGlobalFind.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin
  saveWindowState();
End;

/////

Procedure TfrmGlobalFind.actFindCloseExecute( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmGlobalFind.actFindStartUpdate( Sender : TObject );
Begin
  actFindStart.Enabled := ( Not NGFind.Finding )     And
                          ( cbSearchFor.Text <> '' ) And
                          ( ( actOptionsSearchCurrentGuide.Checked And
                              actOptionsSearchCurrentGuide.Enabled ) Or
                            actOptionsSearchAllKnownGuides.Checked );
End;

/////

Procedure TfrmGlobalFind.actFindStopUpdate( Sender : TObject );
Begin
  actFindStop.Enabled := NGFind.Finding;
End;

/////

Procedure TfrmGlobalFind.actFindStartExecute( Sender : TObject );
Var
  slGuides : TStringList;
  i        : Integer;
  sFind    : String;
Begin

  // Create the string list to hold the list of guids to search.
  slGuides := TStringList.create();

  With TwegBusyCursor.create() Do
    Try

      // If the user is searching the current guide...
      If actOptionsSearchCurrentGuide.Checked Then
      Begin
        // ...use only that...
        If frmMain.focusedGuide() <> Nil Then
          slGuides.add( frmMain.focusedGuide().Guide );
      End
      Else
        // ...otherwise get the guide list from the guide manager.
        frmGuideManager.getGuideNames( slGuides );

      // Remember what we're looking for.
      sFind := cbSearchFor.Text;

      // Start the search.
      NGFind.start( sFind, slGuides );

      // Remember what we've looked for.
      i := cbSearchFor.Items.indexOf( sFind );
      If i > -1 Then
        // The text is in the list, delete it from its current location.
        cbSearchFor.Items.delete( i )
      // Reached the max memory?
      Else If cbSearchFor.Items.Count = 20 Then
        // Delete the last item on the list.
        cbSearchFor.Items.delete( cbSearchFor.Items.Count - 1 );

      // Put the search term at the top of the memory.
      cbSearchFor.Items.insert( 0, sFind );
      cbSearchFor.Text := sFind;

    Finally
      slGuides.free();
      free();
    End;
  
End;

/////

Procedure TfrmGlobalFind.actFindStopExecute( Sender : TObject );
Begin

  With TwegBusyCursor.create() Do
    Try
      NGFind.stop();
    Finally
      free();
    End;
    
End;

/////

Procedure TfrmGlobalFind.alGlobalFindUpdate( Action : TBasicAction; Var Handled : Boolean );
Begin
  lblSearchFor.Enabled := Not NGFind.Finding;
  cbSearchFor.Enabled  := Not NGFind.Finding;
End;

/////

Procedure TfrmGlobalFind.actOptionsSearchCurrentGuideUpdate( Sender : TObject );
Begin
  actOptionsSearchCurrentGuide.Enabled := ( frmMain.focusedGuide() <> Nil );
End;

/////

Procedure TfrmGlobalFind.actOptionsSearchCurrentGuideExecute( Sender : TObject );
Begin
  actOptionsSearchCurrentGuide.Checked   := True;
  actOptionsSearchAllKnownGuides.Checked := False;
End;

/////

Procedure TfrmGlobalFind.actOptionsSearchAllKnownGuidesExecute( Sender : TObject );
Begin
  actOptionsSearchCurrentGuide.Checked   := False;
  actOptionsSearchAllKnownGuides.Checked := True;
End;

/////

Procedure TfrmGlobalFind.NGFindFinishedSearch( Sender : TObject );
ResourceString
  RSFinished = 'Search finished';
Begin

  // Update the status bar.
  sbGlobalFind.SimpleText := RSFinished;

  // Reset the progress bars.
  pbGuides.Position       := 0;
  pbCurrentGuide.Position := 0;
  
End;

/////

Procedure TfrmGlobalFind.NGFindStartSearch( Sender : TObject );
ResourceString
  RSFinished = 'Search started';
Begin

  // Update the status bar.
  sbGlobalFind.SimpleText := RSFinished;

  // Size the guides progress bar.
  pbGuides.Min := 0;
  pbGuides.Max := NGFind.GuideCount;
  
End;

/////

Procedure TfrmGlobalFind.NGFindNewGuide( Sender : TObject );
ResourceString
  RSNewGuide = 'Searching "%s"';
Begin

  // Update the status bar.
  sbGlobalFind.SimpleText := Format( RSNewGuide, [ NortonGuide.Title ] );

  // Update the guide progress bar.
  pbGuides.Position := NGFind.CurrentGuide + 1;

  // Size the current guide progress bar.
  pbCurrentGuide.Min := 0;
  pbCurrentGuide.Max := NortonGuide.Size;
  
End;

/////

Procedure TfrmGlobalFind.NGFindHit( Sender : TObject; rHit : TwegLibNGFindHit );
Begin

  // Add a hit record to the array
  SetLength( aResults, Length( aResults ) + 1 );
  aResults[ Length( aResults ) - 1 ] := rHit;

  // Add the next of the hit to the list box.
  lbHits.Items.add( rHit.sText );
  
End;

/////

Procedure TfrmGlobalFind.actFindClearResultsUpdate( Sender : TObject );
Begin
  actFindClearResults.Enabled := lbHits.Items.Count > 0;
End;

/////

Procedure TfrmGlobalFind.actFindClearResultsExecute( Sender : TObject );
Begin
  // Clear the content of the list box.
  lbHits.Items.clear();
  // Set the length of the results array to zero.
  SetLength( aResults, 0 ); 
End;

/////

Procedure TfrmGlobalFind.NGFindNewEntry( Sender : TObject );
Begin
  // Update the progress bar.
  pbCurrentGuide.Position := NortonGuide.CurrentOffset;
End;

/////

Procedure TfrmGlobalFind.actOptionsLookInShortsUpdate( Sender : TObject );
Begin
  actOptionsLookInShorts.Checked := ( wlfsShorts In NGFind.SearchStyle );
End;

/////

Procedure TfrmGlobalFind.actOptionsLookInLongsUpdate( Sender : TObject );
Begin
  actOptionsLookInLongs.Checked := ( wlfsLongs In NGFind.SearchStyle );
End;

/////

Procedure TfrmGlobalFind.actOptionsLookInShortsExecute( Sender : TObject );
Begin

  If wlfsShorts In NGFind.SearchStyle Then
    NGFind.SearchStyle := NGFind.SearchStyle - [ wlfsShorts ]
  Else
    NGFind.SearchStyle := NGFind.SearchStyle + [ wlfsShorts ];

End;

/////

Procedure TfrmGlobalFind.actOptionsLookInLongsExecute( Sender : TObject );
Begin

  If wlfsLongs In NGFind.SearchStyle Then
    NGFind.SearchStyle := NGFind.SearchStyle - [ wlfsLongs ]
  Else
    NGFind.SearchStyle := NGFind.SearchStyle + [ wlfsLongs ];
      
End;

/////

Procedure TfrmGlobalFind.actOptionsRegexpSearchUpdate( Sender : TObject );
Begin
  actOptionsRegexpSearch.Enabled := NGFind.CanRegExp;
  actOptionsRegexpSearch.Checked := NGFind.CanRegExp And NGFind.RegExpSearch;
End;

/////

Procedure TfrmGlobalFind.actOptionsRegexpSearchExecute( Sender : TObject );
Begin
  NGFind.RegExpSearch := Not NGFind.RegExpSearch;
End;

/////

Procedure TfrmGlobalFind.actFindOpenUpdate( Sender : TObject );
Begin
  actFindOpen.Enabled := ( lbHits.ItemIndex > -1 );
End;

/////

Procedure TfrmGlobalFind.actFindOpenExecute( Sender : TObject );
Begin

  If lbHits.ItemIndex > -1 Then
    With aResults[ lbHits.ItemIndex ] Do
      frmMain.openGuide( sGuide, lEntry, iLine );

End;

/////

Procedure TfrmGlobalFind.actOptionsMatchCaseUpdate( Sender : TObject );
Begin
  actOptionsMatchCase.Checked := NGFind.MatchCase;
End;

/////

Procedure TfrmGlobalFind.actOptionsMatchCaseExecute( Sender : TObject );
Begin
  NGFind.MatchCase := Not NGFind.MatchCase;
End;

/////

Procedure TfrmGlobalFind.NGFindBadRegExp( Sender : TObject );
ResourceString
  RSBadRegExp = 'Invalid regular expression';
Begin

  // Delete the item from the history.
  cbSearchFor.Items.delete( 0 );

  // Moan.
  MessageBeep( MB_ICONERROR );
  MessageDlg( RSBadRegExp, mtError, [ mbOk ], 0 );
  
End;

End.
