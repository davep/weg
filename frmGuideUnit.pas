{* File.......: frmGuideUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Guide form.
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

Unit frmGuideUnit;

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
  wegLibNortonGuide,
  Menus,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  StdCtrls,
  wegLibNGEntryViewer,
  ImgList,
  ActnList,
  wegLibNGEntryFind,
  frmGuideEntryFindUnit;

Type

  {** Guide form }
  TfrmGuide = Class( TForm )
    sbGuide: TStatusBar;
    NortonGuide: TwegLibNortonGuide;
    tbdTop: TTBDock;
    tbdLeft: TTBDock;
    tbdBottom: TTBDock;
    tbdRight: TTBDock;
    tbGuideMenu: TTBToolbar;
    NGEntry: TwegLibNGEntryViewer;
    ilGuide: TImageList;
    tbWindowMenu: TTBToolbar;
    mnuNavigate: TTBSubmenuItem;
    alGuide: TActionList;
    actNavigatePrevious: TAction;
    mnuNavigatePrevious: TTBItem;
    actNavigateNext: TAction;
    mnuNavigateNext: TTBItem;
    actNavigateUp: TAction;
    actNavigateDown: TAction;
    mnuNavigateDown: TTBItem;
    mnuNavigateUp: TTBItem;
    tbNavigate: TTBToolbar;
    sbNavigatePrevious: TTBItem;
    sbNavigateDown: TTBItem;
    sbNavigateUp: TTBItem;
    sbNavigateNext: TTBItem;
    mnuGuide: TTBSubmenuItem;
    actGuideClose: TAction;
    mnuGuideClose: TTBItem;
    tbGuide: TTBToolbar;
    sbGuideClose: TTBItem;
    mnuEdit: TTBSubmenuItem;
    actGuideSaveText: TAction;
    actGuideSaveSource: TAction;
    mnuGuideSaveText: TTBItem;
    mnuGuideSaveSource: TTBItem;
    mnuGuideSplit1: TTBSeparatorItem;
    sbGuideSaveText: TTBItem;
    sbGuideSplit1: TTBSeparatorItem;
    sbGuideSaveSource: TTBItem;
    actGuideCredits: TAction;
    mnuGuideCredits: TTBItem;
    mnuGuideSplit2: TTBSeparatorItem;
    sbGuideCredits: TTBItem;
    sbGuideSplit2: TTBSeparatorItem;
    sdText: TSaveDialog;
    sdSource: TSaveDialog;
    actEditCopyText: TAction;
    actEditCopySource: TAction;
    mnuEditCopyText: TTBItem;
    mnuEditCopySource: TTBItem;
    tbEdit: TTBToolbar;
    sbEditCopyText: TTBItem;
    sbEditCopySource: TTBItem;
    actNavigateBacktrack: TAction;
    actNavigateForetrack: TAction;
    mnuNavigateSplit1: TTBSeparatorItem;
    mnuNavigateBacktrack: TTBItem;
    mnuNavigateForetrack: TTBItem;
    sbNavigateSplit1: TTBSeparatorItem;
    sbNavigateBacktrack: TTBItem;
    sbNavigateForetrack: TTBItem;
    popGuide: TTBPopupMenu;
    popGuideCopySource: TTBItem;
    popGuideCopyText: TTBItem;
    popGuideSplit1: TTBSeparatorItem;
    popGuideSaveText: TTBItem;
    popGuideSaveSource: TTBItem;
    popGuideSplit3: TTBSeparatorItem;
    popGuideCredits: TTBItem;
    popGuideSplit4: TTBSeparatorItem;
    popGuideClose: TTBItem;
    popGuideSeeAlso: TTBSubmenuItem;
    popGuideSplit2: TTBSeparatorItem;
    NGEntryFind: TwegLibNGEntryFind;
    actSearchFind: TAction;
    mnuSearch: TTBSubmenuItem;
    mnuSearchFind: TTBItem;
    actSearchFindAgain: TAction;
    mnuSearchFindAgain: TTBItem;
    tbSearch: TTBToolbar;
    sbSearchFind: TTBItem;
    sbSearchFindAgain: TTBItem;
    pdGuide: TPrintDialog;
    actGuidePrint: TAction;
    mnuGuideSplit3: TTBSeparatorItem;
    mnuGuidePrint: TTBItem;
    sbGuideSplit3: TTBSeparatorItem;
    tbGuidePrint: TTBItem;
    popGuideSplit5: TTBSeparatorItem;
    popGuidePrint: TTBItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NortonGuideOpen(Sender: TObject);
    procedure NGEntryNewEntry(Sender: TObject);
    procedure NGEntryInvalidLink(Sender: TObject);
    procedure NortonGuideError(Sender: TObject; const sError: String);
    procedure actNavigatePreviousUpdate(Sender: TObject);
    procedure actNavigatePreviousExecute(Sender: TObject);
    procedure actNavigateNextExecute(Sender: TObject);
    procedure actNavigateNextUpdate(Sender: TObject);
    procedure actNavigateUpUpdate(Sender: TObject);
    procedure actNavigateUpExecute(Sender: TObject);
    procedure actNavigateDownUpdate(Sender: TObject);
    procedure actNavigateDownExecute(Sender: TObject);
    procedure actGuideCloseExecute(Sender: TObject);
    procedure actGuideSaveTextExecute(Sender: TObject);
    procedure actGuideSaveSourceExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actGuideCreditsExecute(Sender: TObject);
    procedure actGuideSaveTextUpdate(Sender: TObject);
    procedure actGuideSaveSourceUpdate(Sender: TObject);
    procedure actEditCopyTextUpdate(Sender: TObject);
    procedure actEditCopySourceUpdate(Sender: TObject);
    procedure actEditCopyTextExecute(Sender: TObject);
    procedure actEditCopySourceExecute(Sender: TObject);
    procedure actNavigateBacktrackUpdate(Sender: TObject);
    procedure actNavigateForetrackUpdate(Sender: TObject);
    procedure actNavigateBacktrackExecute(Sender: TObject);
    procedure actNavigateForetrackExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure NGEntryFindHit(Sender: TObject; iLine: Integer);
    procedure NGEntryFindMiss(Sender: TObject);
    procedure actSearchFindAgainUpdate(Sender: TObject);
    procedure actSearchFindAgainExecute(Sender: TObject);
    procedure NGEntryClick(Sender: TObject);
    procedure NGEntryFindBadRegExp(Sender: TObject);
    procedure actGuidePrintExecute(Sender: TObject);
    procedure NGEntryKeyPress(Sender: TObject; var Key: Char);
    procedure actGuidePrintUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  Protected

    {** Pointer to the see-also menu item }
    mnuSeeAlso : TTBSubmenuItem;
    {** Pointer to the guide type panel }
    pnlGuideType : TStatusPanel;
    {** Pointer to the guide name panel }
    pnlGuideName : TStatusPanel;
    {** Pointer to the guide title panel }
    pnlGuideTitle : TStatusPanel;
    {** Pointer to the finder form. }
    frmFinder : TfrmGuideEntryFind;
    {** Pointer to a regular expression object for finding URLs }
    oURLRegExp : Variant;
    {** Can we look for URLs and add them to the see-also list? }
    bCanURLSeeAlso : Boolean;
    {** String list for holding URLs found in a guide entry }
    slURLs : TStringList;

    {** Refresh the window title }
    Procedure refreshTitle;
    {** Refresh the status bar }
    Procedure refreshStatusBar;
    {** Refresh the menu }
    Procedure refreshMenu;
    {** Refresh the see-also menu }
    Procedure refreshSeeAlsoMenu;
    {** Populate the window's menu }
    Procedure populateMenu;
    {** Handle a menu click }
    Procedure menuClick( Sender : TObject );
    {** Handle a see-also menu click }
    Procedure seeAlsoClick( Sender : TObject );
    {** Handle a see-also URL menu click }
    Procedure seeAlsoURLClick( Sender : TObject );
    {** Handle a "find" click in the finder dialog }
    Procedure findClick( Sender : TObject );
    {** Look for URLs in the current entry and populate the list }
    Procedure populateURLList;

  End;

Implementation

Uses
  ClipBrd,
  ComObj,
  Printers,
  wegLibUtils,
  wegLibNGLineParser,
  wegUtils,
  frmMainUnit,
  frmGuideCreditsUnit;

{$R *.DFM}

/////

Procedure TfrmGuide.refreshTitle;
Begin
  Caption      := NortonGuide.Title;
  NGEntry.Hint := ExpandUNCFileName( NortonGuide.Guide );
End;

/////

Procedure TfrmGuide.refreshStatusBar;
Begin
  pnlGuideTitle.Text := NGEntry.entryPath();
End;

/////

Procedure TfrmGuide.refreshMenu;
Var
  iMenu   : Integer;
  iPrompt : Integer;
  i       : Integer;
  j       : Integer;
Begin

  // Figure out which menu option we're hanging off.
  iMenu   := NGEntry.Entry.ParentMenu;
  iPrompt := NGEntry.Entry.ParentMenuPrompt;

  // Run thru the menus (un)setting the checked state.
  For i := 0 To tbGuideMenu.Items.Count - 1 Do
    For j := 0 To tbGuideMenu.Items[ i ].Count - 1 Do
      tbGuideMenu.Items[ i ][ j ].Checked := ( ( i = iMenu ) And ( j = iPrompt ) );

End;

/////

Procedure TfrmGuide.refreshSeeAlsoMenu;

  Function SetupSeeAlsoItem( oItem : TTBItem; i : Integer; Const sCaption : String; pOnClick : TNotifyEvent ) : TTBItem;
  ResourceString
    RSSeeAlso = 'See Also "%s"';
  Begin

    With oItem Do
    Begin
      Caption := StringReplace( sCaption, '&', '&&', [ rfReplaceAll ] );
      OnClick := pOnClick;
      Hint    := Format( RSSeeAlso, [ sCaption ] );
      Tag     := i;
    End;

    Result := oItem;
    
  End;

Var
  i : Integer;
Begin

  // Clear the content of the see-also menu.
  mnuSeeAlso.clear();
  popGuideSeeAlso.clear();

  // If the new entry has a see also...
  If NGEntry.Entry.HasSeeAlso Then
    // ...populate the menu.
    For i := 0 To NGEntry.Entry.SeeAlso.Count - 1 Do
    Begin
      mnuSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( mnuSeeAlso ), i, NGEntry.Entry.SeeAlso[ i ], seeAlsoClick ) );
      popGuideSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( popGuideSeeAlso ), i, NGEntry.Entry.SeeAlso[ i ], seeAlsoClick ) );
    End;

  // Now that we've done the real see-also items, add any URLs.
  If slURLs.Count > 0 Then
  Begin

    // If there are some real see-also entries in the menu, add a split line.
    If mnuSeeAlso.Count > 0 Then
    Begin
      mnuSeeAlso.add( TTBSeparatorItem.create( mnuSeeAlso ) );
      popGuideSeeAlso.add( TTBSeparatorItem.create( popGuideSeeAlso ) );
    End;
    
    For i := 0 To slURLs.Count - 1 Do
    Begin
      mnuSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( mnuSeeAlso ), i, slURLs[ i ], seeAlsoURLClick ) );
      popGuideSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( popGuideSeeAlso ), i, slURLs[ i ], seeAlsoURLClick ) );
    End;

  End;

  // Set the enabled state of the see-also menu.
  mnuSeeAlso.Enabled      := ( mnuSeeAlso.Count > 0 );
  popGuideSeeAlso.Visible := mnuSeeAlso.Enabled;
  
End;

/////

Procedure TfrmGuide.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin
  // Ensure that we're freed from memory.
  Action := caFree;
End;

/////

Procedure TfrmGuide.NortonGuideOpen( Sender : TObject );
ResourceString
  RSType = 'Guide type: %s';
Begin

  // Refresh the title of the window.
  refreshTitle();

  // Populate the window's menu.
  populateMenu();

  // Set the guide type.
  pnlGuideType.Text := NortonGuide.GuideType;

  // Have the hint of the staus panel show the long guide type.
  sbGuide.Hint := Format( RSType, [ NortonGuide.LongGuideType ] );

  // Set the guide file name.
  pnlGuideName.Text := ExtractFileName( NortonGuide.Guide );

End;

/////

Procedure TfrmGuide.populateMenu;
ResourceString
  RSSeeAlso = 'See &Also';
  RSJump    = 'Jump to the "%s" entry';
Var
  i     : Integer;
  iSubs : Integer;
Begin

  // Mark that we're doing a big update.
  tbGuideMenu.beginUpdate();

  Try

    // Ensure that the menu is clear.
    tbGuideMenu.Items.clear();

    // For each menu in the guide...
    For i := 0 To NortonGuide.MenuCount - 1 Do
    Begin

      // ...add an item to the menu for the guide menu we're looking at.
      tbGuideMenu.Items.add( TTBSubmenuItem.create( tbGuideMenu ) );

      // Configure it.
      With tbGuideMenu.Items[ tbGuideMenu.Items.Count - 1 ] Do
      Begin

        // Set the caption of the menu title.
        Caption := '&' + StringReplace( NortonGuide.Menus[ i ].Title, '&', '&&', [ rfReplaceAll ] );
        // Set the tag to the menu number.
        Tag := i;

        // Now add the sub-menu items.
        For iSubs := 0 To NortonGuide.Menus[ i ].Count - 1 Do
        Begin

          // Add a menu item to the guide menu we're working on.
          add( TTBItem.create( tbGuideMenu ) );

          // Configure it.
          With Items[ Count - 1 ] Do
          Begin
            // Set the caption of the menu title.
            Caption := StringReplace( NortonGuide.Menus[ i ][ iSubs ], '&', '&&', [ rfReplaceAll ] );
            // Set the hint text.
            Hint := Format( RSJump, [ NortonGuide.Menus[ i ][ iSubs ] ] );
            // Set the click action.
            OnClick := menuClick;
            // Set the tag to the sub-menu number.
            Tag := iSubs;
          End;

        End;

      End;

    End;

    // Finally, add a menu item onto which we can hang any see-also items.
    mnuSeeAlso := TTBSubmenuItem.create( tbGuideMenu );
    tbGuideMenu.Items.add( mnuSeeAlso );
    mnuSeeAlso.Caption := RSSeeAlso;
    mnuSeeAlso.Enabled := False;

  Finally

    // Mark that we've finished the update.
    tbGuideMenu.endUpdate();

  End;
  
End;

/////

Procedure TfrmGuide.NGEntryNewEntry( Sender : TObject );
Begin

  // Refresh the menu.
  refreshMenu();

  // Scan for any URLs.
  populateURLList();
  
  // Refresh the see also menu.
  refreshSeeAlsoMenu();

  // Refresh the status bar.
  refreshStatusBar();

  // If a search was in progress...
  If NGEntryFind.Searching Then
    // ...end the old search.
    NGEntryFind.finishSearch();

End;

/////

Procedure TfrmGuide.menuClick( Sender : TObject );
Begin

  // The sender is a TTBItem.
  With Sender As TTBItem Do
    // Jump to the entry associated with the chosen menu item.
    NGEntry.display( NortonGuide.Menus[ Parent.Tag ].Offsets[ Tag ] );

End;

/////

Procedure TfrmGuide.seeAlsoClick( Sender : TObject );
Begin

  // The sender is a TTBItem.
  With Sender As TTBItem Do
    // Jump to the entry associated with the chosen menu item.
    NGEntry.display( NGEntry.Entry.SeeAlso.Offsets[ Tag ] );

End;

/////

Procedure TfrmGuide.seeAlsoURLClick( Sender : TObject );
Begin

  // The sender is a TTBItem.
  With Sender As TTBItem Do
    // Fire off the URL at Windows.
    wegFireURL( slURLs[ Tag ] );

End;

/////

Procedure TfrmGuide.NGEntryInvalidLink( Sender : TObject );
ResourceString
  RSBadLink = 'Invalid link, unable to follow';
Begin
  MessageBeep( MB_ICONERROR );
  MessageDlg( RSBadLink, mtError, [ mbOk ], 0 );
End;

/////

Procedure TfrmGuide.NortonGuideError( Sender : TObject; Const sError : String );
Begin
  MessageBeep( MB_ICONERROR );
  MessageDlg( sError, mtError, [ mbOk ], 0 );
End;

/////

Procedure TfrmGuide.actNavigatePreviousUpdate( Sender : TObject );
Begin
  actNavigatePrevious.Enabled := NGEntry.canNavigatePrevious();
End;

/////

Procedure TfrmGuide.actNavigatePreviousExecute( Sender : TObject );
Begin
  NGEntry.navigatePrevious();
End;

/////

Procedure TfrmGuide.actNavigateNextUpdate( Sender : TObject );
Begin
  actNavigateNext.Enabled := NGEntry.canNavigateNext();
End;

/////

Procedure TfrmGuide.actNavigateNextExecute( Sender : TObject );
Begin
  NGEntry.navigateNext();
End;

/////

Procedure TfrmGuide.actNavigateUpUpdate( Sender : TObject );
Begin
  actNavigateUp.Enabled := NGEntry.canNavigateUp();
End;

/////

Procedure TfrmGuide.actNavigateUpExecute( Sender : TObject );
Begin
  NGEntry.navigateUp();
End;

/////

Procedure TfrmGuide.actNavigateDownUpdate( Sender : TObject );
Begin
  actNavigateDown.Enabled := NGEntry.canNavigateDown();
End;

/////

Procedure TfrmGuide.actNavigateDownExecute( Sender : TObject );
Begin
  NGEntry.navigateDown();
End;

/////

Procedure TfrmGuide.actGuideCloseExecute( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmGuide.actGuideSaveTextExecute( Sender : TObject );
Var
  slLines : TStringList;
Begin

  // If the user wants to save...
  If sdText.execute() Then
  Begin

    // Create a string list to hold the text.
    slLines := TStringList.create();

    Try
      // Load the text into the string list.
      NGEntry.getText( slLines );
      // Save the strings to the chosen file.
      slLines.saveToFile( sdText.FileName );
    Finally
      slLines.free();
    End;

  End;

End;

/////

Procedure TfrmGuide.actGuideSaveSourceExecute( Sender : TObject );
Var
  slSource : TStringList;
Begin

  // If the user wants to save...
  If sdSource.execute() Then
  Begin

    // Create a string list to hold the source.
    slSource := TStringList.create();

    Try
      // Load the source into the string list.
      NGEntry.Entry.getSourceLines( slSource );
      // Save the source to the chosen file.
      slSource.saveToFile( sdSource.FileName );
    Finally
      slSource.free();
    End;

  End;

End;

/////

Procedure TfrmGuide.FormCreate( Sender : TObject );
Begin

  // Give some useful names to each of the panels in the status bar.
  pnlGuideType  := sbGuide.Panels[ 0 ];
  pnlGuideName  := sbGuide.Panels[ 1 ];
  pnlGuideTitle := sbGuide.Panels[ 2 ];

  // Create the string list for holding the URLs found in an entry.
  slURLs := TStringList.create();

  // Create regexp object for looking for URLs.
  Try

    // Try and create the regular expression object for finding URLs.
    oURLRegExp := CreateOLEObject( 'VBScript.RegExp' );

    // We managed to create it, remember this.
    bCanURLSeeAlso := True;

    // Set the regular expression for funding URLs.
    oURLRegExp.Pattern := '(http|https|ftp|news|mailto|telnet|finger):[^ "''\)>\t\r\n]*';
    
  Except
    // Seems that this facility isn't available in the current environment.
    bCanURLSeeAlso := False;
  End;

End;

/////

Procedure TfrmGuide.actGuideCreditsExecute( Sender : TObject );
Begin

  With TfrmGuideCredits.create( self ) Do
    Try
      // Pass on the pointer to the guide.
      oNortonGuide := NortonGuide;
      // Show the credits dialog.
      showModal();
    Finally
      // Free it.
      free();
    End;

End;

/////

Procedure TfrmGuide.actGuideSaveTextUpdate( Sender : TObject );
ResourceString
  RSSaveCaption         = 'Save &Text...';
  RSSaveHint            = 'Save the text to a file';
  RSSaveSelectedCaption = 'Save Selected &Text...';
  RSSaveSelectedHint    = 'Save the selected text to a file';
Begin

  // Set the caption and the hint of the save text action depending on the
  // select state of the guide entry viewer.
  If NGEntry.SelCount > 1 Then
  Begin
    actGuideSaveText.Caption := RSSaveSelectedCaption;
    actGuideSaveText.Hint    := RSSaveSelectedHint;
  End
  Else
  Begin
    actGuideSaveText.Caption := RSSaveCaption;
    actGuideSaveText.Hint    := RSSaveHint;
  End;

End;

/////

Procedure TfrmGuide.actGuideSaveSourceUpdate( Sender : TObject );
ResourceString
  RSSaveCaption         = 'Save &Source...';
  RSSaveHint            = 'Save the guide source to a file';
  RSSaveSelectedCaption = 'Save Selected &Source...';
  RSSaveSelectedHint    = 'Save the selected guide source to a file';
Begin

  // Set the caption and the hint of the save source action depending on the
  // select state of the guide entry viewer.
  If NGEntry.SelCount > 1 Then
  Begin
    actGuideSaveSource.Caption := RSSaveSelectedCaption;
    actGuideSaveSource.Hint    := RSSaveSelectedHint;
  End
  Else
  Begin
    actGuideSaveSource.Caption := RSSaveCaption;
    actGuideSaveSource.Hint    := RSSaveHint;
  End;

End;

/////

Procedure TfrmGuide.actEditCopyTextUpdate( Sender : TObject );
ResourceString
  RSCopyCaption         = 'Copy &Text...';
  RSCopyHint            = 'Copy the text to the clipboard';
  RSCopySelectedCaption = 'Copy Selected &Text...';
  RSCopySelectedHint    = 'Copy the selected text to the clipboard';
Begin

  // Set the caption and the hint of the copy text action depending on the
  // select state of the guide entry viewer.
  If NGEntry.SelCount > 1 Then
  Begin
    actEditCopyText.Caption := RSCopySelectedCaption;
    actEditCopyText.Hint    := RSCopySelectedHint;
  End
  Else
  Begin
    actEditCopyText.Caption := RSCopyCaption;
    actEditCopyText.Hint    := RSCopyHint;
  End;

End;

/////

Procedure TfrmGuide.actEditCopySourceUpdate(Sender: TObject);
ResourceString
  RSCopyCaption         = 'Copy &Source...';
  RSCopyHint            = 'Copy the guide source to the clipboard';
  RSCopySelectedCaption = 'Copy Selected &Source...';
  RSCopySelectedHint    = 'Copy the selected guide source to the clipboard';
Begin

  // Set the caption and the hint of the copy source action depending on the
  // select state of the guide entry viewer.
  If NGEntry.SelCount > 1 Then
  Begin
    actEditCopySource.Caption := RSCopySelectedCaption;
    actEditCopySource.Hint    := RSCopySelectedHint;
  End
  Else
  Begin
    actEditCopySource.Caption := RSCopyCaption;
    actEditCopySource.Hint    := RSCopyHint;
  End;

End;

/////

Procedure TfrmGuide.actEditCopyTextExecute( Sender : TObject );
Var
  slLines : TStringList;
Begin

  // Create a string list to hold the text.
  slLines := TStringList.create();

  Try
    // Load the text into the string list.
    NGEntry.getText( slLines );
    // Place the text into the clipboard.
    Clipboard().AsText := slLines.Text;
  Finally
    slLines.free();
  End;

End;

/////

Procedure TfrmGuide.actEditCopySourceExecute( Sender : TObject );
Var
  slSource : TStringList;
Begin

  // Create a string list to hold the source.
  slSource := TStringList.create();

  Try
    // Load the source into the string list.
    NGEntry.getSource( slSource );
    // Place the source into the clipboard.
    Clipboard().AsText := slSource.Text;
  Finally
    slSource.free();
  End;

End;

/////

Procedure TfrmGuide.actNavigateBacktrackUpdate( Sender : TObject );
Begin
  actNavigateBacktrack.Enabled := NGEntry.canHistoryPrevious();
End;

/////

Procedure TfrmGuide.actNavigateForetrackUpdate( Sender : TObject );
Begin
  actNavigateForetrack.Enabled := NGEntry.canHistoryNext();
End;

/////

Procedure TfrmGuide.actNavigateBacktrackExecute( Sender : TObject );
Begin
  NGEntry.historyPrevious();
End;

/////

Procedure TfrmGuide.actNavigateForetrackExecute( Sender : TObject );
Begin
  NGEntry.historyNext();
End;

/////

Procedure TfrmGuide.actSearchFindExecute( Sender : TObject );
ResourceString
  RSCaption = 'Find in %s';
Begin

  // If the form hasn't been created yet...
  If frmFinder = Nil Then
  Begin

    // ..create it.
    frmFinder := TfrmGuideEntryFind.create( self );

    // Configure it.
    frmFinder.Caption                     := Format( RSCaption, [ NGEntry.entryTitle() ] );
    frmFinder.Hint                        := frmFinder.Caption;
    frmFinder.cbRegularExpression.Enabled := NGEntryFind.CanRegExp;
    frmFinder.btnFind.OnClick             := findClick;

  End;

  // Show the form.
  frmFinder.show();

End;

/////

Procedure TfrmGuide.findClick( Sender : TObject );
Begin

  // If we're not already searching this guide...
  If Not NGEntryFind.Searching Then
    // ...start the search.
    NGEntryFind.startSearch( NGEntry.Entry, NGEntry.ItemIndex );

  // Try and find.
  NGEntryFind.find( frmFinder.edtFind.Text, frmFinder.cbCaseSensitive.Checked, frmFinder.cbRegularExpression.Checked );
  
End;

/////

Procedure TfrmGuide.NGEntryFindHit( Sender : TObject; iLine : Integer );
Begin
  // Jump to the line.
  NGEntry.jumpToLine( iLine );
  // Set focus to us.
  frmMain.setFocus();
End;

/////

Procedure TfrmGuide.NGEntryFindMiss( Sender : TObject );
ResourceString
  RSMiss = 'Text not found, the search will resume from the top of the entry.';
Begin
  MessageBeep( MB_ICONEXCLAMATION );
  MessageDlg( RSMiss, mtWarning, [ mbOk ], 0 );
  NGEntryFind.goTop();
End;

/////

Procedure TfrmGuide.actSearchFindAgainUpdate( Sender : TObject );
Begin
  actSearchFindAgain.Enabled := NGEntryFind.Searching;
End;

/////

Procedure TfrmGuide.actSearchFindAgainExecute( Sender : TObject );
Begin
  If NGEntryFind.Searching Then
    findClick( Sender );
End;

/////

Procedure TfrmGuide.NGEntryClick( Sender : TObject );
Begin
  // Reset the search.
  NGEntryFind.finishSearch();
End;

/////

Procedure TfrmGuide.NGEntryFindBadRegExp( Sender : TObject );
ResourceString
  RSBadRegExp = 'Bad regular expression, please correct.';
Begin
  MessageBeep( MB_ICONERROR );
  MessageDlg( RSBadRegExp, mtError, [ mbOk ], 0 );
End;

/////

Procedure TfrmGuide.actGuidePrintExecute( Sender : TObject );
Var
  i       : Integer;
  iY      : Integer;
  slLines : TStringList;
Begin

  // Check that the user actually wants to print.
  If pdGuide.execute() Then
    With TwegBusyCursor.create() Do
      Try

        With Printer() Do
        Begin

          // Set the title of the print job.
          Title := NGEntry.entryTitle();
          
          // Start the document.
          beginDoc();

          // Initialise the font for the printer.
          Canvas.Font       := NGEntry.Font;
          Canvas.Font.Color := clBlack;
          Canvas.Font.Pitch := fpFixed;

          // Get the lines that are to be printed.
          slLines := TStringList.create();
          NGEntry.getSource( slLines );
                    
          Try

            // Starting at position 0.
            iY := 0;

            // Print each line in the entry...
            For i := 0 To slLines.Count - 1 Do
            Begin

              // Will we print off the page?
              If ( iY + Canvas.textHeight( 'X' ) ) > PageHeight Then
              Begin
                // Yes, start a new page.
                iY := 0;
                newPage();
              End;

              // Print the line.
              With TwegLibNGLinePainter.create() Do
                Try
                  parse( slLines[ i ], Canvas, iY, 0, NortonGuide.OEMToANSI );
                Finally
                  free();
                End;

              // Move down a line.
              Inc( iY, Canvas.textHeight( 'X' ) );

            End;

          Finally
            // Free the list of lines.
            slLines.free();
            // End the document.
            endDoc();
          End;

        End;

      Finally
        free();
      End;

End;

/////

Procedure TfrmGuide.NGEntryKeyPress( Sender : TObject; Var Key : Char );
Var
  bHandled : Boolean;
Begin

  // Assume that the key has been handled.
  bHandled := True;
  
  // Handle some of the key strokes from the DOS/Linux version of EG.
  Case Key Of
    'Q', 'q' : If NGEntry.canNavigateUp()       Then actNavigateUpExecute( Nil );
    '-'      : If NGEntry.canNavigatePrevious() Then actNavigatePreviousExecute( Nil );
    '+'      : If NGEntry.canNavigateNext()     Then actNavigateNextExecute( Nil );
    '/', '\' :                                       actSearchFindExecute( Nil );
    'n'      :                                       actSearchFindAgainExecute( Nil );
    'S'      :                                       actGuideSaveSourceExecute( Nil );
    's'      :                                       actGuideSaveTextExecute( Nil );
    'r'      :                                       frmMain.actFileOpenExecute( Nil );
    'd', 'D' :                                       frmMain.actFileGuideManagerExecute( Nil );
    '?'      :                                       actGuideCreditsExecute( Nil );
  Else
    // We didn't handle the key.
    bHandled := False;
  End;

  // If we handled the key...
  If bHandled Then
    // ...let the caller know.
    Key := #0;

End;

/////

Procedure TfrmGuide.actGuidePrintUpdate( Sender : TObject );
ResourceString
  RSPrintCaption         = '&Print...';
  RSPrintHint            = 'Print the current entry';
  RSPrintSelectedCaption = '&Print Selected...';
  RSPrintSelectedHint    = 'Print the selected text';
Begin

  // Set the caption and the hint of the print action depending on the
  // select state of the guide entry viewer.
  If NGEntry.SelCount > 1 Then
  Begin
    actGuidePrint.Caption := RSPrintSelectedCaption;
    actGuidePrint.Hint    := RSPrintSelectedHint;
  End
  Else
  Begin
    actGuidePrint.Caption := RSPrintCaption;
    actGuidePrint.Hint    := RSPrintHint;
  End;

End;

/////

Procedure TfrmGuide.FormDestroy( Sender : TObject );
Begin
  // Free the URLs string list.
  slURLs.free();
End;

/////

Procedure TfrmGuide.populateURLList;
Var
  i     : Integer;
  j     : Integer;
  oHits : Variant;
Begin

  // If we can do URL scanning...
  If bCanURLSeeAlso Then
  Begin

    // Clear the current list.
    slURLs.clear();

    // For each line in the current entry.
    For i := 0 To NGEntry.Entry.LineCount - 1 Do
    Begin

      // Look for hits in a line.
      oHits := oURLRegExp.execute( NGEntry.Entry.StrippedLines[ i ] );

      // Found any?
      If oHits.Count > 0 Then
        // Yes, add them to the URL list.
        For j := 0 To oHits.Count - 1 Do
          slURLs.add( oHits.Item[ j ] );
        
    End;
    
  End;

End;

End.
