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
    {** Handle a "find" click in the finder dialog }
    Procedure findClick( Sender : TObject );

  End;

Implementation

Uses
  ClipBrd,
  wegLibUtils,
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
ResourceString
  RSSep = ' » ';
Begin

  With sbGuide, NGEntry.Entry Do
  Begin

    // Start out with the title of the guide.
    pnlGuideTitle.Text := NortonGuide.Title;

    // If we're looking at something that hangs off a menu...
    If validMenu( ParentMenu ) Then
    Begin

      // Add the menu's title to the status bar.
      pnlGuideTitle.Text := pnlGuideTitle.Text + RSSep + tbGuideMenu.Items[ ParentMenu ].Caption;

      // If we also know which menu prompt on that menu we belong to...
      If validMenu( ParentMenuPrompt ) Then
        // ...add the prompt text to the status bar.
        pnlGuideTitle.Text := pnlGuideTitle.Text + RSSep + tbGuideMenu.Items[ ParentMenu ].Items[ ParentMenuPrompt ].Caption;

    End;

  End;

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

  Function SetupSeeAlsoItem( oItem : TTBItem; i : Integer; Const sCaption : String ) : TTBItem;
  ResourceString
    RSSeeAlso = 'See Also "%s"';
  Begin

    With oItem Do
    Begin
      Caption := StringReplace( sCaption, '&', '&&', [ rfReplaceAll ] );
      OnClick := seeAlsoClick;
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
      mnuSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( tbGuideMenu ), i, NGEntry.Entry.SeeAlso[ i ] ) );
      popGuideSeeAlso.add( SetupSeeAlsoItem( TTBItem.create( tbGuideMenu ), i, NGEntry.Entry.SeeAlso[ i ] ) );
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

  // For each menu in the guide...
  For i := 0 To NortonGuide.MenuCount - 1 Do
  Begin

    // ...add an item to the menu for the guide menu we're looking at.
    tbGuideMenu.Items.add( TTBSubmenuItem.create( tbGuideMenu ) );

    // Configure it.
    With tbGuideMenu.Items[ tbGuideMenu.Items.Count - 1 ] Do
    Begin

      // Set the caption of the menu title.
      Caption := NortonGuide.Menus[ i ].Title;
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
  
End;

/////

Procedure TfrmGuide.NGEntryNewEntry( Sender : TObject );
Begin

  // Refresh the menu.
  refreshMenu();

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

Procedure TfrmGuide.NGEntryInvalidLink( Sender : TObject );
ResourceString
  RSBadLink = 'Invalid link, unable to follow';
Begin
  MessageBeep( MB_ICONERROR );
  MessageDlg( RSBadLink, mtError, [ mbOk ], 0 );
End;

/////

Procedure TfrmGuide.NortonGuideError( Sender : TObject; Const sError: String );
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
    frmFinder.Caption                     := Format( RSCaption, [ NortonGuide.Title ] );
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
  self.setFocus(); // TODO: Doesn't work.
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

End.
