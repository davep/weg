{* File.......: frmBookmarksUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2004
 * ID.........: $Id$
 * Description: Bookmarks form.
 * Licence....: GNU General Public Licence (see below)
 *
 * WEG - Norton Guide Reader for Windows.
 * Copyright (C) 2004 Dave Pearson <davep@davep.org>
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

Unit frmBookmarksUnit;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs, TB2Dock, TB2Toolbar, ActnList, ImgList, ComCtrls, TB2Item, Menus;

Type

  {** Bookmarks form }
  TfrmBookmarks = Class( TForm )
    sbBookmarks: TStatusBar;
    alBookmarks: TActionList;
    ilBookmarks: TImageList;
    actBookmarksClose: TAction;
    tdbTop: TTBDock;
    tdbLeft: TTBDock;
    tdbBottom: TTBDock;
    tdbRight: TTBDock;
    tbBookmarksMenu: TTBToolbar;
    mnuBookmark: TTBSubmenuItem;
    mnuBookmarkClose: TTBItem;
    tbBookmarks: TTBToolbar;
    tbBookmarksClose: TTBItem;
    actBookmarksAdd: TAction;
    mnuBookmarkAdd: TTBItem;
    tbBookmarksAdd: TTBItem;
    tbBookmarksSplit1: TTBSeparatorItem;
    lvBookmarks: TListView;
    mnuBookmarkSep1: TTBSeparatorItem;
    actBookmarksRemove: TAction;
    tbBookmarksRemove: TTBItem;
    mnuBookmarkRemove: TTBItem;
    actBookmarksOpen: TAction;
    mnuBookmarkOpen: TTBItem;
    mnuBookmarklSep2: TTBSeparatorItem;
    tbBookmarksOpen: TTBItem;
    tbBookmarksSplit2: TTBSeparatorItem;
    popBookmarks: TTBPopupMenu;
    popBookmarksOpen: TTBItem;
    popBookmarksSplit1: TTBSeparatorItem;
    popBookmarksAdd: TTBItem;
    popBookmarksRemove: TTBItem;
    mnuOptions: TTBSubmenuItem;
    actOptionsRecycleWindows: TAction;
    mnuOptionsRecycleWindows: TTBItem;
    actBookmarksClear: TAction;
    mnuBookmarkClear: TTBItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure actBookmarksCloseExecute(Sender: TObject);
    procedure actBookmarksAddUpdate(Sender: TObject);
    procedure actBookmarksAddExecute(Sender: TObject);
    procedure bookmarkHighlighted(Sender: TObject);
    procedure actBookmarksRemoveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actBookmarksOpenExecute(Sender: TObject);
    procedure lvBookmarksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure actOptionsRecycleWindowsExecute(Sender: TObject);
    procedure actBookmarksClearUpdate(Sender: TObject);
    procedure actBookmarksClearExecute(Sender: TObject);

  Protected

    {** Have the bookmarks been loaded? }
    bLoaded : Boolean;

    {** Save the state of the window }
    Procedure saveWindowState;
    {** Load the state of the window }
    Procedure loadWindowState;
    {** Load the bookmarks }
    Procedure loadBookmarks;
    {** Save the bookmarks }
    Procedure saveBookmarks;
    {** Add a bookmark to the list }
    Procedure addBookmark( Const sCaption : String; Const sName : String; Const sOffset : String );

  End;

Var
  frmBookmarks: TfrmBookmarks;

Implementation

Uses
  Registry,
  wegUtils,
  frmMainUnit,
  frmGuideUnit,
  frmBookmarkNameUnit;

Const
  {** Registry key for the bookmark window data }
  REG_BOOKMARKS_WINDOW = 'Bookmarks';
  {** Key name for the bookmark window's position data }
  REG_POSITION = 'Position';
  {** Key name for the global finder's toolbar positions }
  REG_TOOLBARS = 'Toolbars';
  {** Key name for the bookmark data }
  REG_BOOKMARKS = 'Bookmarks';
  {** Name of the captions list }
  REG_CAPTIONS = 'Captions';
  {** Name of the file name list }
  REG_NAMES = 'Names';
  {** Name of the offsets list }
  REG_OFFSETS = 'Offsets';
  {** Key name for the options for the bookmark window }
  REG_OPTIONS = 'Options';
  {** Name for the guide recycling option }
  REG_OPTION_RECYCLE_WINDOWS = 'Recycle Windows';

{$R *.DFM}

/////

Procedure TfrmBookmarks.saveWindowState;
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
        If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_POSITION ] ), True ) Then
          Try
            // Save the size and location of the window.
            wegSaveWindowState( self, oReg );
          Finally
            // Close the window position/size key.
            closeKey();
          End;

        // Save the toolbar positions.
        TBRegSavePositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

        // Save the options for this window.
        If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_OPTIONS ] ), True ) Then
          Try
            writeBool( REG_OPTION_RECYCLE_WINDOWS, actOptionsRecycleWindows.Checked );
          Finally
            closeKey();
          End;

        // Save the bookmarks if they've been loaded.
        If bLoaded Then
          saveBookmarks();

      Finally
        // Free the registry object.
        free();
      End;

  End;

End;

/////

Procedure TfrmBookmarks.loadWindowState;
Var
  oReg : TRegistry;
Begin

  oReg := TRegistry.create();

  With oReg Do
    Try

      // Open the registry key for remembering the window position.
      If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_POSITION ] ), False ) Then
        Try
          // Try and load the size and location of the window.
          wegRestoreWindowState( self, oReg );
        Finally
          // Close the window position/size key.
          closeKey();
        End;

      // Load the toolbar positions.
      TBRegLoadPositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

      // Load the options for this window.
      If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_OPTIONS ] ), False ) Then
        Try
          Try
            actOptionsRecycleWindows.Checked := readBool( REG_OPTION_RECYCLE_WINDOWS );
          Except
            // GNDN.
          End;  
        Finally
          // Close the settings key.
          closeKey();
        End;
        
      // Load the bookmarks.
      loadBookmarks();
      
    Finally
      // Free the registry object.
      free();
    End;

End;

/////

Procedure TfrmBookmarks.loadBookmarks;
Var
  slCaptions : TStringlist;
  slFiles    : TStringList;
  slOffsets  : TStringList;
  i          : Integer;
Begin

  // If the bookmarks haven't been loaded...
  If Not bLoaded Then
  Begin

    // Create the string lists that will hold the bookmark data.
    slCaptions := TStringList.create();
    slFiles    := TStringList.create();
    slOffsets  := TStringList.create();

    // Mark the start of an update of the list.
    lvBookmarks.Items.beginUpdate();

    Try

      // Ensure that the bookmark list is empty.
      lvBookmarks.Items.clear();

      // Load the bookmarks from the registry.
      With TRegistry.create() Do
        Try
          If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_BOOKMARKS ] ), False ) Then
            Try
              Try
                // Populate the lists from the registry.
                slCaptions.CommaText := readString( REG_CAPTIONS );
                slFiles.CommaText    := readString( REG_NAMES );
                slOffsets.CommaText  := readString( REG_OFFSETS );
              Except
                // If there is any exception, ensure that the lists are clean.
                slCaptions.clear();
                slFiles.clear();
                slOffsets.clear();
              End;
            Finally
              closeKey();
            End;
        Finally
          free();
        End;

      // Now that we've got something (or not), populate the bookmarks list.
      For i := 0 To slCaptions.Count - 1 Do
        addBookmark( slCaptions[ i ], slFiles[ i ], slOffsets[ i ] );

      // Seems we loaded the list.
      bLoaded := True;

    Finally

      // Free the lists.
      slCaptions.free();
      slFiles.free();
      slOffsets.free();

      // End the update.
      lvBookmarks.Items.endUpdate();

    End;

  End;
  
End;

/////

Procedure TfrmBookmarks.saveBookmarks;
Var
  slCaptions : TStringlist;
  slFiles    : TStringList;
  slOffsets  : TStringList;
  i        : Integer;
Begin

  // Create the string lists that will hold the bookmark data.
  slCaptions := TStringList.create();
  slFiles    := TStringList.create();
  slOffsets  := TStringList.create();

  Try

    // Populate the lists.
    For i := 0 To lvBookmarks.Items.Count - 1 Do
    Begin
      slCaptions.add( lvBookmarks.Items[ i ].Caption );
      slFiles.add(    lvBookmarks.Items[ i ].SubItems[ 0 ] );
      slOffsets.add(  lvBookmarks.Items[ i ].SubItems[ 1 ] );
    End;

    // Write the lists to the registry.
    With TRegistry.create() Do
      Try
        If openKey( wegRegistryKey( [ REG_BOOKMARKS_WINDOW, REG_BOOKMARKS ] ), True ) Then
          Try
            writeString( REG_CAPTIONS, slCaptions.CommaText );
            writeString( REG_NAMES,    slFiles.CommaText );
            writeString( REG_OFFSETS,  slOffsets.CommaText );
          Finally
            closeKey();
          End;
      Finally
        free();
      End;

  Finally
    // Free the lists.
    slCaptions.free();
    slFiles.free();
    slOffsets.free();
  End;

End;

/////

Procedure TfrmBookmarks.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin
  // Save the window state.
  saveWindowState();
End;

/////

Procedure TfrmBookmarks.FormShow( Sender : TObject );
Begin
  // Load the window state.
  loadWindowState();
End;

/////

Procedure TfrmBookmarks.actBookmarksCloseExecute( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmBookmarks.actBookmarksAddUpdate( Sender : TObject );
Begin
  // We can only add a bookmark if there is a guide in focus.
  actBookmarksAdd.Enabled := frmMain.focusedGuide() <> Nil;
End;

/////

Procedure TfrmBookmarks.actBookmarksAddExecute( Sender : TObject );
Begin

  // Get a caption for the bookmark from the user.
  With TfrmBookmarkName.create( self ) Do
    Try

      // Suggest a default title.
      edtCaption.Text := frmMain.focusedViewer().entryTitle();

      // If the user entered something...
      If showModal() = mrOk Then
        // ...add the bookmark to the list.
        addBookmark( edtCaption.Text, frmMain.focusedGuide().Guide, IntToStr( frmMain.focusedEntry().Offset ) );

    Finally
      free();
    End;

End;

/////

Procedure TfrmBookmarks.bookmarkHighlighted( Sender : TObject );
Begin
  TAction( Sender ).Enabled := lvBookmarks.Selected <> Nil;
End;

/////

Procedure TfrmBookmarks.actBookmarksRemoveExecute( Sender : TObject );
ResourceString
  RSConfirm = 'Are you sure you want to delete "%s"?';
Begin

  // Check that the user really wants to delete it.
  MessageBeep( MB_ICONQUESTION );
  If MessageDlg( Format( RSConfirm, [ lvBookmarks.Selected.Caption ] ), mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes Then
    // They do, delete it.
    lvBookmarks.Selected.delete();
    
End;

/////

Procedure TfrmBookmarks.FormCreate( Sender : TObject );
Begin
  // To start with the bookmarks haven't been loaded.
  bLoaded := False;
End;

/////

Procedure TfrmBookmarks.addBookmark( Const sCaption : String; Const sName : String; Const sOffset : String );
Begin

  With lvBookmarks.Items.add() Do
  Begin
    Caption := sCaption;
    SubItems.add( sName );
    SubItems.add( sOffset );
  End;

End;

/////

Procedure TfrmBookmarks.actBookmarksOpenExecute( Sender : TObject );

  Function OpenType : TfrmMainOpenGuide;
  Begin
    If actOptionsRecycleWindows.Checked Then
      Result := mogRecycle
    Else
      Result := mogNew;
  End;

Begin

  If lvBookmarks.Selected <> Nil Then
    With lvBookmarks.Selected Do
      frmMain.openGuide( SubItems[ 0 ], OpenType(), StrToInt( SubItems[ 1 ] ), 0 );
      
End;

/////

Procedure TfrmBookmarks.lvBookmarksChange( Sender : TObject; Item : TListItem; Change : TItemChange );
Begin

  If lvBookmarks.Selected = Nil Then
    sbBookmarks.Panels[ 1 ].Text := ''
  Else
    sbBookmarks.Panels[ 1 ].Text := ExtractFileName( lvBookmarks.Selected.SubItems[ 0 ] );
      
End;

/////

Procedure TfrmBookmarks.actOptionsRecycleWindowsExecute( Sender : TObject );
Begin
  actOptionsRecycleWindows.Checked := Not actOptionsRecycleWindows.Checked;
End;

/////

Procedure TfrmBookmarks.actBookmarksClearUpdate( Sender : TObject );
Begin
  actBookmarksClear.Enabled := lvBookmarks.Items.Count > 0;
End;

/////

Procedure TfrmBookmarks.actBookmarksClearExecute( Sender : TObject );
ResourceString
  RSAreYouSure = 'Clear all boomkarks from the bookmark list, are you sure?';
Begin

  MessageBeep( MB_ICONQUESTION );
  If MessageDlg( RSAreYouSure, mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes Then
  Begin
    lvBookmarks.Items.clear();
    saveBookmarks();
  End;

End;

End.
