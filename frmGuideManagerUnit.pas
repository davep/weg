{* File.......: frmGuideManagerUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Guide manager form.
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

Unit frmGuideManagerUnit;

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
  ImgList,
  ActnList,
  TB2Dock,
  TB2Item,
  TB2Toolbar,
  Menus;

Type
  {** Guide manager. }
  TfrmGuideManager = Class( TForm )
    sbGuideManager: TStatusBar;
    tbdTop: TTBDock;
    tbdLeft: TTBDock;
    tbdBottom: TTBDock;
    tbdRight: TTBDock;
    actGuideManager: TActionList;
    ilGuideManager: TImageList;
    lvGuides: TListView;
    actGuidesOpen: TAction;
    tbManagerMenu: TTBToolbar;
    TBSubmenuItem1: TTBSubmenuItem;
    mnuGuidesOpen: TTBItem;
    actGuidesAdd: TAction;
    actGuidesRemove: TAction;
    actGuidesClear: TAction;
    mnuGuidesSplit1: TTBSeparatorItem;
    mnuGuidesAdd: TTBItem;
    mnuGuidesRemove: TTBItem;
    mnuGuidesClear: TTBItem;
    tbGuides: TTBToolbar;
    tbGuidesOpen: TTBItem;
    tbGuidesSplit1: TTBSeparatorItem;
    tbGuidesAdd: TTBItem;
    tbGuidesRemove: TTBItem;
    odAdd: TOpenDialog;
    actGuidesClose: TAction;
    tbGuidesClose: TTBItem;
    tbGuidesSplit2: TTBSeparatorItem;
    mnuGuidesSplit2: TTBSeparatorItem;
    mnuGuidesClose: TTBItem;
    popGuides: TTBPopupMenu;
    popGuidesAdd: TTBItem;
    popGuidesRemove: TTBItem;
    popGuidesOpen: TTBItem;
    popGuidesSep1: TTBSeparatorItem;
    actOptionsShowHint: TAction;
    mnuOptions: TTBSubmenuItem;
    mnuOptionsShowHint: TTBItem;
    actOptionsRecycleWindows: TAction;
    mnuOptionsRecycleWindows: TTBItem;
    actGuidesCredits: TAction;
    mnuGuidesCredits: TTBItem;
    popGuidesCredits: TTBItem;
    tbGuidesCredits: TTBItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actGuidesAddExecute(Sender: TObject);
    procedure actGuidesClearUpdate(Sender: TObject);
    procedure actGuidesClearExecute(Sender: TObject);
    procedure actGuidesSelectedUpdate(Sender: TObject);
    procedure actGuidesOpenExecute(Sender: TObject);
    procedure actGuidesRemoveExecute(Sender: TObject);
    procedure actGuidesCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvGuidesInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure actOptionsShowHintUpdate(Sender: TObject);
    procedure actOptionsShowHintExecute(Sender: TObject);
    procedure actOptionsRecycleWindowsExecute(Sender: TObject);
    procedure actGuidesCreditsExecute(Sender: TObject);

  Protected

    {** Has the guide list been loaded? }
    bLoaded : Boolean;

    {** Save the state of the window }
    Procedure saveWindowState;
    {** Load the state of the window }
    Procedure loadWindowState;
    {** Load the guide list }
    Procedure loadGuideList;
    {** Save the guide list }
    Procedure saveGuideList;
    {** Add a guide, with the given title, to the guide manager }
    Procedure addGuide( Const sFile : String; Const sTitle : String ); Overload;
    {** Add a guide to the guide manager }
    Procedure addGuide( Const sFile : String ); Overload;
    {** Accept dropped files. }
    Procedure acceptFiles( Var msg : TMessage ); Message WM_DROPFILES;

  Public

    {** Get the guide name list }
    Procedure getGuideNames( slNames : TStringList );
    {** Returns the name of the highlighted guide }
    Function highlightedGuide : String;

  End;

Var
  frmGuideManager: TfrmGuideManager;

Implementation

Uses
  ShellAPI,
  Registry,
  frmMainUnit,
  frmGuideCreditsUnit,
  wegLibNortonGuide,
  wegUtils;

Const
  {** Registry key for the guide manager data }
  REG_MANAGER_WINDOW = 'Guide Manager';
  {** Key name for the guide manager's position data }
  REG_POSITION = 'Position';
  {** Key name for the guide manager's toolbar positions }
  REG_TOOLBARS = 'Toolbars';
  {** Name of the title column width }
  REG_TITLE_WIDTH = 'Title Width';
  {** Name of the file name column width }
  REG_NAME_WIDTH = 'Name Width';
  {** Key name for the options for the guide manager window }
  REG_OPTIONS = 'Options';
  {** Name of the show hints option }
  REG_SHOW_HINT = 'Show Hints';
  {** Name for the guide recycling option }
  REG_OPTION_RECYCLE_WINDOWS = 'Recycle Windows';
  {** Key name for the guide manager's guide data }
  REG_GUIDES = 'Guides';
  {** Name of the file name list }
  REG_NAMES = 'Names';
  {** Name of the titles list }
  REG_TITLES = 'Titles';

{$R *.DFM}

Procedure TfrmGuideManager.FormShow( Sender : TObject );
Begin

  // Load the previous state of the window.
  loadWindowState();

  // Let Windows know that we'll accept dropped files.
  DragAcceptFiles( Handle, True );

End;

/////

Procedure TfrmGuideManager.saveWindowState;
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
        If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_POSITION ] ), True ) Then
          Try

            // Save the size and location of the window.
            wegSaveWindowState( self, oReg );

            // Save the column widths.
            writeInteger( REG_TITLE_WIDTH, lvGuides.Columns[ 0 ].Width );
            writeInteger( REG_NAME_WIDTH,  lvGuides.Columns[ 1 ].Width );

          Finally
            // Close the window position/size key.
            closeKey();
          End;

        // Save the toolbar positions.
        TBRegSavePositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_MANAGER_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

        // Save the options for this window.
        If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_OPTIONS ] ), True ) Then
          Try
            writeBool( REG_SHOW_HINT,              lvGuides.ShowHint );
            writeBool( REG_OPTION_RECYCLE_WINDOWS, actOptionsRecycleWindows.Checked );
          Finally
            closeKey();
          End;

        // Save the guide list if it's been loaded.
        If bLoaded Then
          saveGuideList();

      Finally
        // Free the registry object.
        free();
      End;

  End;

End;

/////

Procedure TfrmGuideManager.loadWindowState;
Var
  oReg : TRegistry;
Begin

  oReg := TRegistry.create();

  With oReg Do
    Try

      // Open the registry key for remembering the window position.
      If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_POSITION ] ), False ) Then
        Try

          // Try and load the size and location of the window.
          wegRestoreWindowState( self, oReg );

          // Try and load the column widths.
          Try
            lvGuides.Columns[ 0 ].Width := readInteger( REG_TITLE_WIDTH );
            lvGuides.Columns[ 1 ].Width := readInteger( REG_NAME_WIDTH );
          Except
            // GNDN.
          End;

        Finally
          // Close the window position/size key.
          closeKey();
        End;

      // Load the toolbar positions.
      TBRegLoadPositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_MANAGER_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

      // Load the options for this window.
      If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_OPTIONS ] ), False ) Then
        Try
          Try
            lvGuides.ShowHint                := readBool( REG_SHOW_HINT );
            actOptionsRecycleWindows.Checked := readBool( REG_OPTION_RECYCLE_WINDOWS );
          Except
            lvGuides.ShowHint := True;
          End;
        Finally
          // Close the settings key.
          closeKey();
        End;

      // Load the guides list.
      loadGuideList();

    Finally
      // Free the registry object.
      free();
    End;

End;

/////

Procedure TfrmGuideManager.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin
  saveWindowState();
End;

/////

Procedure TfrmGuideManager.actGuidesAddExecute( Sender : TObject );
Var
  i : Integer;
Begin

  // Point the open dialog at the default guide directory.
  odAdd.InitialDir := frmMain.NGSettings.DefaultGuideDirectory;

  // Let the user select some guides...
  If odAdd.execute() Then
    With TwegBusyCursor.create() Do
      Try

        // Mark the start of an update.
        lvGuides.Items.beginUpdate();

        Try
          // If they selected something, add the files to the guide manager.
          For i := 0 To odAdd.Files.Count - 1 Do
            addGuide( odAdd.Files[ i ] );
        Finally
          // Mark the end of the update.
          lvGuides.items.endUpdate();
        End;

      Finally
        free();
      End;

End;

/////

Procedure TfrmGuideManager.addGuide( Const sFile : String; Const sTitle : String );
Begin

  With lvGuides.Items.add() Do
  Begin
    Caption    := sTitle;
    ImageIndex := 0;
    SubItems.add( sFile );
  End;

End;

/////

Procedure TfrmGuideManager.addGuide( Const sFile : String );
ResourceString
  RSCantOpenAsGuide  = 'Can''t open "%s" as a Norton Guide database.';
  RSAlreadyInManager = '"%s" is already in the manager, ignoring.';
Var
  oGuide : TwegLibNortonGuide;
Begin

  // Create an instance of a Norton Guide so we can test that the passed file
  // is ok.
  oGuide := TwegLibNortonGuide.create( Nil );

  Try

    // Try and open the guide.
    oGuide.Guide := sFile;

    // If it opened ok...
    If oGuide.isOpen() Then
    Begin

      // If it doesn't already exist in the guide manager.
      If lvGuides.findCaption( 0, oGuide.Title, False, True, True ) = Nil Then
        // We made it this far, add the guide.
        addGuide( sFile, oGuide.Title )
      Else
      Begin
        MessageBeep( MB_ICONINFORMATION );
        MessageDlg( Format( RSAlreadyInManager, [ oGuide.Title ] ), mtInformation, [ mbOk ], 0 );
      End;

    End
    Else
    Begin
      MessageBeep( MB_ICONERROR );
      MessageDlg( Format( RSCantOpenAsGuide, [ sFile ] ), mtError, [ mbOk ], 0 );
    End;

  Finally
    oGuide.free();
  End;

End;

/////

Procedure TfrmGuideManager.loadGuideList;
Var
  slTitles : TStringlist;
  slFiles  : TStringList;
  i        : Integer;
Begin

  // If the list hasn't been loaded...
  If Not bLoaded Then
  Begin

    // Create the string lists that will hold the titles and file names.
    slTitles := TStringList.create();
    slFiles  := TStringList.create();

    // Mark the start of an update of the list.
    lvGuides.Items.beginUpdate();

    Try

      // Ensure that the guides list is empty.
      lvGuides.Items.clear();

      // Load the lists from the registry.
      With TRegistry.create() Do
        Try
          If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_GUIDES ] ), False ) Then
            Try
              Try
                // Populate the lists from the registry.
                slTitles.CommaText := readString( REG_TITLES );
                slFiles.CommaText  := readString( REG_NAMES );
              Except
                // If there is any exception, ensure that both the lists are clean.
                slTitles.clear();
                slFiles.clear();
              End;
            Finally
              closeKey();
            End;
        Finally
          free();
        End;

      // Now that we've got something (or not), populate the guides list.
      For i := 0 To slTitles.Count - 1 Do
        addGuide( slFiles[ i ], slTitles[ i ] );

      // Seems we loaded the list.
      bLoaded := True;

    Finally

      // Free the lists.
      slTitles.free();
      slFiles.free();

      // End the update.
      lvGuides.Items.endUpdate();

    End;

  End;

End;

/////

Procedure TfrmGuideManager.saveGuideList;
Var
  slTitles : TStringlist;
  slFiles  : TStringList;
  i        : Integer;
Begin

  // Create the string lists that will hold the titles and file names.
  slTitles := TStringList.create();
  slFiles  := TStringList.create();

  Try

    // Populate the lists.
    For i := 0 To lvGuides.Items.Count - 1 Do
    Begin
      slTitles.add( lvGuides.Items[ i ].Caption );
      slFiles.add(  lvGuides.Items[ i ].SubItems[ 0 ] );
    End;

    // Write the lists to the registry.
    With TRegistry.create() Do
      Try
        If openKey( wegRegistryKey( [ REG_MANAGER_WINDOW, REG_GUIDES ] ), True ) Then
          Try
            writeString( REG_TITLES, slTitles.CommaText );
            writeString( REG_NAMES,  slFiles.CommaText );
          Finally
            closeKey();
          End;
      Finally
        free();
      End;

  Finally
    // Free the lists.
    slTitles.free();
    slFiles.free();
  End;

End;

/////

Procedure TfrmGuideManager.actGuidesClearUpdate( Sender : TObject );
Begin
  actGuidesClear.Enabled := lvGuides.Items.Count > 0;
End;

/////

Procedure TfrmGuideManager.actGuidesClearExecute( Sender : TObject );
ResourceString
  RSAreYouSure = 'Clear all guides from the guide manager, are you sure?';
Begin

  MessageBeep( MB_ICONQUESTION );
  If MessageDlg( RSAreYouSure, mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes Then
  Begin
    lvGuides.Items.clear();
    saveGuideList();
  End;

End;

/////

Procedure TfrmGuideManager.actGuidesSelectedUpdate( Sender : TObject );
Begin
  TAction( Sender ).Enabled := ( lvGuides.Selected <> Nil );
End;

/////

Procedure TfrmGuideManager.actGuidesOpenExecute( Sender : TObject );

  Function OpenType : TfrmMainOpenGuide;
  Begin
    If actOptionsRecycleWindows.Checked Then
      Result := mogRecycle
    Else
      Result := mogNew;
  End;

Begin

  If lvGuides.Selected <> Nil Then
    frmMain.openGuide( highlightedGuide(), OpenType() );

End;

/////

Procedure TfrmGuideManager.actGuidesRemoveExecute( Sender : TObject );
ResourceString
  RSAreYouSure = 'Remove "%s" from the guide manager, are you sure?';
Begin

  MessageBeep( MB_ICONQUESTION );
  If MessageDlg( Format( RSAreYouSure, [ lvGuides.Selected.Caption ] ), mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes THen
  Begin
    lvGuides.Selected.delete();
    saveGuideList();
  End;

End;

/////

Procedure TfrmGuideManager.actGuidesCloseExecute( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmGuideManager.FormCreate( Sender : TObject );
Begin
  // To start with the list isn't loaded.
  bLoaded := False;
End;

/////

Procedure TfrmGuideManager.lvGuidesInfoTip( Sender : TObject; Item : TListItem; Var InfoTip : String );
Begin

  // If possible, display the credits of the guide as the hint. Note that we
  // "cache" the credits in the SubItems array of each of the items.

  // If there isn't a cached hint...
  If Item.SubItems.Count = 1 Then
    // ...load it and cache it.
    With TwegLibNortonGuide.create( Nil ) Do
      Try
        OEMToANSI := True;
        Guide     := Item.SubItems[ 0 ];
        If isOpen() Then
        Begin
          If Trim( Credits ) <> '' Then
            Item.SubItems.add( Item.Caption + #13#10#13#10 + TrimRight( Credits ) )
          Else
            Item.SubItems.add( Item.Caption );
        End
        Else
          Item.SubItems.add( Item.Caption );
      Finally
        free();
      End;

  // Get the hint.
  InfoTip := Item.SubItems[ 1 ]

End;

/////

Procedure TfrmGuideManager.actOptionsShowHintUpdate( Sender : TObject );
Begin
  actOptionsShowHint.Checked := lvGuides.ShowHint;
End;

/////

Procedure TfrmGuideManager.actOptionsShowHintExecute( Sender : TObject );
Begin
  lvGuides.ShowHint := Not lvGuides.ShowHint;
End;

/////

Procedure TfrmGuideManager.getGuideNames( slNames : TStringList );
Var
  i : Integer;
Begin

  // Ensure that the list is loaded.
  loadGuideList();

  // Add the guide file names the the passed list.
  For i := 0 To lvGuides.Items.Count - 1 Do
    slNames.add( lvGuides.Items[ i ].SubItems[ 0 ] );

End;

/////

Procedure TfrmGuideManager.acceptFiles( Var msg : TMessage );
Var
  acFileName : Array[ 0..MAX_PATH - 1 ] Of Char;
  iFiles     : Integer;
  i          : Integer;
Begin

  // Work out how many files are being dropped.
  iFiles := DragQueryFile( msg.WParam, $FFFFFFFF, acFileName, MAX_PATH );

  // Loop over the dropped files, adding them to the guide manager.
  For i := 0 To iFiles - 1 Do
  Begin
    DragQueryFile( msg.WParam, i, acFileName, MAX_PATH );
    addGuide( acFileName );
  End;

End;

/////

Procedure TfrmGuideManager.actOptionsRecycleWindowsExecute( Sender : TObject );
Begin
  actOptionsRecycleWindows.Checked := Not actOptionsRecycleWindows.Checked;
End;

/////

Function TfrmGuideManager.highlightedGuide : String;
Begin
  If lvGuides.Selected = Nil Then
    Result := ''
  Else
    Result := lvGuides.Selected.SubItems[ 0 ];
End;

/////

Procedure TfrmGuideManager.actGuidesCreditsExecute( Sender : TObject );
Var
  oGuide : TwegLibNortonGuide;
Begin

  // Create a guide object.
  oGuide := TwegLibNortonGuide.create( Nil );

  Try

    // Configure it and open the guide.
    oGuide.Settings := frmMain.NGSettings;
    oGuide.Guide    := lvGuides.Selected.SubItems[ 0 ];
    If oGuide.isOpen() Then
      With TfrmGuideCredits.create( self ) Do
        Try
          oNortonGuide := oGuide;
          showModal();
        Finally
          free();
        End;

  Finally
    oGuide.free();
  End;

End;

End.
