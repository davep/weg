{* File.......: frmMainUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Main form.
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

Unit frmMainUnit;

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
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  ActnList,
  ImgList,
  TB2ExtItems,
  TB2MDI,
  wegLibNortonGuide,
  wegLibNGEntry,
  wegLibNGEntryViewer,
  wegLibNGColours,
  wegLibNGSettings,
  Menus,
  DdeMan,
  frmGuideUnit;

Type

  {** Types of guide opening options }
  TfrmMainOpenGuide = ( mogNew, mogRecycle );
  
  {** Main form }
  TfrmMain = Class( TForm )
    sbMain: TStatusBar;
    tbdTop: TTBDock;
    tbdLeft: TTBDock;
    tbdBottom: TTBDock;
    tbdRight: TTBDock;
    alMain: TActionList;
    ilMain: TTBImageList;
    actFileOpen: TAction;
    tbMainMenu: TTBToolbar;
    odGuide: TOpenDialog;
    tbFile: TTBToolbar;
    mnuFile: TTBSubmenuItem;
    mnuFileOpen: TTBItem;
    mnuHelp: TTBSubmenuItem;
    actFileExit: TAction;
    mnuFileExit: TTBItem;
    mnuFileSep1: TTBSeparatorItem;
    tbFileExit: TTBItem;
    tbFileSep1: TTBSeparatorItem;
    mnuWindows: TTBSubmenuItem;
    mnuWindowsList: TTBMDIWindowItem;
    MDIHandler: TTBMDIHandler;
    NGColours: TwegLibNGColours;
    actWindowCascade: TAction;
    actWindowTileHorizontal: TAction;
    actWindowTileVertical: TAction;
    mnuWindowCascade: TTBItem;
    mnuWindowTileHorizontal: TTBItem;
    mnuWindowTileVertical: TTBItem;
    mnuWindowSplit1: TTBSeparatorItem;
    tbWindow: TTBToolbar;
    sbWindowCascade: TTBItem;
    sbWindowTileHorizontal: TTBItem;
    sbWindowTileVertical: TTBItem;
    mnuFileReOpen: TTBSubmenuItem;
    tbFileOpen: TTBSubmenuItem;
    actFileCloseAll: TAction;
    mnuFileCloseAll: TTBItem;
    actFileGuideManager: TAction;
    mnuFileGuideManager: TTBItem;
    mnuFileSplit2: TTBSeparatorItem;
    tbFileGuideManager: TTBItem;
    NGSettings: TwegLibNGSettings;
    actOptionsGuidePreferences: TAction;
    actOptionsGuideColours: TAction;
    mnuOptions: TTBSubmenuItem;
    mnuOptionsPreferences: TTBItem;
    mnuOptionsColours: TTBItem;
    actHelpAbout: TAction;
    mnuHelpAbout: TTBItem;
    actFileGlobalFind: TAction;
    mnuFileSplit3: TTBSeparatorItem;
    mnuFileGlobalFind: TTBItem;
    tbFileSep2: TTBSeparatorItem;
    tbFileGlobalFind: TTBItem;
    actFileBookmarks: TAction;
    mnuFileBoomarks: TTBItem;
    mnuFileSplit4: TTBSeparatorItem;
    tbFileBookmarks: TTBItem;
    tbFileSep3: TTBSeparatorItem;
    psdGuide: TPrinterSetupDialog;
    actOptionsPrinterSetup: TAction;
    mnuOptionsPrinterSetup: TTBItem;
    Execute: TDdeServerConv;
    actOptionsRecycleDDE: TAction;
    mnuOptionsRecycle: TTBSubmenuItem;
    mnuOptionsRecycleDDE: TTBItem;
    actOptionsRecycleCommandLine: TAction;
    mnuOptionsRecycleCommandLine: TTBItem;
    actOptionsRecycleDragAndDrop: TAction;
    mnuOptionsRecycleDragAndDrop: TTBItem;
    actOptionsRecycleFileOpen: TAction;
    actOptionsRecycleFileReOpen: TAction;
    mnuOptionsRecycleFileOpen: TTBItem;
    mnuOptionsRecycleFileReOpen: TTBItem;
    mnuOptionsSplit1: TTBSeparatorItem;
    mnuOptionsSplit2: TTBSeparatorItem;
    actOptionsPrintColour: TAction;
    mnuOptionsPrintColour: TTBItem;
    actOptionsBOLShortSearch: TAction;
    mnuOptionsBOLShortSearch: TTBItem;
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure actWindowUpdate(Sender: TObject);
    procedure actWindowCascadeExecute(Sender: TObject);
    procedure actWindowTileHorizontalExecute(Sender: TObject);
    procedure actWindowTileVerticalExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileReOpenPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actFileCloseAllUpdate(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actFileGuideManagerExecute(Sender: TObject);
    procedure actOptionsGuidePreferencesExecute(Sender: TObject);
    procedure actOptionsGuideColoursExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFileGlobalFindExecute(Sender: TObject);
    procedure actFileBookmarksExecute(Sender: TObject);
    procedure actOptionsPrinterSetupExecute(Sender: TObject);
    procedure ExecuteExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure actOptionsCheckItemExecute(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);

  Public

    {** Open a guide in the given window }
    Function openGuide( Const sFile : String; oWindow : TfrmGuide; lEntry : LongInt = -1; iStartingLine : Integer = -1 ) : TForm; Overload;
    {** Open a guide }
    Function openGuide( Const sFile : String; mog : TfrmMainOpenGuide; lEntry : LongInt = -1; iStartingLine : Integer = -1 ) : TForm; Overload;
    {** Return a pointer to the focused Norton Guide }
    Function focusedGuide : TwegLibNortonGuide;
    {** Return a pointer to the focused Norton Guide entry viewer }
    Function focusedViewer : TwegLibNGEntryViewer;
    {** Return a pointer to the focused Norton Guide entry }
    Function focusedEntry : TwegLibNGEntry;

  Protected

    {** MRU file name list }
    slMRUFiles : TStringList;
    {** MRU guide titles list }
    slMRUTitles : TStringList;

    {** Default the guide location }
    Function defaultGuideLocation( Const sFile : String ) : String;
    {** Save the state of the window }
    Procedure saveWindowState;
    {** Load the state of the window }
    Procedure loadWindowState;
    {** Remember a guide in the MRU lists }
    Procedure rememberInMRU( oGuide : TwegLibNortonGuide );
    {** Procedure for reopening a guide from the MRU list }
    Procedure reopenFromMRU( oSender : TObject );
    {** Refresh the MRU menu }
    Procedure refreshMRUMenu;
    {** Refresh all child windows }
    Procedure refreshChildWindows;
    {** Accept dropped files. }
    Procedure acceptFiles( Var msg : TMessage ); Message WM_DROPFILES;
    {** Work out the guide open type based on the state of its config option }
    Function openType( oAction : TAction ) : TfrmMainOpenGuide;

  End;

Var
  frmMain : TfrmMain;

Implementation

Uses
  ShellAPI,
  Registry,
  wegUtils,
  frmGuideManagerUnit,
  frmAboutUnit,
  frmGlobalFindUnit,
  frmBookmarksUnit;

{$R *.DFM}

Const
  {** Key name for the main window registry data }
  REG_MAIN_WINDOW = 'Main Window';
  {** Key name for the main window's position data }
  REG_POSITION = 'Position';
  {** Key name for the main window's toolbar data }
  REG_TOOLBARS = 'Toolbars';
  {** Key name for the preferences }
  REG_PREFERENCES = 'Preferences';
  {** Key name for the colour settings }
  REG_COLOURS = 'Colours';
  {** Value name for the file-open window recycling option }
  REG_RECYCLE_FILE_OPENS = 'Recycle Windows on File Open';
  {** Value name for the file-reopen window recycling option }
  REG_RECYCLE_FILE_REOPENS = 'Recycle Windows on File Reopen';
  {** Value name for the DDE window recycling option }
  REG_RECYCLE_DDE_OPENS = 'Recycle Windows on DDE Open';
  {** Value name for the command line window recycling option }
  REG_RECYCLE_COMMAND_LINE_OPENS = 'Recycle Windows on Command Line Open';
  {** Value name for the drag and drop window recycling option }
  REG_RECYCLE_DRAG_AND_DROP_OPENS = 'Recycle Windows on Drag and Drop Open';
  {** Value name for the colour printing option }
  REG_COLOUR_PRINTING = 'Colour Printing';
  {** Value name for the BOL first short searching option }
  REG_BOL_SHORT_SEARCHING = 'BOL First Searching in Short Entries';
  {** Key name for the child window list }
  REG_CHILDREN = 'Child Windows';
  {** Value name for the guide of a child window }
  REG_GUIDE = 'Guide';
  {** Value name for the entry of a child window }
  REG_ENTRY = 'Entry';
  {** Value name for the line of a child window }
  REG_LINE = 'Line';
  {** Value name for the line visible at the top of the window }
  REG_TOP_LINE = 'TopLine';
  {** Value name for remembering if the guide manager was open }
  REG_GUIDE_MANAGER_OPEN = 'Guide Manager Open';
  {** Value name for remembering if the bookmark manager was open }
  REG_BOOMKARK_MANAGER_OPEN = 'Bookmark Manager Open';
  {** Value name for remembering if the global finder was open }
  REG_GLOBAL_FINDER_OPEN = 'Global Finder Open';
  {** Key name for the main window's MRU data }
  REG_MRU = 'MRU';
  {** The file names of the guides in the MRU }
  REG_MRU_NAMES = 'Names';
  {** The titles of the guides in the MRU }
  REG_MRU_TITLES = 'Titles';
  {** Max number of files to keep in the MRU list }
  MRU_MAX = 10;

/////

Function TfrmMain.defaultGuideLocation( Const sFile : String ) : String;
Begin

  // If the file doesn't exist and there is no path information and we've got
  // a default guide directory...
  If ( Not FileExists( sFile ) ) And ( ExtractFilePath( sFile ) = '' ) And ( NGSettings.DefaultGuideDirectory <> '' ) Then
    // ...assume that the user wishes to try and open it from the default
    // guide directory.
    Result := IncludeTrailingBackslash( NGSettings.DefaultGuideDirectory ) + sFile
  Else
    // ...otherwise just go with what we've got.
    Result := sFile;

End;

/////

Function TfrmMain.openGuide( Const sFile : String; oWindow : TfrmGuide; lEntry : LongInt; iStartingLine : Integer ) : TForm;
Begin

  With oWindow Do
  Begin

    // Open guide.
    NortonGuide.Guide := defaultGuideLocation( sFile );

    // If it couldn't be opened...
    If Not NortonGuide.isOpen() Then
    Begin
      // ...close the window.
      close();
      // Don't return the pointer to it.
      oWindow := Nil;
    End
    // Otherwise, if we're supposed to load a certain entry...
    Else If ( lEntry > -1 ) And NortonGuide.isOffsetValidEntry( lEntry ) Then
      // ...try and load it.
      NGEntry.display( lEntry, iStartingLine )
    Else
      // Failing all that, display the first entry.
      NGEntry.displayFirstEntry();

    // If we managed to open a guide, remember it.
    If oWindow <> Nil Then
      rememberInMRU( NortonGuide );

  End;

  // Return the window.
  Result := oWindow;
  
End;

/////

Function TfrmMain.openGuide( Const sFile : String; mog : TfrmMainOpenGuide; lEntry : LongInt; iStartingLine : Integer ) : TForm;
Var
  oWindow : TfrmGuide;
  i       : Integer;
  sNeedle : String;
Begin

  // Assume that we need a new window.
  oWindow := Nil;

  // If we should try and recycle a window...
  If mog = mogRecycle Then
  Begin

    // Clean up the guide name.
    sNeedle := AnsiLowerCase( ExpandUNCFileName( defaultGuideLocation( sFile ) ) );

    // Look thru the child windows, trying to find one that is using the same
    // guide.
    For i := 0 To MDIChildCount - 1 Do
      // If the child form is a guide window...
      If MDIChildren[ i ] Is TfrmGuide Then
        // If it's looking at the guide that's being opened...
        If AnsiLowerCase( ExpandUNCFileName( TfrmGuide( MDIChildren[ i ] ).NortonGuide.Guide ) ) = sNeedle Then
        Begin
          // ...use this window.
          oWindow := TfrmGuide( MDIChildren[ i ] );
          oWindow.bringToFront();
          setFocus();
          Break;
        End;

  End;

  // If we don't have a window to work with...
  If oWindow = Nil Then
    // ...create one.
    oWindow := TfrmGuide.create( self );

  // Open the guide.
  Result := openGuide( sFile, oWindow, lEntry, iStartingLine );
  
End;

/////

Function TfrmMain.focusedGuide : TwegLibNortonGuide;
Begin

  // If there is an MDI child kicking about...
  If ActiveMDIChild <> Nil Then
    // ...return a pointer to its Norton Guide component.
    Result := TfrmGuide( ActiveMDIChild ).NortonGuide
  Else
    // ...otherwise return Nil.
    Result := Nil;

End;

/////

Function TfrmMain.focusedViewer : TwegLibNGEntryViewer;
Begin

  // If there is an MDI child kicking about...
  If ActiveMDIChild <> Nil Then
    // ...return a pointer to its Norton Guide entry viewer component.
    Result := TfrmGuide( ActiveMDIChild ).NGEntry
  Else
    // ...otherwise return Nil.
    Result := Nil;

End;

/////

Function TfrmMain.focusedEntry : TwegLibNGEntry;
Begin

  // If there is an MDI child kicking about...
  If ActiveMDIChild <> Nil Then
    // ...return a pointer to its Norton Guide entry component.
    Result := TfrmGuide( ActiveMDIChild ).NGEntry.Entry
  Else
    // ...otherwise return Nil.
    Result := Nil;

End;

/////

Procedure TfrmMain.actFileOpenExecute( Sender : TObject );
Var
  i : Integer;
Begin

  // Point the open dialog at the default guide directory.
  odGuide.InitialDir := NGSettings.DefaultGuideDirectory;
  
  // Fire off the guide open dialog.
  If odGuide.execute() Then
    // If the user selected something, attempt to open what they selected.
    For i := 0 To odGuide.Files.Count - 1 Do
      openGuide( odGuide.Files[ i ], openType( actOptionsRecycleFileOpen ) );

End;

/////

Procedure TfrmMain.actFileExitExecute( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmMain.saveWindowState;
Var
  i       : Integer;
  oReg    : TRegistry;
  sPrefix : String;
Begin

  // Show a busy cursor while we're doing this.
  With TwegBusyCursor.create() Do
    Try

      oReg := TRegistry.create();

      With oReg Do
        Try

          // Open the registry key for remembering the window position. Create
          // it if it doesn't exist.
          If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_POSITION ] ), True ) Then
            Try
              // Save the size and location of the window.
              wegSaveWindowState( self, oReg );
            Finally
              // Close the window position/size key.
              closeKey();
            End;

          // Save the toolbar positions.
          TBRegSavePositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_MAIN_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

          // Now save the colours.
          NGColours.saveToRegistry( wegRegistryKey( REG_COLOURS ), oReg );

          // Now save the preferences.
          NGSettings.saveToRegistry( wegRegistryKey( REG_PREFERENCES ), oReg );

          // Now save the details of the open child windows. First, if there is
          // already a key of that name, delete it.
          If keyExists( wegRegistryKey( [ REG_MAIN_WINDOW, REG_CHILDREN ] ) ) Then
            deleteKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_CHILDREN ] ) );

          // Now re-create the key and write the child window list.
          If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_CHILDREN ] ), True ) Then
            Try

              // Loop over the child windows, saving their state.
              For i := 0 To MDIChildCount - 1 Do
              Begin
                sPrefix := IntToStr( ( MDIChildCount - 1 ) - i ) + '.';
                writeString(  sPrefix + REG_GUIDE,    TfrmGuide( MDIChildren[ i ] ).NortonGuide.Guide );
                writeInteger( sPrefix + REG_ENTRY,    TfrmGuide( MDIChildren[ i ] ).NGEntry.Entry.Offset );
                writeInteger( sPrefix + REG_LINE,     TfrmGuide( MDIChildren[ i ] ).NGEntry.ItemIndex );
                writeInteger( sPrefix + REG_TOP_LINE, TfrmGuide( MDIChildren[ i ] ).NGEntry.TopIndex );
                wegSaveWindowState( MDIChildren[ i ], oReg, sPrefix );
              End;

              // Remember if the guide manager was open.
              writeBool( REG_GUIDE_MANAGER_OPEN, frmGuideManager.Visible );

              // Remember if the bookmark manager was open.
              writeBool( REG_BOOMKARK_MANAGER_OPEN, frmBookmarks.Visible );

              // Remember if the global finder was open.
              writeBool( REG_GLOBAL_FINDER_OPEN, frmGlobalFind.Visible );

            Finally
              // Close the child windows key.
              closeKey();
            End;

          // Save the MRU lists.
          If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_MRU ] ), True ) Then
            Try
              writeString( REG_MRU_NAMES,  slMRUFiles.CommaText );
              writeString( REG_MRU_TITLES, slMRUTitles.CommaText );
            Finally
              // Close the MRU key.
              closeKey();
            End;

          // Misc preferences.
          If openKey( wegRegistryKey( REG_PREFERENCES ), True ) Then
            Try
              writeBool( REG_RECYCLE_FILE_OPENS,          actOptionsRecycleFileOpen.Checked );        
              writeBool( REG_RECYCLE_FILE_REOPENS,        actOptionsRecycleFileReOpen.Checked );        
              writeBool( REG_RECYCLE_DDE_OPENS,           actOptionsRecycleDDE.Checked );
              writeBool( REG_RECYCLE_COMMAND_LINE_OPENS,  actOptionsRecycleCommandLine.Checked );
              writeBool( REG_RECYCLE_DRAG_AND_DROP_OPENS, actOptionsRecycleDragAndDrop.Checked );
              writeBool( REG_COLOUR_PRINTING,             actOptionsPrintColour.Checked );
              writeBool( REG_BOL_SHORT_SEARCHING,         actOptionsBOLShortSearch.Checked );
            Finally
              // Close the preferences key.
              closeKey();
            End;

        Finally
          // Free the registry object.
          free();
        End;

    Finally
      // Restore the old cursor.
      free();
    End;

End;

/////

Procedure TfrmMain.loadWindowState;
Var
  oReg     : TRegistry;
  i        : Integer;
  bDone    : Boolean;
  sGuide   : String;
  lOffset  : LongInt;
  iLine    : Integer;
  iTopLine : Integer;
  sPrefix  : String;
  oForm    : TForm;
Begin

  oReg := TRegistry.create();

  With oReg Do
    Try

      // Open the registry key for remembering the window position.
      If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_POSITION ] ), False ) Then
        Try
          // Try and load the size and location of the window.
          wegRestoreWindowState( self, oReg );
        Finally
          // Close the window position/size key.
          closeKey();
        End;

      // Load the toolbar positions.
      TBRegLoadPositions( self, HKEY_CURRENT_USER, wegRegistryKey( [ REG_MAIN_WINDOW, REG_POSITION, REG_TOOLBARS ] ) );

      // Now load the colours.
      NGColours.loadFromRegistry( wegRegistryKey( REG_COLOURS ), oReg );

      // Now load the preferences.
      NGSettings.loadFromRegistry( wegRegistryKey( REG_PREFERENCES ), oReg );

      // Now load the child windows.
      If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_CHILDREN ] ), False ) Then
        Try

          // Get ready to keep looping until we can't find any window details.
          i     := 0;
          bDone := False;

          // Loop until we're done.
          While Not bDone Do
            Try

              sPrefix := IntToStr( i ) + '.';
              
              // Load the details of the child window.
              sGuide   := readString(  sPrefix + REG_GUIDE    );
              lOffset  := readInteger( sPrefix + REG_ENTRY    );
              iLine    := readInteger( sPrefix + REG_LINE     );
              iTopLine := readInteger( sPrefix + REG_TOP_LINE );

              // If we got this far without an exception we can open the window.
              oForm := openGuide( sGuide, mogNew, lOffset, iLine );

              // If it looks like we managed to open the window...
              If oForm <> Nil Then
              Begin
                // ... try and restore the window's size and location.
                wegRestoreWindowState( oForm, oReg, sPrefix );
                TfrmGuide( oForm ).NGEntry.TopIndex := iTopLine;
                // Ensure that the item index is the selected line.
                TfrmGuide( oForm ).NGEntry.Selected[ TfrmGuide( oForm ).NGEntry.ItemIndex ] := True; 
              End;

              // Get ready for the next window.
              Inc( i );

            Except
              // Exception thrown, assume we're done.
              bDone := True;
            End;

            Try
              // If the guide manager was previously open...
              If readBool( REG_GUIDE_MANAGER_OPEN ) Then
                // ...open it.
                frmGuideManager.show();
            Except
              // GNDN.
            End;

            Try
              // If the bookmark manager was previously open...
              If readBool( REG_BOOMKARK_MANAGER_OPEN ) Then
                // ...open it.
                frmBookmarks.show();
            Except
              // GNDN.
            End;

            Try
              // If the global finder was previously open...
              If readBool( REG_GLOBAL_FINDER_OPEN ) Then
                // ...open it.
                frmGlobalFind.show();
            Except
              // GNDN.
            End;

        Finally
          // Close the child window key.
          closeKey();
        End;

      // Load the MRU lists.
      If openKey( wegRegistryKey( [ REG_MAIN_WINDOW, REG_MRU ] ), False ) Then
        Try

          Try
            slMRUFiles.CommaText  := readString( REG_MRU_NAMES );
            slMRUTitles.CommaText := readString( REG_MRU_TITLES );
          Except
            slMRUFiles.clear();
            slMRUTitles.clear();
          End;

        Finally
          // Close the MRU key.
          closeKey();
        End;

      // Ensure we refresh the MRU menu.
      refreshMRUMenu();

      // Misc preferences.
      If openKey( wegRegistryKey( REG_PREFERENCES ), False ) Then
        Try
          Try
            actOptionsRecycleFileOpen.Checked    := readBool( REG_RECYCLE_FILE_OPENS );
            actOptionsRecycleFileReOpen.Checked  := readBool( REG_RECYCLE_FILE_REOPENS );
            actOptionsRecycleDDE.Checked         := readBool( REG_RECYCLE_DDE_OPENS );
            actOptionsRecycleCommandLine.Checked := readBool( REG_RECYCLE_COMMAND_LINE_OPENS );
            actOptionsRecycleDragAndDrop.Checked := readBool( REG_RECYCLE_DRAG_AND_DROP_OPENS );
            actOptionsPrintColour.Checked        := readBool( REG_COLOUR_PRINTING );
            actOptionsBOLShortSearch.Checked     := readBool( REG_BOL_SHORT_SEARCHING );
          Except
            // GNDN.
          End;
        Finally
          // Close the preferences key.
          closeKey();
        End;

    Finally
      // Free the registry object.
      free();
    End;

End;

/////

Procedure TfrmMain.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin
  // Save the state of the window.
  saveWindowState();
  // Ensure that the guide manager gets to save its data.
  frmGuideManager.close();
  // Ensure that the global finder gets to save its data.
  frmGlobalFind.close();
  // Ensure that the bookmarks window gets to save its data.
  frmBookmarks.close();
End;

/////

Procedure TfrmMain.FormShow( Sender : TObject );
Var
  i : Integer;
Begin

  // Load the previous state of the window.
  loadWindowState();

  // Let Windows know that we'll accept dropped files.
  DragAcceptFiles( Handle, True );

  // Have a look at the command line and see if we've been asked to open
  // anything.
  If ParamCount() > 0 Then
    // Seems we have, try and open guides from the command line.
    For i := 1 To ParamCount() Do
      openGuide( ParamStr( i ), openType( actOptionsRecycleCommandLine ) );
      
End;

/////

Procedure TfrmMain.actWindowUpdate( Sender : TObject );
Begin
  TAction( Sender ).Enabled := ( MDIChildCount > 0 );
End;

/////

Procedure TfrmMain.actWindowCascadeExecute( Sender : TObject );
Begin
  cascade();
End;

/////

Procedure TfrmMain.actWindowTileHorizontalExecute( Sender : TObject );
Begin
  TileMode := tbHorizontal;
  tile();
End;

/////

Procedure TfrmMain.actWindowTileVerticalExecute( Sender : TObject );
Begin
  TileMode := tbVertical;
  tile();
End;

/////

Procedure TfrmMain.FormCreate( Sender : TObject );
Begin

  // Create the MRU lists.
  slMRUFiles  := TStringList.create();
  slMRUTitles := TStringList.create();

End;

/////

Procedure TfrmMain.FormDestroy( Sender : TObject );
Begin

  // Free the MRU lists.
  slMRUFiles.free();
  slMRUTitles.free();

End;

/////

Procedure TfrmMain.rememberInMRU( oGuide : TwegLibNortonGuide );
Var
  i      : Integer;
  sFile  : String;
  sTitle : String;
Begin

  // Always work in lower case.
  sFile := AnsiLowerCase( oGuide.Guide );
  // Menu safe title.
  sTitle := StringReplace( oGuide.Title, '&', '&&', [ rfReplaceAll ] );

  // Is the guide in the MRU list?
  i := slMRUFiles.indexOf( sFile );

  If i > -1 Then
  Begin
    // The file is in the list, delete it from where it is now.
    slMRUFiles.delete( i );
    slMRUTitles.delete( i );
  End
  Else
  Begin
    // It isn't in the list so we'll be adding it. First
    // we ensure that the MRU won't grow more than required.
    If slMRUFiles.Count = MRU_MAX Then
    Begin
      slMRUFiles.delete( slMRUFiles.Count - 1 );
      slMRUTitles.delete( slMRUTitles.Count - 1 );
    End;
  End;

  // Put the file at the top of the list.
  slMRUFiles.insert( 0, sFile );
  slMRUTitles.insert( 0, sTitle );

  // Refresh the MRU menu.
  refreshMRUMenu();
  
End;

/////

Procedure TfrmMain.reopenFromMRU( oSender : TObject );
Begin

  // If we've been called from someone we know...
  If oSender Is TTBItem Then
    // ...deal with the call.
    openGuide( slMRUFiles[ TTBItem( oSender ).Tag ], openType( actOptionsRecycleFileReOpen ) );

End;

/////

Procedure TfrmMain.mnuFileReOpenPopup( Sender : TTBCustomItem; FromLink : Boolean );
Begin
End;

/////

Procedure TfrmMain.refreshMRUMenu;
ResourceString
  RSReOpen = 'Reopen "%s"';
Var
  i : Integer;
Begin

  // Clear the menu.
  mnuFileReOpen.clear();

  // If there are items in the MRU list...
  If slMRUTitles.Count > 0 Then
  Begin

    // Populate the reopen menu list.
    For i := 0 To slMRUTitles.Count - 1 Do
    Begin
      mnuFileReOpen.add( TTBItem.create( mnuFileReOpen ) );
      With mnuFileReOpen.Items[ i ] Do
      Begin
        Caption    := slMRUTitles[ i ];
        Hint       := Format( RSReOpen, [ slMRUTitles[ i ] ] );
        OnClick    := reopenFromMRU;
        ImageIndex := 0;
        Tag        := i;
      End;
    End;

    // Enable the reopen menu item.
    mnuFileReOpen.Enabled    := True;
    tbFileOpen.DropdownCombo := True;

  End
  Else
  Begin
    // Disable the reopen menu item.
    mnuFileReOpen.Enabled    := False;
    tbFileOpen.DropdownCombo := False;
  End;

End;

/////

Procedure TfrmMain.refreshChildWindows;
Var
  i : Integer;
Begin

  // Refresh the guide windows.
  For i := MDIChildCount - 1 DownTo 0 Do
    With TfrmGuide( MDIChildren[ i ] ) Do
    Begin
      NortonGuide.readSettings();
      NGEntry.redisplay();
    End;

  // Refresh the global find window.
  frmGlobalFind.NortonGuide.readSettings();
  
End;

/////

Procedure TfrmMain.actFileCloseAllUpdate( Sender : TObject );
Begin
  actFileCloseAll.Enabled := MDIChildCount > 0;
End;

/////

Procedure TfrmMain.actFileCloseAllExecute( Sender : TObject );
Var
  i : Integer;
Begin
  For i := MDIChildCount - 1 DownTo 0 Do MDIChildren[ i ].close();
End;

/////

Procedure TfrmMain.actFileGuideManagerExecute( Sender : TObject );
Begin
  frmGuideManager.show();
End;

/////

Procedure TfrmMain.actOptionsGuidePreferencesExecute( Sender : TObject );
ResourceString
  RSCaption = 'Guide Preferences';
Begin

  // Call the edit interface for the guide settings.
  If NGSettings.edit( RSCaption ) Then
  Begin
    // If the user appeared to edit something we get all of the windows to
    // refresh themselves.
    refreshChildWindows();
    // Save the new settings.
    NGSettings.saveToRegistry( wegRegistryKey( REG_PREFERENCES ) );
  End;

End;

/////

Procedure TfrmMain.actOptionsGuideColoursExecute( Sender : TObject );
ResourceString
  RSCaption = 'Guide Colours';
Begin

  // Call the edit interface for the colour settings.
  If NGColours.edit( RSCaption ) Then
  Begin
    // If the user appeared to edit something we get all of the windows to
    // refresh themselves.
    refreshChildWindows();
    // Save the new colours.
    NGColours.saveToRegistry( wegRegistryKey( REG_COLOURS ) );
  End;

End;

/////

Procedure TfrmMain.actHelpAboutExecute( Sender : TObject );
Begin

  With TfrmAbout.create( self ) Do
    Try
      showModal();
    Finally
      free();
    End;

End;

/////

Procedure TfrmMain.actFileGlobalFindExecute( Sender : TObject );
Begin
  frmGlobalFind.show();
End;

/////

Procedure TfrmMain.acceptFiles( Var msg : TMessage );
Var
  acFileName : Array[ 0..MAX_PATH - 1 ] Of Char;
  iFiles     : Integer;
  i          : Integer;
Begin

  // Work out how many files are being dropped.
  iFiles := DragQueryFile( msg.WParam, $FFFFFFFF, acFileName, MAX_PATH );

  // Loop over the dropped files, opening them.
  For i := 0 To iFiles - 1 Do
  Begin
    DragQueryFile( msg.WParam, i, acFileName, MAX_PATH );
    openGuide( acFileName, openType( actOptionsRecycleDragAndDrop ) );
  End;

End;

/////

Procedure TfrmMain.actFileBookmarksExecute( Sender : TObject );
Begin
  frmBookmarks.show();
End;

/////

Procedure TfrmMain.actOptionsPrinterSetupExecute( Sender : TObject );
Begin
  psdGuide.execute();
End;

/////

Procedure TfrmMain.ExecuteExecuteMacro( Sender : TObject; Msg : TStrings );

  Procedure DDEOpenFile( slParams : TStringList );
  ResourceString
    RSNoFileGiven = 'DDE Error:'#13#10#13#10'No file given for "open" macro.';
  Begin

    If slParams.Count > 0 Then
      openGuide( slParams[ 0 ], openType( actOptionsRecycleDDE ) )
    Else
    Begin
      MessageBeep( MB_ICONERROR );
      MessageDlg( RSNoFileGiven, mtError, [ mbOk ], 0 );
    End;

  End;

  Procedure DDESetGlobalSearch( slParams : TStringList );
  Var
    sSetting : String;
    sValue   : String;
  Begin

    If slParams.Count > 1 Then
    Begin

      // If there isn't a find in progress.
      If Not frmGlobalFind.NGFind.Finding Then
      Begin

        // Ensure that the global finder is visible.
        If Not frmGlobalFind.Visible Then frmGlobalFind.show();
        
        // Get the setting.
        sSetting := AnsiLowerCase( Trim( slParams[ 0 ] ) );
        // Get the value.
        sValue := AnsiLowerCase( Trim( slParams[ 1 ] ) );

        // Act on the setting.
        If sSetting = 'guides' Then
        Begin
          If sValue = 'all' Then
            frmGlobalFind.setSearchAll()
          Else
            frmGlobalFind.setSearchCurrent();
        End
        Else If sSetting = 'shorts' Then
          frmGlobalFind.setShortSearch( sValue = 'on' )
        Else If sSetting = 'longs' Then
          frmGlobalFind.setLongSearch( sValue = 'on' )
        Else If sSetting = 'matchcase' Then
          frmGlobalFind.setMatchCase( sValue = 'on' )
        Else If sSetting = 'regexp' Then
          frmGlobalFind.setRegExpSearch( sValue = 'on' );

      End;

    End;
    
  End;
  
  Procedure DDEGlobalSearch( slParams : TStringList );
  ResourceString
    RSNoSearchStringGiven = 'DDE Error:'#13#10#13#10'No search string given for "gsearch" macro.';
  Begin

    If slParams.Count > 0 Then
    Begin

      // If there isn't a find in progress.
      If Not frmGlobalFind.NGFind.Finding Then
        With frmGlobalFind Do
        Begin
          // Ensure that the global finder is visible.
          If Not Visible Then show();
          // Set the search string.
          setSearchText( slParams[ 0 ] );
          // Bring the window to the front.
          bringToFront();
          // Start the search.
          startSearch();
        End;

    End
    Else
    Begin
      MessageBeep( MB_ICONERROR );
      MessageDlg( RSNoSearchStringGiven, mtError, [ mbOk ], 0 );
    End;

  End;

Var
  slCall    : TStringList;
  sFunction : String;
  i         : Integer;
Begin

  // Ensure the user can see the application.
  Application.restore();

  // For each line in the macro...
  For i := 0 To Msg.Count - 1 Do
  Begin

    // Create a string list to hold the macro line.
    slCall := TStringList.create();

    Try

      // Create a string list out of the macro line.
      slCall.CommaText := Msg[ i ];

      // Extract the function.
      sFunction := AnsiLowerCase( Trim( slCall[ 0 ] ) );
      slCall.delete( 0 );

      // Act on the function.
      If sFunction = 'open' Then
        DDEOpenFile( slCall )
      Else If sFunction = 'gsearchset' Then
        DDESetGlobalSearch( slCall )  
      Else If sFunction = 'gsearch' Then
        DDEGlobalSearch( slCall );
        
    Finally
      // Free the string list used for breaking up the call.
      slCall.free();
    End;

  End;

End;

/////

Procedure TfrmMain.actOptionsCheckItemExecute( Sender : TObject );
Begin
  TAction( Sender ).Checked := Not TAction( Sender ).Checked;
End;

/////

Function TfrmMain.openType( oAction : TAction ) : TfrmMainOpenGuide;
Begin
  If oAction.Checked Then Result := mogRecycle Else Result := mogNew;
End;

/////

Procedure TfrmMain.FormDragOver( Sender, Source : TObject; X, Y : Integer; State : TDragState; Var Accept : Boolean );
Begin
  // We accept dropped items from the guide manager, the bookmark window and
  // the global finder.
  Accept := ( Source = frmGuideManager.lvGuides ) Or
            ( Source = frmBookmarks.lvBookmarks ) Or
            ( Source = frmGlobalFind.lbHits     );
End;

/////

Procedure TfrmMain.FormDragDrop( Sender, Source : TObject; X, Y : Integer );
Begin

  // If the user is dropping a guide from the guide manager...
  If Source = frmGuideManager.lvGuides Then
    // ...open it.
    frmGuideManager.actGuidesOpenExecute( Sender )
  // Perhaps it's from the bookmark window?
  Else If Source = frmBookmarks.lvBookmarks Then
    // ...yup, open the bookmark.
    frmBookmarks.actBookmarksOpenExecute( Sender )
  // How about the global finder?
  Else If Source = frmGlobalFind.lbHits Then
    // ..that's it, open the hit.
    frmGlobalFind.actFindOpenExecute( Sender );
    
End;

End.
