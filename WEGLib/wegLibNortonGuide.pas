{* File.......: wegLibNortonGuide.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Norton Guide component.
 * Licence....: GNU General Public Licence (see below)
 *
 * WEGLib - Norton Guide Reader Library for Delphi.
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

Unit wegLibNortonGuide;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  wegLibNGMenu,
  wegLibNGEntry,
  wegLibNGSettings;

Type

  {** Type of a callback procedure for handling a guide error }
  TwegLibGuideErrorEvent = Procedure( Sender : TObject; Const sError : String ) Of Object;

  {** Norton Guide reading component }
  TwegLibNortonGuide = Class( TComponent )

  Protected

    {** Name of the currently open file }
    FGuide : String;
    {** The title of the guide }
    FTitle : String;
    {** The magic marker of the file }
    FMagic : Integer;
    {** Remember the physical size of the guide }
    FSize : LongInt;
    {** Count of menus in the guide }
    FMenuCount : Integer;
    {** The credits for the guide }
    FCredits : String;
    {** Menus for this guide }
    FMenus : Array Of TwegLibNGMenu;
    {** Do OEM to ANSI conversion on credits and gude entries? }
    FOEMToANSI : Boolean;
    {** The offset of the first entry in the guide }
    FFirstEntry : LongInt;
    {** Pointer to a procedure that will receive details of any guide errors }
    FOnError : TwegLibGuideErrorEvent;
    {** Pointer to a notification procedure that's called when a guide is opened }
    FOnOpen : TNotifyEvent;
    {** Pointer to a notification procedure that's called when a guide is closed }
    FOnClose : TNotifyEvent;
    {** Pointer to a wegLib settings component }
    FSettings : TwegLibNGSettings;

    {** Stream for the open guide }
    hNG : TFileStream;

    {** Handle notification messages }
    Procedure notification( AComponent : TComponent; Operation : TOperation ); Override;
    {** Throw an error }
    Procedure throwError( Const sError : String ); Virtual;
    {** Open a new guide }
    Procedure open( Const sGuide : String ); Virtual;
    {** Close the guide }
    Procedure close; Virtual;
    {** Read the header of the guide }
    Procedure readHeader; Virtual;
    {** Is the guide a valud guide? }
    Function isValid : Boolean; Virtual;
    {** Load the guide's menus }
    Procedure loadMenus; Virtual;
    {** Free the menus for this guide }
    Procedure freeMenus; Virtual;
    {** Access a menu from the memu array }
    Function getMenu( i : Integer ) : TwegLibNGMenu; Virtual;
    {** Skip an entry in the guide }
    Procedure skipEntry; Virtual;
    {** Return a string to identify the type of guide }
    Function getGuideType : String; Virtual;
    {** Return a long string to identify the type of guide }
    Function getLongGuideType : String; Virtual;
    {** Assuming we're sat at the start of an entry, get its type. }
    Function getCurrentEntryType : Integer; Virtual;
    {** Are we looking at a short entry? }
    Function getLookingAtShort : Boolean; Virtual;
    {** Are we looking at a long entry? }
    Function getLookingAtLong : Boolean; Virtual;
    {** Get the current byte position that we're at in the file }
    Function getCurrentOffset : LongInt; Virtual;

  Public

    {** The name of the currently open file, if set the file is opened }
    Property Guide : String Read FGuide Write open;
    {** Read-only access to the guide's title }
    Property Title : String Read FTitle;
    {** Read-only access to the guide's magic marker }
    Property Magic : Integer Read FMagic;
    {** Read-only access to the physical size of the guide }
    Property Size : LongInt Read FSize;
    {** Read-only access to the count of menus in the guide }
    Property MenuCount : Integer Read FMenuCount;
    {** Read-only access to the credits of the guide }
    Property Credits : String Read FCredits;
    {** Read-only access to the array of menus }
    Property Menus[ i : Integer ] : TwegLibNGMenu Read getMenu;
    {** Read-only access to a string ID for the type of the guide }
    Property GuideType : String Read getGuideType;
    {** Read-only access to a long string for the type of the guide }
    Property LongGuideType : String Read getLongGuideType;
    {** Read-only access to the offset of the first entry in the guide }
    Property FirstEntry : LongInt Read FFirstEntry;
    {** Get the current byte position that we're at in the file }
    Property CurrentOffset : LongInt Read getCurrentOffset;
    {** Read-only property to say if we're looking at a short entry }
    Property LookingAtShort : Boolean Read getLookingAtShort;
    {** Read-only property to say if we're looking at a long entry }
    Property LookingAtLong : Boolean Read getLookingAtLong;

    {** Constructor }
    Constructor create( AOwner : TComponent ); Override;
    {** Destructor }
    Destructor destroy; Override;

    {** Is there an open guide? }
    Function isOpen : Boolean; Virtual;
    {** Position the file pointer at the start of the entry at the given offset }
    Procedure gotoEntry( lOffset : LongInt ); Virtual;
    {** Go to the first entry in the guide }
    Procedure goTop; Virtual;
    {** Like skipEntry but expects that we're sat on the entry ID }
    Procedure nextEntry; Virtual;
    {** Load a Norton Guide entry from the current offset in the database }
    Function loadEntry : TwegLibNGEntry; Overload; Virtual;
    {** Load a Norton Guide entry from the given offset in the database }
    Function loadEntry( lOffset : LongInt ) : TwegLibNGEntry; Overload; Virtual;
    {** Free a previously loaded guide entry }
    Procedure freeEntry( Var oEntry : TwegLibNGEntry ); Virtual;
    {** Call this procedure when you want wegLib to re-read settings from the settings component }
    Procedure readSettings; Virtual;
    {** Does it look like we're at the end of the file? }
    Function isEOF : Boolean; Virtual;

  Published

    {** Do OEM to ANSI conversion on credits and gude entries? }
    Property OEMToANSI : Boolean Read FOEMToANSI Write FOEMToANSI;
    {** Pointer to a procedure that will receive details of any guide errors }
    Property OnError : TwegLibGuideErrorEvent Read FOnError Write FOnError;
    {** Pointer to a notification procedure that's called when a guide is opened }
    Property OnOpen : TNotifyEvent Read FOnOpen Write FOnOpen;
    {** Pointer to a notification procedure that's called when a guide is closed }
    Property OnClose : TNotifyEvent Read FOnClose Write FOnClose;
    {** Pointer to a wegLib settings component }
    Property Settings : TwegLibNGSettings Read FSettings Write FSettings;

  End;

{** Register components with Delphi's IDE }
Procedure Register;

Implementation

Uses
  SysUtils,
  wegLibUtils;

/////

Constructor TwegLibNortonGuide.create( AOwner : TComponent );
Begin

  Inherited;

  // By default we don't convert.
  FOEMToANSI := False;
  
End;

/////

Destructor TwegLibNortonGuide.destroy;
Begin

  // Ensure that the guide is closed.
  close();

  Inherited;
  
End;

/////

Procedure TwegLibNortonGuide.notification( AComponent : TComponent; Operation : TOperation );
Begin

  Inherited;

  // Are we running in the IDE?
  If csDesigning In ComponentState Then
    // Is someone removing a component?
    If Operation = opRemove Then
    Begin
      // Yes, work out which one and kill our pointer to it.
      If AComponent = FSettings Then
        FSettings := Nil;
    End
    Else If Operation = opInsert Then
      // Yes, see if we can use it.
      If ( AComponent Is TwegLibNGSettings ) And ( FSettings = Nil ) Then
        FSettings := TwegLibNGSettings( AComponent );

End;

/////

Procedure TwegLibNortonGuide.throwError( Const sError : String );
Begin
  If Assigned( FOnError ) Then FOnError( self, sError );
End;

/////

Procedure TwegLibNortonGuide.open( Const sGuide : String );
ResourceString
  RSNotAValidGuide = '"%s" is not a Norton Guide database'; 
Begin

  // Ensure that any previously opened guide is closed.
  close();

  // An empty guide name is the same as doing a close.
  If Trim( sGuide ) <> '' Then
  Begin

    // Read from any settings component.
    readSettings();

    // Now attempt to open the file.
    Try
      hNG := TFileStream.create( sGuide, fmOpenRead Or fmShareDenyNone );
    Except
      // There was an error, deal with it.
      On E : Exception Do throwError( E.Message );
    End;

    // Now, if we managed to open the file...
    If isOpen() Then
    Begin

      // ...read the header of the file.
      readHeader();

      // Is it a valid Norton Guide or Expert Help database?
      If isValid() Then
      Begin

        // Remember the name of the file.
        FGuide := sGuide;

        // If there are any menus...
        If FMenuCount > 0 Then
          // Load the menus.
          loadMenus();

        // The first entry should be where we are now.
        FFirstEntry := hNG.seek( 0, soFromCurrent );

        // Also work out the size.
        Try
          // Get the position of the last byte.
          FSize := hNG.seek( 0, soFromEnd );
        Finally
          // Go back to the start.
          goTop();
        End;

        // Handle any callback.
        If Assigned( FOnOpen ) Then
          FOnOpen( self );

      End
      Else
      Begin
        throwError( Format( RSNotAValidGuide, [ sGuide ] ) );
        close();
      End;

    End;

  End;

End;

/////

Procedure TwegLibNortonGuide.readHeader;

  Function EOL( i : Integer ) : String;
  Begin
    If i < 4 Then Result := #13#10 Else Result := '';
  End;

Var
  i : Integer;
Begin

  // Read the magic marker.
  FMagic := wegLibReadWord( hNG, wlrtNoDecrypt );

  // Skip an unknown word.
  wegLibReadWord( hNG, wlrtNoDecrypt );
  // And another one.
  wegLibReadWord( hNG, wlrtNoDecrypt );

  // Read the count of menus in the guide.
  FMenuCount := wegLibReadWord( hNG, wlrtNoDecrypt );

  // Read the title of the guide. We always OEM->ANSI this.
  FTitle := wegLibOEMToANSI( wegLibReadString( hNG, 40, wlrtNoDecrypt ) );

  // Read the credits for the guide.
  For i := 0 To 4 Do
    If FOEMToANSI Then
      FCredits := FCredits + wegLibOEMToANSI( wegLibReadString( hNG, 66, wlrtNoDecrypt ) ) + EOL( i )
    Else
      FCredits := FCredits + wegLibReadString( hNG, 66, wlrtNoDecrypt ) + EOL( i );

End;

/////

Function TwegLibNortonGuide.isValid : Boolean;
Begin
  Result := ( FMagic = wegLib_MAGIC_NG ) Or ( FMagic = wegLib_MAGIC_EH );
End;

/////

Procedure TwegLibNortonGuide.loadMenus;
Var
  i : Integer;
Begin

  // So far we've found no menus.
  i := 0;
  freeMenus();

  // Size the menu array.
  SetLength( FMenus, FMenuCount );

  // Loop over each entry in the guide, looking for menus and loading them.  
  Repeat

    Case wegLibReadWord( hNG ) Of

      // Skip a short or long entry.
      wegLib_ENTRY_SHORT,
      wegLib_ENTRY_LONG : skipEntry();

      // If it's a menu entry.
      wegLib_ENTRY_MENU :
      Begin
        // Create a new menu object.
        FMenus[ i ] := TwegLibNGMenu.create();
        // Load the menu.
        FMenus[ i ].load( hNG );
        // Increase the count of loaded menus.
        Inc( i );
      End;

    Else
      // Unknown entry ID, get out of here.
      Break;
    End;

  Until ( i = FMenuCount );

End;

/////

Procedure TwegLibNortonGuide.freeMenus;
Var
  i : Integer;
Begin

  // Free each of the menu objects.
  For i := Low( FMenus ) To High( FMenus ) Do
    FMenus[ i ].free();

  // Nil out the menu array.
  FMenus := Nil;
  
End;

/////

Function TwegLibNortonGuide.getMenu( i : Integer ) : TwegLibNGMenu;
Begin
  Result := FMenus[ i ];
End;

/////

Procedure TwegLibNortonGuide.close;
Begin

  // If we've got an open guide...
  If isOpen() Then
  Begin

    // ...close it.
    FreeAndNil( hNG );
    freeMenus();
    FGuide     := '';
    FTitle     := '';
    FMagic     := 0;
    FSize      := 0;
    FMenuCount := 0;
    FCredits   := '';

    // Handle any callback.
    If Assigned( FOnClose ) Then
      FOnClose( self );
      
  End;

End;

/////

Procedure TwegLibNortonGuide.skipEntry;
Begin
  // Ooo, look, one less that 23!
  hNG.seek( 22 + wegLibReadWord( hNG ), soFromCurrent );
End;

/////

Function TwegLibNortonGuide.isOpen : Boolean;
Begin
  Result := ( hNG <> Nil );
End;

/////

Function TwegLibNortonGuide.getGuideType : String;
ResourceString
  RSNG      = 'NG';
  RSEH      = 'EH';
  RSUnknown = '??';
Begin

  Case FMagic Of
    wegLib_MAGIC_NG : Result := RSNG;
    wegLib_MAGIC_EH : Result := RSEH;
  Else
    Result := RSUnknown;
  End;

End;

/////

Function TwegLibNortonGuide.getLongGuideType : String;
ResourceString
  RSNG      = 'Norton Guide';
  RSEH      = 'Expert Help';
  RSUnknown = 'unknown';
Begin

  Case FMagic Of
    wegLib_MAGIC_NG : Result := RSNG;
    wegLib_MAGIC_EH : Result := RSEH;
  Else
    Result := RSUnknown;
  End;

End;

/////

Procedure TwegLibNortonGuide.gotoEntry( lOffset : LongInt );
Begin
  // Seek to the passed offset in the file.
  hNG.seek( lOffset, soFromBeginning );
End;

/////

Procedure TwegLibNortonGuide.goTop;
Begin
  gotoEntry( FFirstEntry );
End;

/////

Procedure TwegLibNortonGuide.nextEntry;
Begin
  // Skip the ID byte.
  hNG.seek( 2, soFromCurrent );
  // Skip the entry.
  skipEntry();
End;

/////

Function TwegLibNortonGuide.loadEntry( lOffset : LongInt ) : TwegLibNGEntry;
Begin

  // Create the guide entry.
  Result := TwegLibNGEntry.create();

  // Pass on the OEM->ANSI setting.
  Result.bOEMToANSI := FOEMToANSI;

  // Jump to the entry.
  gotoEntry( lOffset );

  // Load the entry.
  Result.load( hNG );

  // Now settle on the next available entry.
  hNG.seek( Result.Offset + 2, soFromBeginning );
  skipEntry();
  
End;

/////

Function TwegLibNortonGuide.loadEntry : TwegLibNGEntry;
Begin
  Result := loadEntry( hNG.seek( 0, soFromCurrent ) );
End;

/////

Procedure TwegLibNortonGuide.freeEntry( Var oEntry : TwegLibNGEntry );
Begin
  FreeAndNil( oEntry );
End;

/////

Procedure TwegLibNortonGuide.readSettings;
Begin

  // If we've got a pointer to a settings component...
  If FSettings <> Nil Then
    // ..get the sttings from it.
    FOEMToANSI := FSettings.OEMToANSI;

End;

/////

Function TwegLibNortonGuide.isEOF : Boolean;
Var
  lCurrentPos : LongInt;
  lEOFPos     : LongInt;
Begin

  // Remember where we are.
  lCurrentPos := hNG.seek( 0, soFromCurrent );

  Try
    // See what the size of the file is.
    lEOFPos := hNG.seek( 0, soFromEnd );
  Finally
    // Restore the old position.
    hNG.seek( lCurrentPos, soFromBeginning );
  End;

  // Does it look like we're at the end?
  If lCurrentPos >= lEOFPos Then
    // Yes, that's EOF folks!
    Result := True
  Else
    // Otherwise we see if we're looking at a short or a long and, if not,
    // we assume we're at EOF.
    Result := ( Not LookingAtShort ) And ( Not LookingAtLong );

End;

/////

Function TwegLibNortonGuide.getCurrentEntryType : Integer;
Var
  lSavOff : LongInt;
Begin

  lSavOff := hNG.seek( 0, soFromCurrent );
  Try
    Result := wegLibReadWord( hNG );
  Finally
    hNG.seek( lSavOff, soFromBeginning );
  End;

End;

/////

Function TwegLibNortonGuide.getLookingAtShort : Boolean;
Begin
  Result := ( getCurrentEntryType() = wegLib_ENTRY_SHORT ); 
End;

/////

Function TwegLibNortonGuide.getLookingAtLong : Boolean;
Begin
  Result := ( getCurrentEntryType() = wegLib_ENTRY_LONG );
End;

/////

/////

Function TwegLibNortonGuide.getCurrentOffset : LongInt;
Begin
  Result := hNG.seek( 0, soFromCurrent );
End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNortonGuide ] );
End;

End.
 