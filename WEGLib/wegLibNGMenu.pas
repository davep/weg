{* File.......: wegLibNGMenu.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Norton Guide menu class.
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

Unit wegLibNGMenu;

Interface

Uses
  Classes;

Const
  {** Max length of a menu item }
  wegLib_MAX_MENU_ITEM_LEN = 128;

Type

  {** Norton Guide menu item }
  TwegLibNGMenuItem = Record

    {** The prompt of the menu item }
    sPrompt : String;
    {** The offset of the entry that this menu item points to }
    lOffset : LongInt;

  End;

  {** Norton Guide menu class }
  TwegLibNGMenu = Class

  Protected

    {** The title of the menu }
    FTitle : String;
    {** Array of prompts on this menu }
    FPrompts : Array Of TwegLibNGMenuItem;

    {** Get a prompt }
    Function getPrompt( i : Integer ) : String; Virtual;
    {** Get the offset of the entry that a prompt points at }
    Function getOffset( i : Integer ) : LongInt; Virtual;
    {** Get the count of prompts on this menu }
    Function getCount : Integer; Virtual;

  Public

    {** Do OEM to ANSI conversion on the text we read? }
    bOEMToANSI : Boolean;

    {** Access to the title of the menu }
    Property Title : String Read FTitle;
    {** Access to the prompts on the menu }
    Property Prompts[ i : Integer ] : String Read getPrompt; Default;
    {** Access to the offsets of the prompts on the menu }
    Property Offsets[ i : Integer ] : LongInt Read getOffset;
    {** Get the count of prompts on this menu }
    Property Count : Integer Read getCount;

    {** Destructor }
    Destructor destroy; Override;

    {** Load a menu from the passed file, note that we expect to be past the entry marker. }
    Procedure load( hNG : TFileStream ); Virtual;
    {** Add a prompt to the menu }
    Procedure add( Const sPrompt : String; lOffset : LongInt ); Virtual;

  End;

Implementation

Uses
  SysUtils,
  wegLibUtils;
  
/////

Destructor TwegLibNGMenu.destroy;
Begin

  // Nil out the prompts array.
  FPrompts := Nil;

  Inherited;

End;

/////

Procedure TwegLibNGMenu.load( hNG : TFileStream );
Var
  i        : Integer;
  iPrompts : Word;
Begin

  // Skip an unknown word.
  wegLibReadWord( hNG, wlrtNoDecrypt );

  // Read the count of prompts.
  iPrompts := wegLibReadWord( hNG );

  // Size the prompts array.
  SetLength( FPrompts, iPrompts - 1 );

  // Skip 20 unknown bytes.
  hNG.seek( 20, soFromCurrent );

  // First we load the offsets for each prompt on the menu.
  For i := 0 To ( iPrompts - 2 ) Do
    FPrompts[ i ].lOffset := wegLibReadLong( hNG );

  // Skip a number of unknown values.
  hNG.seek( iPrompts * ( SizeOf( LongInt ) * 2 ), soFromCurrent );

  // Load the title for the menu.
  If bOEMToANSI Then
    FTitle := wegLibOEMToANSI( Trim( wegLibExpand( wegLibReadStringZ( hNG, wegLib_MAX_MENU_ITEM_LEN ) ) ) )
  Else
    FTitle := Trim( wegLibExpand( wegLibReadStringZ( hNG, 50 ) ) );

  // Now load the text of each of the prompts on the menu.
  For i := 0 To ( iPrompts - 2 ) Do
    If bOEMToANSI Then
      FPrompts[ i ].sPrompt := wegLibOEMToANSI( Trim( wegLibExpand( wegLibReadStringZ( hNG, wegLib_MAX_MENU_ITEM_LEN ) ) ) )
    Else
      FPrompts[ i ].sPrompt := Trim( wegLibExpand( wegLibReadStringZ( hNG, wegLib_MAX_MENU_ITEM_LEN ) ) );

  // Skip a byte (I forget what it is, end of entry marker perhaps?).
  wegLibReadByte( hNG, wlrtNoDecrypt );

End;

/////

Function TwegLibNGMenu.getPrompt( i : Integer ) : String;
Begin
  Result := FPrompts[ i ].sPrompt;
End;

/////

Function TwegLibNGMenu.getOffset( i : Integer ) : LongInt;
Begin
  Result := FPrompts[ i ].lOffset;
End;

/////

Function TwegLibNGMenu.getCount : Integer;
Begin
  Result := Length( FPrompts );
End;

/////

Procedure TwegLibNGMenu.add( Const sPrompt : String; lOffset : LongInt );
Begin
  SetLength( FPrompts, Count + 1 );
  FPrompts[ Count ].sPrompt := sPrompt;
  FPrompts[ Count ].lOffset := lOffset;
End;

End.
