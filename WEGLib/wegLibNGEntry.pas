{* File.......: wegLibNGEntry.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Norton Guide entry class.
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

Unit wegLibNGEntry;

Interface

Uses
  Classes,
  wegLibNGSeeAlso,
  wegLibNGLineParser;

Type

  {** Type for a line in a NG entry }
  TwegLibNGEntryLine = Record

    {** The text of the line }
    sText : String;
    {** The text of the line with all control characters removed }
    sStripped : String;
    {** Have we stripped the text yet? }
    bStripped : Boolean;
    {** The offset of the line if it points somewhere else }
    lOffset : LongInt;

  End;

  {** Norton Guide entry class }
  TwegLibNGEntry = Class

  Protected

    {** The lines in the entry }
    FLines : Array Of TwegLibNGEntryLine;
    {** The offset in the file that this entry came from }
    FOffset : LongInt;
    {** The number of lines in the entry }
    FLineCount : Integer;
    {** Does this entry have a see also list? }
    FHasSeeAlso : Boolean;
    {** The see-also list, if this entry has one }
    FSeeAlso : TwegLibNGSeeAlso;
    {** The line on the parent that points to this entry }
    FParentLine : Integer;
    {** Offset of the parent for this entry }
    FParent : LongInt;
    {** Number of the parent menu of this entry }
    FParentMenu : Integer;
    {** Number of the parent menu prompt of this entry }
    FParentPrompt : Integer;
    {** Offset of the previous entry }
    FPrevious : LongInt;
    {** Offset of the next entry }
    FNext : LongInt;

    {** The type of the entry }
    iType : Integer;
    {** The size of the entry }
    iSize : Integer;
    {** Control code stripping object }
    oStripper : TwegLibNGLineStripper;

    {** Get a line from the entry }
    Function getLine( i : Integer ) : String; Virtual;
    {** Get a stripped line from the entry }
    Function getStrippedLine( i : Integer ) : String; Virtual;
    {** Gen an offset from the entry }
    Function getOffset( i : Integer ) : LongInt; Virtual;
    {** Get if this is a short entry }
    Function getIsShort : Boolean; Virtual;
    {** Get if this is a long entry }
    Function getIsLong : Boolean; Virtual;
    {** Read the entry as a short entry }
    Procedure readShort( hNG : TStream ); Virtual;
    {** Read the entry as a long entry }
    Procedure readLong( hNG : TStream ); Virtual;
    {** Read the text for the entry }
    Procedure readText( hNG : TStream ); Virtual;
    {** Strip the control characters from the passed text }
    Function stripControls( sRaw : String ) : String; Virtual;
    {** Is there a previous entry }
    Function getHasPrevious : Boolean; Virtual;
    {** Is there a next entry? }
    Function getHasNext : Boolean; Virtual;
    {** Is there a parent entry? }
    Function getHasParent : Boolean; Virtual;
    {** Is there a child entry? }
    Function getHasChild( i : Integer ) : Boolean; Virtual;

  Public

    {** Do OEM to ANSI conversion on the text we read? }
    bOEMToANSI : Boolean;

    {** Get the count of lines in the entry }
    Property LineCount : Integer Read FLineCount;
    {** Access to the lines in the entry }
    Property Lines[ i : Integer ] : String Read getLine; Default;
    {** Access to the stripped lines in the entry }
    Property StrippedLines[ i : Integer ] : String read getStrippedLine;
    {** Access to the offsets in the guide entry }
    Property Offsets[ i : Integer ] : LongInt Read getOffset;
    {** Read-only access to the offset in the file that this entry came from }
    Property Offset : LongInt Read FOffset;
    {** Read-only access to the see also flag }
    Property HasSeeAlso : Boolean Read FHasSeeAlso;
    {** Read-only access to the see also list }
    Property SeeAlso : TwegLibNGSeeAlso Read FSeeAlso;
    {** Read-only access to the parent line }
    Property ParentLine : Integer Read FParentLine;
    {** Read-only access to the offset of the parent of this entry }
    Property Parent : LongInt Read FParent;
    {** Number of the parent menu of this entry }
    Property ParentMenu : Integer Read FParentMenu;
    {** Number of the parent menu prompt of this entry }
    Property ParentMenuPrompt : Integer Read FParentPrompt;
    {** Offset of the previous entry }
    Property Previous : LongInt Read FPrevious;
    {** Offset of the next entry }
    Property Next : LongInt Read FNext;
    {** Is this a short entry? }
    Property IsShort : Boolean Read getIsShort;
    {** Is this a long entry? }
    Property IsLong : Boolean Read getIsLong;
    {** Is there a previous entry? }
    Property HasPrevious : Boolean Read getHasPrevious;
    {** Is there a next entry? }
    Property HasNext : Boolean Read getHasNext;
    {** Is there a parent entry? }
    Property HasParent : Boolean Read getHasParent;
    {** Is there a child entry? }
    Property HasChild[ i : Integer ] : Boolean Read getHasChild;

    {** Constructor }
    Constructor create; Virtual;
    
    {** Destructor }
    Destructor destroy; Override;

    {** Load an entry from the passed stream }
    Procedure load( hNG : TStream ); Virtual;
    {** Is the passed offset a valid offset? }
    Function validOffset( lOffset : LongInt ) : Boolean; Virtual;
    {** Is the passed menu ID vaid? }
    Function validMenu( iMenu : Integer ) : Boolean; Virtual;
    {** Load the source lines into a string list }
    Procedure getSourceLines( sl : TStringList ); Virtual;
    {** Load the text of the lines into a string list }
    Procedure getLines( sl : TStringList ); Virtual;

  End;

Implementation

Uses
  wegLibUtils;

/////

Constructor TwegLibNGEntry.create;
Begin

  Inherited;
  
  // Create the line stripper.
  oStripper := TwegLibNGLineStripper.create();
  
End;

/////

Destructor TwegLibNGEntry.destroy;
Begin

  // Nil out the lines array.
  FLines := Nil;
  // Free any see-also list we've got.
  FSeeAlso.free();
  // Free the line stripper.
  oStripper.free();

  Inherited;
  
End;

/////

Function TwegLibNGEntry.getLine( i : Integer ) : String;
Begin
  Result := FLines[ i ].sText;
End;

/////

Function TwegLibNGEntry.getStrippedLine( i : Integer ) : String;
Begin

  // If the line in question hasn't been stripped already
  If Not FLines[ i ].bStripped Then
  Begin
    FLines[ i ].sStripped := stripControls( FLines[ i ].sText );
    FLines[ i ].bStripped := True;
  End;

  // Return the stripped version of the line.
  Result := FLines[ i ].sStripped;

End;

/////

Function TwegLibNGEntry.getOffset( i : Integer ) : LongInt;
Begin
  Result := FLines[ i ].lOffset;
End;

/////

Function TwegLibNGEntry.getIsShort : Boolean;
Begin
  Result := ( iType = wegLib_ENTRY_SHORT );
End;

/////

Function TwegLibNGEntry.getIsLong : Boolean;
Begin
  Result := ( iType = wegLib_ENTRY_LONG );
End;

/////

Procedure TwegLibNGEntry.load( hNG : TStream );
Begin

  // Free any previously used see-also list.
  FSeeAlso.free();

  // Create a new see-also list.
  FSeeAlso            := TwegLibNGSeeAlso.create();
  FSeeAlso.bOEMToANSI := bOEMToANSI;
  
  // Read the "header" information from the entry.
  FOffset       := hNG.seek( 0, soFromCurrent );
  iType         := wegLibReadWord( hNG );
  iSize         := wegLibReadWord( hNG );
  FLineCount    := wegLibReadWord( hNG );
  FHasSeeAlso   := ( wegLibReadWord( hNG ) > 0 );
  FParentLine   := wegLibReadWord( hNG );
  FParent       := wegLibReadLong( hNG );
  FParentMenu   := wegLibReadWord( hNG );
  FParentPrompt := wegLibReadWord( hNG );
  FPrevious     := wegLibReadLong( hNG );
  FNext         := wegLibReadLong( hNG );

  // Do some tidying up.
  If FPrevious     = -1    Then FPrevious     := 0;
  If FNext         = -1    Then FNext         := 0;
  If FParent       = -1    Then FParent       := 0;
  If FParentMenu   = $FFFF Then FParentMenu   := -1;
  If FParentPrompt = $FFFF Then FParentPrompt := -1;

  // Size the lines array.
  SetLength( FLines, FLineCount );
  
  // Read the rest of the entry, depending on its type.
  If IsShort Then
    readShort( hNG )
  Else
    readLong( hNG );
      
End;

/////

Procedure TwegLibNGEntry.readShort( hNG : TStream );
Var
  i : Integer;
Begin

  // First we load the offsets of the entries that each of the lines point at.
  For i := 0 To FLineCount - 1 Do
  Begin

    // Skip unknown word.
    wegLibReadWord( hNG, wlrtNoDecrypt );

    // Now read the offset that the line points at.
    FLines[ i ].lOffset := wegLibReadLong( hNG );
    
  End;

  // Now we load the text of the entry.
  readText( hNG );

End;

/////

Procedure TwegLibNGEntry.readLong( hNG : TStream );
Begin

  // First we load the text of the entry.
  readText( hNG );

  // If this long entry has a see-also list...
  If FHasSeeAlso Then
    FSeeAlso.load( hNG );
    
End;

/////

Procedure TwegLibNGEntry.readText( hNG : TStream );
Const
  MAX_LINE_LENGTH = 512;
Var
  i : Integer;
Begin

  For i := 0 To FLineCount - 1 Do
    FLines[ i ].sText := wegLibExpand( wegLibReadStringZ( hNG, MAX_LINE_LENGTH ) );

End;

/////

Function TwegLibNGEntry.validOffset( lOffset : LongInt ) : Boolean;
Begin
  Result := ( lOffset > 0 );
End;

/////

Function TwegLibNGEntry.validMenu( iMenu : Integer ) : Boolean;
Begin
  Result := ( iMenu >= 0 );
End;

/////

Function TwegLibNGEntry.stripControls( sRaw : String ) : String;
Begin

  With oStripper Do
  Begin
    parse( sRaw, bOEMToANSI );
    Result := sStripped;
  End;

End;

/////

Function TwegLibNGEntry.getHasPrevious : Boolean;
Begin
  Result := validOffset( FPrevious );
End;

/////

Function TwegLibNGEntry.getHasNext : Boolean;
Begin
  Result := validOffset( FNext );
End;

/////

Function TwegLibNGEntry.getHasParent : Boolean;
Begin
  Result := validOffset( FParent );
End;

/////

Function TwegLibNGEntry.getHasChild( i : Integer ) : Boolean;
Begin
  Result := validOffset( FLines[ i ].lOffset );
End;

/////

Procedure TwegLibNGEntry.getSourceLines( sl : TStringList );
Var
  i : Integer;
Begin
  For i := 0 To LineCount - 1 Do sl.add( Lines[ i ] );
End;

/////

Procedure TwegLibNGEntry.getLines( sl : TStringList );
Var
  i : Integer;
Begin
  For i := 0 To LineCount - 1 Do sl.add( StrippedLines[ i ] );
End;

End.
