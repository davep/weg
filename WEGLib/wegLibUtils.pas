{* File.......: wegLibUtils.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Utility functions and procedures.
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

Unit wegLibUtils;

Interface

Uses
  Classes;

Const
  {** Magic number of a Norton Guide compiled database }
  wegLib_MAGIC_NG = $474E;
  {** Magic number of an Expert Help compiled database }
  wegLib_MAGIC_EH = $4845;
  {** ID of a short entry }
  wegLib_ENTRY_SHORT = 0;
  {** ID of a long entry }
  wegLib_ENTRY_LONG = 1;
  {** ID of a menu entry }
  wegLib_ENTRY_MENU = 2;
  {** Version of WEGLib }
  wegLib_VERSION = '1.9';

Type
  {** Types of reads that can take place }
  TwegLibReadType = ( wlrtNoDecrypt, wlrtDecrypt );

{** Decrypt a byte }
Function wegLibDecrypt( c : Byte ) : Byte;
{** Read a byte from the passed stream }
Function wegLibReadByte( h : TStream; wlrt : TwegLibReadType = wlrtDecrypt ) : Byte;
{** Read a word from the passed stream }
Function wegLibReadWord( h : TStream; wlrt : TwegLibReadType = wlrtDecrypt ) : Word;
{** Read a long from the passed stream }
Function wegLibReadLong( h : TStream; wlrt : TwegLibReadType = wlrtDecrypt ) : LongInt;
{** Expand the passed text }
Function wegLibExpand( Const s : String ) : String;
{** Get a string of a given length from the passed stream }
Function wegLibReadString( h : TStream; wLen : Word; wlrt : TwegLibReadType = wlrtDecrypt ) : String;
{** Get a null terminated string from the passed stream }
Function wegLibReadStringZ( h : TStream; wLen : Word; wlrt : TwegLibReadType = wlrtDecrypt ) : String;
{** Convert OEM text to ANSI text }
Function wegLibOEMToANSI( Const s : String ) : String;
{** Convert a hex string into an integer }
Function wegLibHex2Int( s : String ) : Integer;
{** Convert a hex string into a character}
Function wegLibHex2Char( Const s : String ) : Char;

Implementation

Uses
  Windows,
  SysUtils;
  
/////

Function wegLibDecrypt( c : Byte ) : Byte;
Begin
  Result := ( c XOr $1A );
End;

/////

Function wegLibReadByte( h : TStream; wlrt : TwegLibReadType ) : Byte;
Var
  c : Byte;
Begin

  // Read the byte.
  h.read( c, 1 );

  // If we need to decrypt it...
  If wlrt = wlrtDecrypt Then
    // ...decrypt it...
    Result := wegLibDecrypt( c )
  Else
    // ...otherwise, just return it as is.
    Result := c;

End;

/////

Function wegLibReadWord( h : TStream; wlrt : TwegLibReadType ) : Word;
Var
  cLow  : Byte;
  cHigh : Byte;
Begin

  // Read the low byte.
  cLow := wegLibReadByte( h, wlrt );
  // Read the high byte.
  cHigh := wegLibReadByte( h, wlrt );
  // Combine them.
  Result := ( ( cHigh Shl 8 ) + cLow );

End;

/////

Function wegLibReadLong( h : TStream; wlrt : TwegLibReadType ) : LongInt;
Var
  wLow  : Word;
  wHigh : Word;
Begin

  // Read the low word.
  wLow := wegLibReadWord( h, wlrt );
  // Read the high word.
  wHigh := wegLibReadWord( h, wlrt );
  // Combine them.
  Result := ( ( wHigh Shl 16 ) + wLow );

End;

/////

Function wegLibExpand( Const s : String ) : String;
Var
  i       : Integer;
  iLen    : Integer;
  iRLE    : Integer;
  iExpand : Integer;
  bJump   : Boolean;
Begin

  // Init.
  iLen   := Length( s );
  Result := '';
  bJump  := False;

  For i := 1 To iLen Do
    // If the current character is an RLE marker and we're not at the end of
    // the string...
    If ( s[ i ] = #$ff ) And ( i < iLen ) Then
    Begin

      // We'll be jumping the next character...
      bJump := True;
      // ...because it's a count of spaces to unroll.
      iRLE := Ord( s[ i + 1 ] );

      // Is the RLE count the RLE marker too?
      If iRLE = $ff Then
        // Yes, it is, just stick in a couple of spaces. I'm not sure if this is
        // the right thing to do but I've seen at least one Norton Guide
        // database where something that renders as a normal line of spaces in
        // NG.exe and EG.exe is actually a whole line of "hard" spaces. If we
        // did normal RLE processing on that we'd get a massive line of spaces.
        // I don't know if this is the right thing to do but it seems to solve
        // the problem with no adverse effects elsewhere.
        Result := Result + '  '
      Else
        // Everything looks reasonably normal, do the unrolling.
        For iExpand := 1 To iRLE Do
          Result := Result + ' ';

    End
    // ...otherwise, if we're not supposed to jump the next character...
    Else If Not bJump Then
      // ...use it.
      Result := Result + s[ i ]
    Else
      // Reset the jump flag.
      bJump := False;

End;

/////

Function wegLibReadString( h : TStream; wLen : Word; wlrt : TwegLibReadType ) : String;
Var
  pBuff : PByteArray;
  i     : Integer;
Begin

  // Allocate memory for the buffer.
  GetMem( pBuff, wLen );

  Try

    // Read wLen bytes from the file into the buffer.
    h.read( pBuff^, wLen );

    // Decrypt the content of the buffer if necessary.
    If wlrt = wlrtDecrypt Then
      For i := 0 To ( wLen - 1 ) Do
        pBuff[ i ] := wegLibDecrypt( pBuff[ i ] );

    // Now turn the buffer into a string up to the first null byte.
    i      := 0;
    Result := '';
    While ( pBuff[ i ] <> 0 ) And ( i < wLen ) Do
    Begin
      Result := Result + Char( pBuff[ i ] );
      Inc( i );
    End;

  Finally
    // Free the buffer.
    FreeMem( pBuff );
  End;

End;

/////

Function wegLibReadStringZ( h : TStream; wLen : Word; wlrt : TwegLibReadType ) : String;
Var
  lSavOff : LongInt;
Begin

  // Remember where we are.
  lSavOff := h.seek( 0, soFromCurrent );

  // Load the string.
  Result := wegLibReadString( h, wLen, wlrt );

  // Now roll back to the next byte after the end of what was read.
  h.seek( lSavOff + Length( Result ) + 1, soFromBeginning );
  
End;

/////

Function wegLibOEMToANSI( Const s : String ) : String;
Var
  pBuff : PByteArray;
  iLen  : Integer;
  i     : Integer;
Begin

  Result := '';
  iLen   := Length( s );

  If iLen > 0 Then
  Begin

    // Allocate memory for the buffer.
    GetMem( pBuff, iLen );

    Try
      // Convert from OEM to ANSI.
      OEMToCharBuff( PChar( s ), PChar( pBuff ), iLen );
      // Copy the coverted buffer back to a string.
      i := 0;
      While ( pBuff[ i ] <> 0 ) And ( i < iLen ) Do
      Begin
        Result := Result + Chr( pBuff[ i ] );
        Inc( i );
      End;
    Finally
      // Free the buffer.
      FreeMem( pBuff );
    End;

  End;

End;

/////

Function wegLibHex2Int( s : String ) : Integer;
Var
  i     : Integer;
  iMult : Integer;
Begin

  // Initial setup.
  s      := AnsiUpperCase( s );
  Result := 0;
  iMult  := 1;

  // Loop from the end of the string to the start, working things out.
  For i := Length( s ) DownTo 1 Do
  Begin

    Case s[ i ] Of
      '0'..'9' : Result := Result + ( ( Ord( s[ i ] ) - Ord( '0' ) ) * iMult );
      'A'..'F' : Result := Result + ( ( Ord( s[ i ] ) - Ord( '7' ) ) * iMult );
    End;

    iMult := iMult * $10;
    
  End;

End;

/////

Function wegLibHex2Char( Const s : String ) : Char;
Begin

  // Convert the hex string into a character.
  Result := Chr( wegLibHex2Int( s ) );

  // If this is a null byte...
  If Result = #0 Then
    // ...use a space instead.
    Result := ' ';
    
End;

End.
