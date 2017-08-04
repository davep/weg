{* File.......: wegUtils.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Utility code.
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

Unit wegUtils;

Interface

Uses
  Forms,
  Registry,
  Controls;

Type

  {** Class for displaying a busy cursor }
  TwegBusyCursor = Class

  Protected

    {** The saved cursor }
    csrSave : TCursor;

  Public

    {** Cosntructor }
    Constructor create;

    {** Destructor }
    Destructor destroy; Override;

  End;

{** Return a WEG registry key }
Function wegRegistryKey( Const sName : String = '' ) : String; Overload;
{** Return a WEG registry key built from a list of names }
Function wegRegistryKey( aNames : Array Of String ) : String; Overload;
{** Save the size and location of a window, it is assumed that the desired key is already open }
Procedure wegSaveWindowState( oForm : TCustomForm; oReg : TRegistry; Const sPrefix : String = '' );
{** Restore the size and location of a window, it is assumed that the desired key is already open }
Procedure wegRestoreWindowState( oForm : TCustomForm; oReg : TRegistry; Const sPrefix : String = '' );
{** Get the version of WEG }
Function wegVersion : String;
{** Fire off a URL }
Procedure wegFireURL( Const sURL : String );

Implementation

Uses
  Windows,
  ShellAPI,
  SysUtils,
  Dialogs;

Const
  {** Registry entry for the top of the form }
  REG_FORM_TOP = 'Top';
  {** Registry entry for the left of the form }
  REG_FORM_LEFT = 'Left';
  {** Registry entry for the width of the form }
  REG_FORM_WIDTH = 'Width';
  {** Registry entry for the height of the form }
  REG_FORM_HEIGHT = 'Height';
  {** Registry entry for the maximized flag }
  REG_FORM_MAXIMIZED = 'Maximized';

/////

Constructor TwegBusyCursor.create;
Begin
  // Remember the current cursor.
  csrSave := Screen.Cursor;
  // Set the cursor to a hour glass.
  Screen.Cursor := crHourGlass;
End;

/////

Destructor TwegBusyCursor.destroy;
Begin
  // Restore the cursor.
  Screen.Cursor := csrSave;
  // Call the super.
  Inherited;
End;

/////

Function wegRegistryKey( Const sName : String ) : String;
Begin
  Result := IncludeTrailingBackSlash( 'Software\davep.org\WEG\' + sName );
End;

/////

Function wegRegistryKey( aNames : Array Of String ) : String;
Var
  i : Integer;
Begin

  // Start out with an empty string.
  Result := '';

  // Build the name from the array.
  For i := Low( aNames ) To High( aNames ) Do
    Result := Result + IncludeTrailingBackslash( aNames[ i ] );

  // Return the registry key.
  Result := wegRegistryKey( Result );

End;

/////

Procedure wegSaveWindowState( oForm : TCustomForm; oReg : TRegistry; Const sPrefix : String );
Var
  rPlacement : TWindowPlacement;
Begin

  // Init the size field of the placement record.
  rPlacement.length := SizeOf( TWindowPlacement );

  // Try and get the window's placement.
  If GetWindowPlacement( oForm.Handle, @rPlacement ) Then
  Begin

    // We got it, save the placement details of the widnow.

    // Write the maximized flag.
    oReg.writeBool( sPrefix + REG_FORM_MAXIMIZED, oForm.WindowState = wsMaximized );

    // Save the main location details.
    With rPlacement.rcNormalPosition Do
    Begin
      oReg.writeInteger( sPrefix + REG_FORM_TOP,    Top          );
      oReg.writeInteger( sPrefix + REG_FORM_LEFT,   Left         );
      oReg.writeInteger( sPrefix + REG_FORM_WIDTH,  Right - Left );
      oReg.writeInteger( sPrefix + REG_FORM_HEIGHT, Bottom - Top )
    End;

  End;

End;

/////

Procedure wegRestoreWindowState( oForm : TCustomForm; oReg : TRegistry; Const sPrefix : String );
Begin

  // Try and load the size and location of the window.
  Try
    oForm.Top    := oReg.readInteger( sPrefix + REG_FORM_TOP    );
    oForm.Left   := oReg.readInteger( sPrefix + REG_FORM_LEFT   );
    oForm.Width  := oReg.readInteger( sPrefix + REG_FORM_WIDTH  );
    oForm.Height := oReg.readInteger( sPrefix + REG_FORM_HEIGHT );
  Except
    // We don't do anything.
  End;

  // Try the maximized flag.
  Try
    If oReg.readBool( sPrefix + REG_FORM_MAXIMIZED ) Then oForm.WindowState := wsMaximized;
  Except
    // We don't do anything.
  End;

End;

/////

Function wegVersion : String;
Var
  pcBuff    : PChar;
  dwBuffLen : DWord;
  dwDummy   : DWord;
  pVersion  : Pointer;
  uiVerLen  : UInt;
Begin

  Result    := '';
  dwBuffLen := GetFileVersionInfoSize( PChar( Application.ExeName ), dwDummy );

  If dwBuffLen <> 0 Then
  Begin

    Try
      GetMem( pcBuff, dwBuffLen + 1 );
      Try
        If GetFileVersionInfo( PChar( Application.ExeName ), 0, dwBuffLen, pcBuff ) Then
          If VerQueryValue( pcBuff, '\\StringFileInfo\\080904E4\\ProductVersion', pVersion, uiVerLen ) Then
            Result := PChar( pVersion );
      Finally
        FreeMem( pcBuff );
      End;
    Finally
      { Do Nothing }
    End;

  End;

End;

/////

Procedure wegFireURL( Const sURL : String );
ResourceString
  RSError = 'Unable to invoke URL. Perhaps Windows isn''t configured to handle this type of URL?';
Begin
  If ShellExecute( Application.MainForm.Handle, nil, PChar( sURL ), PChar( '' ), PChar( '' ), SW_SHOWNORMAL ) <= 32 Then
  Begin
    MessageBeep( MB_ICONERROR );
    MessageDlg( RSError, mtError, [ mbOk ], 0 );
  End;
End;

End.
