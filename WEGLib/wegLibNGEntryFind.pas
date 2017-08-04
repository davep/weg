{* File.......: wegLibNGEntryFind.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Component for finding text in a Norton Guide entry.
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

Unit wegLibNGEntryFind;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  SysUtils,
  wegLibNGEntry;

Type

  {** Type of error thrown by the TweLibNGEntryFind component }
  EwegLibNGEntryFindError = Exception;

  {** Type of a callback procedure called when something is found. }
  TwegLibNGEntryFindHit = Procedure( Sender : TObject; iLine : Integer ) Of Object;

  {** Component for finding text in a Norton Guide entry. }
  TwegLibNGEntryFind = Class( TComponent )

  Protected

    {** Procedure called when something is found. }
    FOnHit : TwegLibNGEntryFindHit;
    {** Procedure called when something isn't found. }
    FOnMiss : TNotifyEvent;
    {** Procedure called when a bad regular expression is given }
    FOnBadRegExp : TNotifyEvent;
    {** Should we do beginning-of-line searching first in shorts? }
    FBOLSearchFirstInShorts : Boolean;
    {** Have we done the init stuff? }
    bInitDone : Boolean;
    {** Flag to record if we can or can't do regular expressions }
    bCanRegExp : Boolean;
    {** Pointer to the VBScript regular expression object. }
    oRegExp : Variant;
    {** Pointer to the entry that we're looking in. }
    oEntry : TwegLibNGEntry;
    {** Number of the next line to search. }
    iNextLine : Integer;

    {** Init the find object. }
    Procedure doInit; Virtual;
    {** Get the value of bCanRegExp, doing the init stuff if it hasn't been done. }
    Function getCanRegExp : Boolean; Virtual;
    {** Get the flag that says if a search is active. }
    Function getSearching : Boolean; Virtual;
    {** Called when we get a hit. }
    Procedure hit( iLine : Integer ); Virtual;
    {** Called when we get a miss. }
    Procedure miss; Virtual;
    {** Called when we've got a bad regular expression }
    Procedure badRegExp; Virtual;

  Public

    {** Read only access to the regular expression ability flag. }
    Property CanRegExp : Boolean Read getCanRegExp;
    {** Read only access to the searching flag. }
    Property Searching : Boolean Read getSearching;

    {** Constructor }
    Constructor create( AOwner : TComponent ); Override;

    {** Start a new search. }
    Procedure startSearch( oHaystack : TwegLibNGEntry; iStartLine : Integer ); Virtual;
    {** Find the next instance of the search string in the entry we're looking at. }
    Procedure find( Const sFind : String; bCaseSensitive : Boolean = False; bRegExp : Boolean = False ); Virtual;
    {** Start searching from the top of the entry }
    Procedure goTop; Virtual;
    {** Mark that a search has finished. This breaks the link with the entry. }
    Procedure finishSearch; Virtual;

  Published

    {** Procedure called when something is found. }
    Property OnHit : TwegLibNGEntryFindHit Read FOnHit Write FOnHit;
    {** Procedure called when something isn't found. }
    Property OnMiss : TNotifyEvent Read FOnMiss Write FOnMiss;
    {** Procedure called when a bad regular expression is given }
    Property OnBadRegExp : TNotifyEvent Read FOnBadRegExp Write FOnBadRegExp;
    {** Should we do beginning-of-line searching first in shorts? }
    Property BOLSearchFirstInShorts : Boolean Read FBOLSearchFirstInShorts Write FBOLSearchFirstInShorts;

  End;

{** Register components with Delphi's IDE }
Procedure Register;

Implementation

Uses
  ComOBJ;

/////

Constructor TwegLibNGEntryFind.create( AOwner : TComponent );
Begin

  Inherited;

  // Initial values.
  bInitDone               := False;
  bCanRegExp              := False;
  FBOLSearchFirstInShorts := False;

End;

/////

Procedure TwegLibNGEntryFind.doInit;
Begin

  // If we've not done the init yet...
  If Not bInitDone Then
  Begin

    Try
      // Try and create a regular expression engine object.
      oRegExp := CreateOLEObject( 'VBScript.Regexp' );
      // Got it, remember that we got it.
      bCanRegExp := True;
    Except
      // There was an exception, this probably means that the class we're after
      // isn't available on this machine. Remember this.
      bCanRegExp := False;
    End;

    // Record that we've done the init.
    bInitDone := True;

  End;

End;

/////

Function TwegLibNGEntryFind.getCanRegExp : Boolean;
Begin

  // Ensure that the init has been done.
  doInit();

  // Return the "can regexp" flag.
  Result := bCanRegExp;
  
End;

/////

Function TwegLibNGEntryFind.getSearching : Boolean;
Begin
  Result := oEntry <> Nil;
End;

/////

Procedure TwegLibNGEntryFind.startSearch( oHaystack : TwegLibNGEntry; iStartLine : Integer );
Begin

  // Ensure that the init has been done.
  doInit();

  // Where we're looking.
  oEntry := oHaystack;

  // Where we'll start looking.
  iNextLine := iStartLine;
  
End;

/////

Procedure TwegLibNGEntryFind.find( Const sFind : String; bCaseSensitive : Boolean; bRegExp : Boolean );

  Function ContainsRegExp( Const s : String ) : Boolean;
  Begin
    Result := oRegExp.test( s );
  End;

  Function ContainsText( Const s : String ) : Boolean;
  Begin
    If bCaseSensitive Then
      Result := Pos( sFind, s ) > 0
    Else
      Result := Pos( AnsiUpperCase( sFind ), AnsiUpperCase( s ) ) > 0;
  End;

  Function BeginsWithText( Const s : String ) : Boolean;
  Begin
    If bCaseSensitive Then
      Result := Copy( s, 1, Length( sFind ) ) = sFind
    Else
      Result := AnsiUpperCase( Copy( s, 1, Length( sFind ) ) ) = AnsiUpperCase( sFind );  
  End;

  Function Contains( Const s : String ) : Boolean;
  Begin
    If bRegExp Then
      Result := ContainsRegExp( s )
    Else
      Result := ContainsText( s );
  End;
  
ResourceString
  RSNoRegExp = 'Regular expression searches are not available.';
  RSNoStart  = 'Call to find without a call to startSearch.';
Var
  i    : Integer;
  bHit : Boolean;
  iHit : Integer;
Begin

  // If the user wants to do a regular expression search and we can't actually
  // do regular expressions...
  If bRegExp And ( Not bCanRegExp ) Then
    // ...complain.
    Raise EwegLibNGEntryFindError.create( RSNoRegExp );

  // If a search hasn't started...
  If Not Searching Then
    Raise EwegLibNGEntryFindError.create( RSNoStart );

  // If this is a regular expression search...
  If bRegExp Then
  Begin
    // ...prime the regular expression object.
    oRegExp.Pattern    := sFind;
    oRegExp.IgnoreCase := Not bCaseSensitive;
  End;

  // Look for the next hit.
  Try

    // Initialise the hit status.
    bHit := False;
    iHit := -1;

    // First, if we're supposed to do BOL searching in shorts and this isn't a
    // regular expression search and if the entry we're searching is a short...
    If FBOLSearchFirstInShorts And ( Not bRegExp ) And oEntry.IsShort Then
      For i := iNextLine To oEntry.LineCount - 1 Do
        If BeginsWithText( TrimLeft( oEntry.StrippedLines[ i ] ) ) Then
        Begin
          bHit      := True;
          iHit      := i;
          iNextLine := ( iHit + 1 );
          Break;
        End;

    // If the above wasn't relevant or we didn't find anything...
    If Not bHit Then
      // See if we can find a line that contains the search text.
      For i := iNextLine To oEntry.LineCount - 1 Do
        If Contains( oEntry.StrippedLines[ i ] ) Then
        Begin
          bHit      := True;
          iHit      := i;
          iNextLine := ( iHit + 1 );
          Break;
        End;

    // Call any callback procedures.
    If bHit Then
      hit( iHit )
    Else
      miss();

  Except
    // If there's an OLE exception that probably means that the user typed
    // in an invalid regular expression. Handle it as such.
    On EOleException Do badRegExp();
  End;

End;

/////

Procedure TwegLibNGEntryFind.goTop;
Begin
  iNextLine := 0;
End;

/////

Procedure TwegLibNGEntryFind.finishSearch;
Begin
  oEntry := Nil;
End;

/////

Procedure TwegLibNGEntryFind.hit( iLine : Integer );
Begin
  If Assigned( FOnHit ) Then FOnHit( self, iLine );
End;

/////

Procedure TwegLibNGEntryFind.miss;
Begin
  If Assigned( FOnMiss ) Then FOnMiss( self );
End;

/////

Procedure TwegLibNGEntryFind.badRegExp;
Begin
  If Assigned( FOnBadRegExp ) Then FOnBadRegExp( self );
End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNGEntryFind ] );
End;

End.
