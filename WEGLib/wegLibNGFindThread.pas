{* File.......: wegLibNGFindThread.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Thread class for searching entries in a list of Norton Guides.
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

Unit wegLibNGFindThread;

Interface

Uses
  Classes,
  wegLibNortonGuide;
  
Type

  {** Record for holding a "hit" when doing a global find. }
  TwegLibNGFindHit = Record
    {** Name of the guide the hit was found in }
    sGuide : String;
    {** The text of the line the hit was found in }
    sText : String;
    {** The offset of the entry the hit was found in }
    lEntry : LongInt;
    {** The line on which the hit was found }
    iLine : Integer;
  End;

  {** Search styles. }
  TwegLibNGFindStyle = ( wlfsShorts, wlfsLongs );
  {** Set of search styles. }
  TwegLibNGFindStyles = Set Of TwegLibNGFindStyle;

  {** Global find thread class. }
  TwegLibNGFindThread = Class( TThread )

  Protected

    {** List of guides to search }
    FGuides : TStringList;
    {** Number of the guide we're working on }
    FGuide : Integer;
    {** Flag to say if the search is running or not }
    bRunning : Boolean;

    {** Procedure to set the guide list }
    Procedure setGuideList( slGuides : TStringList ); Virtual;
    {** Get the running status }
    Function getRunningStatus : Boolean; Virtual;
    {** Get the guide count }
    Function getGuideCount : Integer; Virtual;
    {** Do the finding }
    Procedure execute; Override;
    {** Called when the search starts }
    Procedure started; Virtual;
    {** Called when the search finishes }
    Procedure finished; Virtual;
    {** Called when we start searching a new guide }
    Procedure newGuide; Virtual;
    {** Called when we start searching a new entry }
    Procedure newEntry; Virtual;
    {** Called when we hit something }
    Procedure hit; Virtual;
    {** Called when it looks like we've got a bad regular expression }
    Procedure badRegExp; Virtual;

  Public

    {** Pointer to the Norton Guide component we'll be using to read the guides }
    oNG : TwegLibNortonGuide;
    {** What we're looking for }
    sFind : String;
    {** Should we match the case when searching? }
    bMatchCase : Boolean;
    {** Should we be doing a regular expression match? }
    bRegExp : Boolean;
    {** Pointer to the regular expression engine }
    oRegExp : Variant;
    {** Details of the last hit }
    rHit : TwegLibNGFindHit;
    {** Style of search }
    fsFindStyle : TwegLibNGFindStyles;
    {** Pointer to a procedure to call when we're starting the search }
    pStarted : Procedure Of Object;
    {** Pointer to a procedure to call when we've finished the search }
    pFinished : Procedure Of Object;
    {** Pointer to a procedure to call when we start searching a new guide }
    pNewGuide : Procedure Of Object;
    {** Poinrer to a procedure to call when we start looking at a new entry }
    pNewEntry : Procedure Of Object;
    {** Pointer to a procedure to call when we hit something }
    pHit : Procedure Of Object;
    {** Pointer to a procedure to call when it looks like we've got a bad regualr expression }
    pBadRegExp : Procedure Of Object;

    {** Write-only access to the guides list }
    Property Guides : TStringList Write setGuideList;
    {** Read-only access to the number of the guide we're working on }
    Property Guide : Integer Read FGuide;
    {** Read-only access to the count of guides we've got to work on }
    Property GuideCount : Integer Read getGuideCount;
    {** Are we running? }
    Property Running : Boolean Read getRunningStatus;

    {** Constructor }
    Constructor create;
    {** Destructor }
    Destructor destroy; Override;

  End;

Implementation

Uses
  ComObj,
  SysUtils,
  wegLibNGEntry;

/////

Constructor TwegLibNGFindThread.create;
Begin

  // Start out suspended.
  Inherited create( True );

  // We want to be freed when we terminate.
  FreeOnTerminate := True;
  
  // Create the guide list.
  FGuides := TStringList.create();

End;

/////

Destructor TwegLibNGFindThread.destroy;
Begin

  // Free the guide list.
  FGuides.free();

  Inherited;

End;

/////

Procedure TwegLibNGFindThread.execute;

  Function ContainsRegExp( Const s : String ) : Boolean;
  Begin
    Result := oRegExp.test( s );
  End;

  Function ContainsText( Const s : String ) : Boolean;
  Begin
    If bMatchCase Then
      Result := Pos( sFind, s ) > 0
    Else
      Result := Pos( sFind, AnsiUpperCase( s ) ) > 0;
  End;
  
  Function Contains( Const s : String ) : Boolean;
  Begin
    If bRegExp Then
      Result := ContainsRegExp( s )
    Else
      Result := ContainsText( s );
  End;

Var
  oEntry    : TwegLibNGEntry;
  i         : Integer;
  bLookHere : Boolean;
Begin

  Try

    // Mark that we're working.
    bRunning := True;

    Try

      // Let our owner know that we've started.
      synchronize( started );

      // Starting with the first guide, obviously.
      FGuide := 0;

      // Get things ready.
      If bRegExp Then
      Begin
        oRegExp.IgnoreCase := Not bMatchCase;
        oRegExp.Pattern    := sFind;
      End
      Else If Not bMatchCase Then
        sFind := AnsiUpperCase( sFind );

      // While we've not been asked to terminate and we've got guides to look at.
      While ( Not Terminated ) And ( FGuide < FGuides.Count ) Do
      Begin

        // Try and open the next guide.
        oNG.Guide := FGuides[ FGuide ];

        // If we managed to open the guide.
        If oNG.isOpen() Then
        Begin

          // Mark that we're looking at a new guide.
          synchronize( newGuide );

          // Go to the start of the guide.
          oNG.goTop();

          // While we've not been asked to terminate and we're not at the end
          // of the guide...
          While ( Not Terminated ) And ( Not oNG.isEOF() ) Do
          Begin

            // Figure out if we should search the next availabel entry.
            If oNG.LookingAtShort Then
              bLookHere := wlfsShorts In fsFindStyle
            Else If oNG.LookingAtLong Then
              bLookHere := wlfsLongs In fsFindStyle
            Else
              bLookHere := False;

            // Going to look?
            If bLookHere Then
            Begin

              // Load the entry.
              oEntry := oNG.loadEntry();

              Try

                // Mark that we're looking at a new entry.
                synchronize( newEntry );

                // Loop over each of the lines looking for a hit.
                For i := 0 To oEntry.LineCount - 1 Do
                Begin

                  // If it looks like we're been asked to terminate, get out.
                  If Terminated Then Break;

                  // Found anything?
                  If Contains( oEntry.StrippedLines[ i ] ) Then
                    With rHit Do
                    Begin

                      // Fill in the hit record.
                      sGuide := oNG.Guide;
                      sText  := oEntry.StrippedLines[ i ];
                      lEntry := oEntry.Offset;
                      iLine  := i;

                      // Let our caller know we've found something.
                      synchronize( hit );

                    End;

                End;

              Finally
                // Free the entry we were looking at.
                oNG.freeEntry( oEntry );
              End;

            End
            Else
            Begin
              // We're not going to look so just skip the current entry.
              oNG.nextEntry();
              synchronize( newEntry );
            End;

          End;

        End;

        // On to the next guide.
        Inc( FGuide );

      End;

    Finally
      // Let our owner know that we've finished.
      synchronize( finished );
      // Ensure that the guide isn't left looking at anything.
      oNG.Guide := '';
      // Mark that we're finished.
      bRunning := False;
    End;

  Except
    // OLE exception? Probably a bad regular expression.
    On EOleException Do synchronize( badRegExp );
    On EOleError     Do synchronize( badRegExp );
  End;
  
End;

/////

Procedure TwegLibNGFindThread.started;
Begin
  If Assigned( pStarted ) Then pStarted();
End;

/////

Procedure TwegLibNGFindThread.finished;
Begin
  If Assigned( pFinished ) Then pFinished();
End;

/////

Procedure TwegLibNGFindThread.newGuide;
Begin
  If Assigned( pNewGuide ) Then pNewGuide();
End;

/////

Procedure TwegLibNGFindThread.newEntry;
Begin
  If Assigned( pNewEntry ) Then pNewEntry();
End;

/////

Procedure TwegLibNGFindThread.hit;
Begin
  If Assigned( pHit ) Then pHit();
End;

/////

Procedure TwegLibNGFindThread.badRegExp;
Begin
  If Assigned( pBadRegExp ) Then pBadRegExp();
End;

/////

Procedure TwegLibNGFindThread.setGuideList( slGuides : TStringList );
Begin
  // All we actually do is copy the content of the string list, we don't keep
  // a pointer to the one we're passed.
  FGuides.Text := slGuides.Text;
End;

/////

Function TwegLibNGFindThread.getRunningStatus : Boolean;
Begin
  Result := ( Not ( Suspended Or Terminated ) ) And bRunning;
End;

/////

Function TwegLibNGFindThread.getGuideCount : Integer;
Begin
  Result := FGuides.Count;
End;

End.
