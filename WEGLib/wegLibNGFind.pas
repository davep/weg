{* File.......: wegLibNGFind.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Component for controlling a global find operation.
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

Unit wegLibNGFind;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  wegLibNortonGuide,
  wegLibNGFindThread;

Type

  {** Type of an event called when a hit happens }
  TwegLibNGFindHitEvent = Procedure( Sender : TObject; rHit : TwegLibNGFindHit ) Of Object;

  {** Component for controlling a global find operation. }
  TwegLibNGFind = Class( TComponent )

  Protected

    {** Pointer to the Norton Guide component we'll use to search each guide. }
    FNortonGuide : TwegLibNortonGuide;
    {** Search started event }
    FOnStartSearch : TNotifyEvent;
    {** Search finished event }
    FOnFinishedSearch : TNotifyEvent;
    {** Search of new guide started event }
    FOnNewGuide : TNotifyEvent;
    {** Search of new entry started event }
    FOnNewEntry : TNotifyEvent;
    {** Something was found }
    FOnHit : TwegLibNGFindHitEvent;
    {** Looks like a bad regular expression }
    FOnBadRegExp : TNotifyEvent;
    {** Style of search to conduct }
    FSearchStyle : TwegLibNGFindStyles;
    {** Do a regular expression search? }
    FRegExpSearch : Boolean;
    {** Match case when searching? }
    FMatchCase : Boolean;

    {** Pointer to a VBScript regular expression matcher object }
    oRegExp : Variant;
    {** Have we attempted to init the regular expression engine? }
    bRegExpInitDone : Boolean;
    {** Can we do regular expressions? }
    bCanRegExp : Boolean;
    {** Thread object that does the actual finding. }
    oFindThread : TwegLibNGFindThread;

    {** Handle notification messages }
    Procedure notification( AComponent : TComponent; Operation : TOperation ); Override;
    {** Init the regular expression engine }
    Procedure initRegExp; Virtual;
    {** Figure out if we're in the process of finding. }
    Function getFindStatus : Boolean; Virtual;
    {** Get the number of the guide we're looking at }
    Function getCurrentGuide : Integer; Virtual;
    {** Get the count of guides to be looked at }
    Function getGuideCount : Integer; Virtual;
    {** See if regular expression searches are available }
    Function getRegExpStatus : Boolean; Virtual;
    {** Do things when the search starts }
    Procedure startSearch; Virtual;
    {** Do things when the search finishes }
    Procedure finishedSearch; Virtual;
    {** Do things when we start to search a new guide }
    Procedure newGuide; Virtual;
    {** Search of new entry started event }
    Procedure newEntry; Virtual;
    {** Do things when a hit happens }
    Procedure hit; Virtual;
    {** Do things when it looks like we've got a bad regular expression }
    Procedure badRegExp; Virtual;

  Public

    {** Read-only flag to test if we're finding or not }
    Property Finding : Boolean Read getFindStatus;
    {** Return the number of the guide we're currently searching }
    Property CurrentGuide : Integer Read getCurrentGuide;
    {** Return the number of guides there are to be searched }
    Property GuideCount : Integer Read getGuideCount;
    {** Return the regular expression ability flag }
    Property CanRegExp : Boolean Read getRegExpStatus;

    {** Start a search operation }
    Procedure start( Const sFind : String; slGuides : TStringList ); Virtual;
    {** Stop a search operation }
    Procedure stop; Virtual;

  Published

    {** Pointer to the Norton Guide component we'll use to search each guide. }
    Property NortonGude : TwegLibNortonGuide Read FNortonGuide Write FNortonGuide;
    {** Search started event }
    Property OnStartSearch : TNotifyEvent Read FOnStartSearch Write FOnStartSearch;
    {** Search finished event }
    Property OnFinishedSearch : TNotifyEvent Read FOnFinishedSearch Write FOnFinishedSearch;
    {** Search of new guide started event }
    Property OnNewGuide : TNotifyEvent Read FOnNewGuide Write FOnNewGuide;
    {** Search of new entry started event }
    Property OnNewEntry : TNotifyEvent Read FOnNewEntry Write FOnNewEntry;
    {** Something was found }
    Property OnHit : TwegLibNGFindHitEvent Read FOnHit Write FOnHit;
    {** Looks like a bad regular expression }
    Property OnBadRegExp : TNotifyEvent Read FOnBadRegExp Write FOnBadRegExp;
    {** Style of search to conduct }
    Property SearchStyle : TwegLibNGFindStyles Read FSearchStyle Write FSearchStyle;
    {** Do a regular expression search? }
    Property RegExpSearch : Boolean Read FRegExpSearch Write FRegExpSearch;
    {** Match case when searching? }
    Property MatchCase : Boolean Read FMatchCase Write FMatchCase;

  End;

{** Register components with Delphi's IDE }
Procedure Register;

Implementation

Uses
  ComObj,
  SysUtils;

/////

Procedure TwegLibNGFind.notification( AComponent : TComponent; Operation : TOperation );
Begin

  Inherited;

  // Are we running in the IDE?
  If csDesigning In ComponentState Then
    // Is someone removing a component?
    If Operation = opRemove Then
    Begin
      // Yes, work out which one and kill our pointer to it.
      If AComponent = FNortonGuide Then
        FNortonGuide := Nil;
    End
    Else If Operation = opInsert Then
      // Yes, see if we can use it.
      If ( AComponent Is TwegLibNortonGuide ) And ( FNortonGuide = Nil ) Then
        FNortonGuide := TwegLibNortonGuide( AComponent );

End;

/////

Procedure TwegLibNGFind.initRegExp;
Begin

  // If we've not done this already...
  If Not bRegExpInitDone Then
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
    bRegExpInitDone := True;

  End;
  
End;

/////

Function TwegLibNGFind.getFindStatus : Boolean;
Begin

  If oFindThread = Nil Then
    Result := False
  Else
    Result := oFindThread.Running;

End;

/////

Function TwegLibNGFind.getCurrentGuide : Integer;
Begin
  If Finding Then Result := oFindThread.Guide Else Result := 0;
End;

/////

Function TwegLibNGFind.getGuideCount : Integer;
Begin
  If Finding Then Result := oFindThread.GuideCount Else Result := 0;
End;

/////

Function TwegLibNGFind.getRegExpStatus : Boolean;
Begin

  // Try and initalise the regular expression engine.
  initRegExp();
  
  // Return the flag.
  Result := bCanRegExp;
  
End;

/////

Procedure TwegLibNGFind.start( Const sFind : String; slGuides : TStringList );
Begin

  // Init the regular expression engine.
  initRegExp();
  
  // Create the finder thread.
  oFindThread := TwegLibNGFindThread.create();

  // Configure the thread.
  oFindThread.Guides      := slGuides;
  oFindThread.oNG         := FNortonGuide;
  oFindThread.pStarted    := startSearch;
  oFindThread.pFinished   := finishedSearch;
  oFindThread.pNewGuide   := newGuide;
  oFindThread.pNewEntry   := newEntry;
  oFindThread.pHit        := hit;
  oFindThread.pBadRegExp  := badRegExp;
  oFindThread.sFind       := sFind;
  oFindThread.fsFindStyle := FSearchStyle;
  oFindThread.bRegExp     := FRegExpSearch;
  oFindThread.oRegExp     := oRegExp;
  oFindThread.bMatchCase  := FMatchCase;

  // Start the search.
  oFindThread.resume();

End;

/////

Procedure TwegLibNGFind.stop;
Begin

  // Ask the thread to terminate.
  oFindThread.terminate();
  // Wait for it to terminate.
  oFindThread.waitFor();
  // Forget the thread (it's self freeing).
  oFindThread := Nil;

End;

/////

Procedure TwegLibNGFind.startSearch;
Begin
  // Call the search start event.
  If Assigned( FOnStartSearch ) Then FOnStartSearch( self );
End;

/////

Procedure TwegLibNGFind.finishedSearch;
Begin

  // Forget the thread (it's self freeing).
  oFindThread := Nil;

  // Call the search finished event.
  If Assigned( FOnFinishedSearch ) Then FOnFinishedSearch( self );
  
End;

/////

Procedure TwegLibNGFind.newGuide;
Begin
  // Call the new guide event.
  If Assigned( FOnNewGuide ) Then FOnNewGuide( self );
End;

/////

Procedure TwegLibNGFind.newEntry;
Begin
  // Call the new entry event.
  If Assigned( FOnNewEntry ) Then FOnNewEntry( self );
End;

/////

Procedure TwegLibNGFind.hit;
Begin
  // Call the hit event.
  If Assigned( FOnHit ) Then FOnHit( self, oFindThread.rHit );
End;

/////

Procedure TwegLibNGFind.badRegExp;
Begin
  // Call the bad regular expression event.
  If Assigned( FOnBadRegExp ) Then FOnBadRegExp( self );
End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNGFind ] );
End;

End.
 