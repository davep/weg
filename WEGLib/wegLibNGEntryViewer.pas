{* File.......: wegLibNGEntryViewer.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Norton Guide entry viewing component.
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

Unit wegLibNGEntryViewer;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  SysUtils,
  StdCtrls,
  Windows,
  wegLibNortonGuide,
  wegLibNGEntry,
  wegLibNGColours;

Type

  {** Type of an exception thrown by this component }
  EwegLibNGEntryViewer = Class( Exception );

  {** Record for holding an item in the history }
  TwegLibNGEntryViewerHistoryItem = Record

    {** Offset of the entry }
    lOffset : LongInt;
    {** Line we were looking at }
    iLine : Integer;

  End;

  {** Type for holding a entry history }
  TwegLibNGEntryViewerHistory = Array Of TwegLibNGEntryViewerHistoryItem;

  {** Norton Guide entry viewing component }
  TwegLibNGEntryViewer = Class( TCustomListBox )

  Protected

    {** Pointer to the Norton Guide component }
    FNortonGuide : TwegLibNortonGuide;
    {** Pointer to the guide display colour component }
    FColours : TwegLibNGColours;
    {** The entry we're viewing }
    FEntry : TwegLibNGEntry;
    {** Type of cursor to display when we're sat over a link }
    FLinkCursor : TCursor;
    {** Type of cursor to display when we're sat over normal text }
    FTextCursor : TCursor;
    {** Callback for when a new entry is displayed }
    FOnNewEntry : TNotifyEvent;
    {** Callback for when an invalid link is found }
    FOnInvalidLink : TNotifyEvent;
    {** Should we jump to a linked entry on a single click? }
    FSingleClickJump : Boolean;

    {** Backtrack history }
    aBackHistory : TwegLibNGEntryViewerHistory;
    {** Foretrack history }
    aForeHistory : TwegLibNGEntryViewerHistory;
    {** Flag to say if we're navigating the history list }
    bDoingHistory : Boolean;

    {** Handle notification messages }
    Procedure notification( AComponent : TComponent; Operation : TOperation ); Override;
    {** Check that we've got a guide pointer }
    Function guideCheck : TwegLibNortonGuide; Virtual;
    {** Refresh the display }
    Procedure refreshDisplay; Virtual;
    {** Do things when a new entry is loaded and displayed }
    Procedure doNewEntry; Virtual;
    {** Ensure that the passed link is ok, call OnInvalidLink if it isn't }
    Function linkIsOk( lOffset : LongInt ) : Boolean; Virtual;
    {** Remember the current entry in the history list }
    Procedure rememberEntry( Var aHistory : TwegLibNGEntryViewerHistory ); Virtual;
    {** Move thru a history list}
    Procedure historyMove( Var aThis : TwegLibNGEntryViewerHistory; Var aOther : TwegLibNGEntryViewerHistory ); Virtual;
    {** Add a horizontal scroll bar } 
    Procedure addHorizontalScrollBar; Virtual;

    {** Handle a click }
    Procedure click; Override;
    {** Handle a double click }
    Procedure dblClick; Override;
    {** Handle a mouse move }
    Procedure mouseMove( ssShift : TShiftState; iX : Integer; iY : Integer ); Override;
    {** Handle an item draw event }
    Procedure drawItem( iIndex : Integer; rRect : TRect; State: TOwnerDrawState ); Override;

  Public

    {** Read-only access to the current entry }
    Property Entry : TwegLibNGEntry Read FEntry;

    {** Constructor }
    Constructor create( AOwner : TComponent ); Override;
    {** Destructor }
    Destructor destroy; Override;

    {** Display the entry at the given offset }
    Procedure display( lOffset : LongInt; iStartingLine : Integer = 0 ); Virtual;
    {** Load the first entry in the guide }
    Procedure displayFirstEntry; Virtual;
    {** Re-display the current entry }
    Procedure redisplay; Virtual;
    {** Can we navigate up from the current entry? }
    Function canNavigateUp : Boolean; Virtual;
    {** Can we navigate down from the current entry? }
    Function canNavigateDown : Boolean; Virtual;
    {** Can we navigate to the previous entry from the current entry? }
    Function canNavigatePrevious : Boolean; Virtual;
    {** Can we navigate to the next entry from the current entry? }
    Function canNavigateNext : Boolean; Virtual;
    {** Navigate up }
    Procedure navigateUp; Virtual;
    {** Navigate down }
    Procedure navigateDown; Virtual;
    {** Navigate to the previous entry }
    Procedure navigatePrevious; Virtual;
    {** Navigate to the next entry }
    Procedure navigateNext; Virtual;
    {** Can we move forwards in the history list? }
    Function canHistoryNext : Boolean; Virtual;
    {** Can we move backwards in the history list? }
    Function canHistoryPrevious : Boolean; Virtual;
    {** Move forwards in the history list }
    Procedure historyNext; Virtual;
    {** Move backwards in the history list }
    Procedure historyPrevious; Virtual;
    {** Populate the passed list with text, only do selected text if a range is selected }
    Procedure getText( sl : TStringList ); Virtual;
    {** Populate the passed list with source, only do selected text if a range is selected }
    Procedure getSource( sl : TStringList ); Virtual;
    {** Jump to a given line in the current entry }
    Procedure jumpToLine( iLine : Integer ); Virtual;

  Published

    {** Pointer to the Norton Guide component }
    Property NortonGuide : TwegLibNortonGuide Read FNortonGuide Write FNortonGuide;
    {** Pointer to the guide display colour component }
    Property Colours : TwegLibNGColours Read FColours Write FColours;
    {** Type of cursor to display when we're sat over a link }
    Property LinkCursor : TCursor Read FLinkCursor Write FLinkCursor;
    {** Type of cursor to display when we're sat over normal text }
    Property TextCursor : TCursor Read FTextCursor Write FTextCursor;
    {** Pointer to a notification procedure that's called when a new entry is loaded }
    Property OnNewEntry : TNotifyEvent Read FOnNewEntry Write FOnNewEntry;
    {** Callback for when an invalid link is found }
    Property OnInvalidLink : TNotifyEvent Read FOnInvalidLink Write FOnInvalidLink;
    {** Should we jump to a linked entry on a single click? }
    Property SingleClickJump : Boolean Read FSingleClickJump Write FSingleClickJump;

    // Publish some inherited properties.
    Property Align;
    Property Font;
    Property Hint;
    Property MultiSelect;
    Property OnClick;
    Property PopupMenu;

  End;

{** Register components with Delphi's IDE }
Procedure Register;

Implementation

Uses
  Graphics,
  Messages,
  wegLibUtils;
  
/////

Constructor TwegLibNGEntryViewer.create( AOwner : TComponent );
Begin

  Inherited;

  // Set the default cursors.
  FLinkCursor := crHandPoint;
  FTextCursor := crDefault;

  // Initial state of history.
  bDoingHistory := False;

End;

/////

Destructor TwegLibNGEntryViewer.destroy;
Begin

  // If this isn't design time...
  If Not ( csDesigning In ComponentState ) Then
    // ... ensure that any entry we're looking at is freed.
    With guideCheck() Do
      freeEntry( FEntry );

  // Nil out the history arrays.
  aBackHistory := Nil;
  aForeHistory := Nil;

  Inherited;

End;

/////

Procedure TwegLibNGEntryViewer.notification( AComponent : TComponent; Operation : TOperation );
Begin

  Inherited;

  // Are we running in the IDE?
  If csDesigning In ComponentState Then
    // Is someone removing a component?
    If Operation = opRemove Then
    Begin
      // Yes, work out which one and kill our pointer to it.
      If AComponent = FNortonGuide Then
        FNortonGuide := Nil
      Else If AComponent = FColours Then
        FColours := Nil;
    End
    Else If Operation = opInsert Then
      // Yes, see if we can use it.
      If ( AComponent Is TwegLibNortonGuide ) And ( FNortonGuide = Nil ) Then
        FNortonGuide := TwegLibNortonGuide( AComponent )
      Else If ( AComponent Is TwegLibNGColours ) And ( FColours = Nil ) Then
        FColours := TwegLibNGColours( AComponent );

End;

/////

Function TwegLibNGEntryViewer.guideCheck : TwegLibNortonGuide;
ResourceString
  RSNoGuide = 'Guide viewer "%s" has not had its NortonGuide property set';
Begin

  If FNortonGuide <> Nil Then
    Result := FNortonGuide
  Else
    Raise EwegLibNGEntryViewer.createFmt( RSNoGuide, [ Name ] );

End;

/////

Procedure TwegLibNGEntryViewer.display( lOffset : LongInt; iStartingLine : Integer = 0 );
Begin

  With guideCheck() Do
    // If the link is ok...
    If linkIsOk( lOffset ) Then
    Begin

      // If we're not doing a history thing.
      If Not bDoingHistory Then
      Begin
        // Remember the current entry.
        rememberEntry( aBackHistory );
        // Zero out the fore history.
        aForeHistory := Nil;
      End;

      // Free any entry we're looking at.
      freeEntry( FEntry );

      // Load the new entry.
      FEntry := loadEntry( lOffset );

      // If we can't see some settings...
      If FNortonGuide.Settings = Nil Then
        // ...do colour display by default.
        Style := lbOwnerDrawFixed
      Else
        // ...otherwise consult the settings.
        If FNortonGuide.Settings.UseColour Then
          Style := lbOwnerDrawFixed
        Else
          Style := lbStandard;

      // Refresh our display.
      refreshDisplay();

      // Jump to the starting line.
      jumpToLine( iStartingLine );

      // It's a new entry.
      doNewEntry();

    End;

End;

/////

Procedure TwegLibNGEntryViewer.displayFirstEntry;
Begin

  With guideCheck() Do
    display( FirstEntry );

End;

/////

Procedure TwegLibNGEntryViewer.redisplay;
Var
  iTop : Integer;
Begin

  With guideCheck() Do
  Begin

    // Ok, we're not really doing history, but this ensures that the current
    // item doesn't get added to the history list.
    bDoingHistory := True;

    Try
      // Remember the top index.
      iTop := TopIndex;
      // Redisplay the current item.
      display( FEntry.Offset, ItemIndex );
      // Reset the top index.
      TopIndex := iTop;
    Finally
      // Reset the "doing history" flag.
      bDoingHistory := False;
    End;

  End;
    
End;

/////

Procedure TwegLibNGEntryViewer.refreshDisplay;
Var
  i : Integer;
Begin

  // Inherit any settings from a main settings component (if there is one).
  If FNortonGuide <> Nil Then
  Begin

    // Inherit the font.
    Font := FNortonGuide.Settings.GuideFont;

    // Inherit the single click setting.
    FSingleClickJump := FNortonGuide.Settings.SingleClickJump;

    // Inherit the use colour settings.
    If FNortonGuide.Settings.UseColour Then
    Begin
      Font.Color := FColours[ FColours.NormalText ];
      Color      := FColours[ FColours.NormalBackground ];
    End
    Else
    Begin
      Font.Color := clWindowText;
      Color      := clWindow;
    End;

  End;

  // Fudge! I smell fudge!
  ItemHeight := Abs( Font.Height ) + ( Round( Abs( Font.Height ) * 0.2 ) );
  
  // Start an update.
  Items.beginUpdate();

  Try

    // Clear anything that we're displaying now.
    clear();

    // Add the stripped lines to the list. These are used if the user decides
    // thay they want to display without colour.
    For i := 0 To FEntry.LineCount - 1 Do
      Items.add( FEntry.StrippedLines[ i ] );

    // Add the horizontal scroll bar.
    addHorizontalScrollBar();

  Finally
    // Mark the end of the update.
    Items.endUpdate();
  End;
    
End;

/////

Procedure TwegLibNGEntryViewer.doNewEntry;
Begin

  // If there's a callback...
  If Assigned( FOnNewEntry ) Then
    // ...call it.
    FOnNewEntry( self );

End;

/////

Function TwegLibNGEntryViewer.linkIsOk( lOffset : LongInt ) : Boolean;
Begin

  // See if the offset is valid.
  If FEntry = Nil Then
    // If we've not got an entry yet we assume that the link is valid.
    Result := True
  Else
    Result := FEntry.validOffset( lOffset );

  // If it isn't and if we've got a callback for this...
  If ( Not Result ) And Assigned( FOnInvalidLink ) Then
    // ...call it.
    FOnInvalidLink( self ); 

End;

/////

Procedure TwegLibNGEntryViewer.rememberEntry( Var aHistory : TwegLibNGEntryViewerHistory );
Begin

  // If there's something to remember...
  If FEntry <> Nil Then
  Begin
    // ...remember the current location in the history list.
    SetLength( aHistory, Length( aHistory ) + 1 );
    aHistory[ Length( aHistory ) - 1 ].lOffset := FEntry.Offset;
    aHistory[ Length( aHistory ) - 1 ].iLine   := ItemIndex;
  End;

End;

/////

Procedure TwegLibNGEntryViewer.historyMove( Var aThis : TwegLibNGEntryViewerHistory; Var aOther : TwegLibNGEntryViewerHistory );
Begin

  // Remember the current entry in the other history list.
  rememberEntry( aOther );

  // We're doing a history movement.
  bDoingHistory := True;

  Try
    // Jump to the entry at the top of this history.
    With aThis[ Length( aThis ) - 1 ] Do display( lOffset, iLine );
    // Pop the entry off the array.
    SetLength( aThis, Length( aThis ) - 1 );
  Finally
    bDoingHistory := False;
  End;

End;

/////

Procedure TwegLibNGEntryViewer.addHorizontalScrollBar;
Var
  sMax    : String;
  iMax    : Integer;
  i       : Integer;
  iPixels : Integer;
Begin

  // First we find the longest string to be displayed.
  iMax := 0;
  For i := 0 To Items.Count - 1 Do
    If Length( FEntry.StrippedLines[ i ] ) > iMax Then
    Begin
      iMax := Length( FEntry.StrippedLines[ i ] );
      sMax := FEntry.StrippedLines[ i ];
    End;

  // Work out how wide, in pixels, the string is (plus one extra character).
  iPixels := Canvas.textWidth( sMax + 'W' );

  // Add the scroll bar.
  SendMessage( Handle, LB_SETHORIZONTALEXTENT, iPixels, 0 );
  
End;

/////

Procedure TwegLibNGentryViewer.click;
Begin

  Inherited;
  
  // TODO: Don't do this in click, possibly do it as a mouseUp event instead.
  // Currently moving in the list with the cursor keys will cause a link to
  // be followed when single clicking is turned on. We don't want this to
  // happen.

  If FSingleClickJump Then
    navigateDown();
    
End;

/////

Procedure TwegLibNGEntryViewer.dblClick;
Begin
  navigateDown();
End;

/////

Procedure TwegLibNGEntryViewer.mouseMove( ssShift : TShiftState; iX : Integer; iY : Integer );
Var
  rPoint : TPoint;
  iLine  : Integer;
Begin

  // If we're enabled.
  If Enabled Then
  Begin

    // Populate a point record for the mouse location.
    rPoint.X := iX;
    rPoint.Y := iY;

    // Figure out what line in the list the cursor is over.
    iLine := itemAtPos( rPoint, True );

    // If there is no line there...
    If iLine = -1 Then
      // Use a normal cursor.
      Cursor := crDefault
    // Otherwise, if it's a short entry and the entry points somewhere...
    Else If FEntry.IsShort And FEntry.validOffset( FEntry.Offsets[ iLine ] ) Then
      // ...use the link cursor.
      Cursor := FLinkCursor
    Else
      // Otherwise use the text cursor.
      Cursor := FTextCursor;
        
  End
  Else
    // Use the default cursor when we're disabled.
    Cursor := crDefault;

End;

/////

Procedure TwegLibNGEntryViewer.drawItem( iIndex : Integer; rRect : TRect; State: TOwnerDrawState );
Const
  CTRL_CHAR = '^';
Type
  TCtrlMode = ( cmNormal, cmBold, cmUnderline, cmReverse );
Var
  cmMode  : TCtrlMode;
  sRaw    : String;
  iLeft   : Integer;
  iCtrl   : Integer;
  iColour : Integer;
  sText   : String;
Begin

  // Starting points.
  cmMode := cmNormal;
  iLeft  := rRect.Left;

  // If the line is selected...
  If odSelected In State Then
  Begin

    // If the current line is focused...
    If odFocused In State Then
    Begin
      Canvas.Font.Color  := FColours[ FColours.FocusedText ];
      Canvas.Brush.Color := FColours[ FColours.FocusedBackground ];
    End
    // Nope, it's selected but not focused.
    Else
    Begin
      Canvas.Font.Color  := FColours[ FColours.SelectedText ];
      Canvas.Brush.Color := FColours[ FColours.SelectedBackground ];
    End;

    // Draw the line.
    Canvas.fillRect( rRect );
    Canvas.textOut( iLeft, rRect.Top, Items[ iIndex ] );

  End
  Else
  // It isn't a selected line so it's angry fruit salad time...
  Begin

    // Start out with normal colours.
    Canvas.Brush.Color := FColours[ FColours.NormalBackground ];
    Canvas.Font.Color  := FColours[ FColours.NormalText ];
    Canvas.fillRect( rRect );

    // Get the text to display.
    sRaw := FEntry[ iIndex ];

    // Find the first control sequence.
    iCtrl := Pos( CTRL_CHAR, sRaw );

    // Loop over the text, dawing it as we go.
    While ( iCtrl <> 0 ) And ( iCtrl < Length( sRaw ) ) Do
    Begin

      // Draw the text up to the next control sequence.
      sText := Copy( sRaw, 1, iCtrl - 1 );
      Canvas.textOut( iLeft, rRect.Top, sText );
      Inc( iLeft, Canvas.textWidth( sText ) ); 

      // Deal with the control character.
      Case sRaw[ iCtrl + 1 ] Of

        // Colour attribute.
        'A', 'a' :
        Begin

          // Work out the colour ID.
          iColour := wegLibHex2Int( Copy( sRaw, iCtrl + 2, 2 ) );

          // Convert the ID into a Windows colour.
          Canvas.Font.Color  := FColours[ iColour And $F ];
          Canvas.Brush.Color := FColours[ ( iColour And $F0 ) Shr 4 ];

          // Go to normal mode.
          cmMode := cmNormal;

          // Skip along the string.
          Inc( iCtrl, 4 );

        End;

        // Bold mode toggle.
        'B', 'b' :
        Begin

          // If we're already bold...
          If cmMode = cmBold Then
          Begin
            // Turn it off
            Canvas.Font.Color  := FColours[ FColours.NormalText ];
            Canvas.Brush.Color := FColours[ FColours.NormalBackground ];
            cmMode             := cmNormal;
          End
          Else
          Begin
            // Turn it on.
            Canvas.Font.Color  := FColours[ FColours.BoldText ];
            Canvas.Brush.Color := FColours[ FColours.BoldBackground ];
            cmMode             := cmBold;
          End;

          // Skip along the string.
          Inc( iCtrl, 2 );

        End;
        
        // Character.
        'C', 'c' :
        Begin

          // Get the character.
          If FNortonGuide.OEMToANSI Then
            sText := wegLibOEMToANSI( wegLibHex2Char( Copy( sRaw, iCtrl + 2, 2 ) ) )
          Else
            sText := wegLibHex2Char( Copy( sRaw, iCtrl + 2, 2 ) );

          // Draw the character.
          Canvas.textOut( iLeft, rRect.Top, sText );
          Inc( iLeft, Canvas.textWidth( sText ) );

          // Skip along the string.
          Inc( iCtrl, 4 );

        End;

        // Normal mode.
        'N', 'n' :
        Begin
          Canvas.Font.Color  := FColours[ FColours.NormalText ];
          Canvas.Brush.Color := FColours[ FColours.NormalBackground ];
          cmMode             := cmNormal;
          Inc( iCtrl, 2 );
        End;

        // Reverse mode toggle.
        'R', 'r' :
        Begin

          // If we're already reversed...
          If cmMode = cmReverse Then
          Begin
            // Turn it off
            Canvas.Font.Color  := FColours[ FColours.NormalText ];
            Canvas.Brush.Color := FColours[ FColours.NormalBackground ];
            cmMode             := cmNormal;
          End
          Else
          Begin
            // Turn it on.
            Canvas.Font.Color  := FColours[ FColours.ReverseText ];
            Canvas.Brush.Color := FColours[ FColours.ReverseBackground ];
            cmMode             := cmReverse;
          End;

          // Skip along the string.
          Inc( iCtrl, 2 );

        End;

        // Underline mode toggle.
        'U', 'u' :
        Begin

          // If we're already underlined...
          If cmMode = cmUnderline Then
          Begin
            // Turn it off
            Canvas.Font.Color  := FColours[ FColours.NormalText ];
            Canvas.Brush.Color := FColours[ FColours.NormalBackground ];
            cmMode             := cmNormal;
          End
          Else
          Begin
            // Turn it on.
            Canvas.Font.Color  := FColours[ FColours.UnderlinedText ];
            Canvas.Brush.Color := FColours[ FColours.UnderlinedBackground ];
            cmMode             := cmUnderline;
          End;

          // Skip along the string.
          Inc( iCtrl, 2 );

        End;

        // The control character.
        CTRL_CHAR :
        Begin
          // We simply draw it.
          Canvas.textOut( iLeft, rRect.Top, CTRL_CHAR );
          Inc( iLeft, Canvas.textWidth( CTRL_CHAR ) );
          Inc( iCtrl, 2 );
        End;

        // None of the above, just move along.
        Else
          Inc( iCtrl );
          
      End;

      // Chop the bits we've done off the raw string.
      sRaw := Copy( sRaw, iCtrl, Length( sRaw ) );
      // Find the next control character.
      iCtrl := Pos( CTRL_CHAR, sRaw );
      
    End;

    // Draw anything that's left.
    Canvas.textOut( iLeft, rRect.Top, sRaw );

  End;

End;

/////

Function TwegLibNGEntryViewer.canNavigateUp : Boolean;
Begin
  Result := FEntry.HasParent;
End;

/////

Function TwegLibNGEntryViewer.canNavigateDown : Boolean;
Begin
  Result := ( ItemIndex > -1 ) And FEntry.HasChild[ ItemIndex ];
End;

/////

Function TwegLibNGEntryViewer.canNavigatePrevious : Boolean;
Begin
  Result := FEntry.HasPrevious;
End;

/////

Function TwegLibNGEntryViewer.canNavigateNext : Boolean;
Begin
  Result := FEntry.HasNext;
End;

/////

Procedure TwegLibNGEntryViewer.navigateUp;
Begin
  If canNavigateUp() Then display( FEntry.Parent, FEntry.ParentLine );
End;

/////

Procedure TwegLibNGEntryViewer.navigateDown;
Begin
  If canNavigateDown() Then display( FEntry.Offsets[ ItemIndex ] );
End;

/////


Procedure TwegLibNGEntryViewer.navigatePrevious;
Begin
  If canNavigatePrevious() Then display( FEntry.Previous );
End;

/////

Procedure TwegLibNGEntryViewer.navigateNext;
Begin
  If canNavigateNext() Then display( FEntry.Next );
End;

/////

Function TwegLibNGEntryViewer.canHistoryNext : Boolean;
Begin
  Result := Length( aForeHistory ) > 0;
End;

/////

Function TwegLibNGEntryViewer.canHistoryPrevious : Boolean;
Begin
  Result := Length( aBackHistory ) > 0;
End;

/////

Procedure TwegLibNGEntryViewer.historyNext;
Begin
  If canHistoryNext() Then historyMove( aForeHistory, aBackHistory );
End;

/////

Procedure TwegLibNGEntryViewer.historyPrevious;
Begin
  If canHistoryPrevious() Then historyMove( aBackHistory, aForeHistory );
End;

/////

Procedure TwegLibNGEntryViewer.getText( sl : TStringList );
Var
  i : Integer;
Begin

  // If a range is selected...
  If SelCount > 1 Then
  Begin
    // Only populate the list with selected lines
    For i := 0 To Items.Count - 1 Do
      If Selected[ i ] Then
        sl.add( FEntry.StrippedLines[ i ] );
  End
  Else
    // No range selected, use all lines.
    FEntry.getLines( sl );

End;

/////

Procedure TwegLibNGEntryViewer.getSource( sl : TStringList );
Var
  i : Integer;
Begin

  // If a range is selected...
  If SelCount > 1 Then
  Begin
    // Only populate the list with selected lines
    For i := 0 To Items.Count - 1 Do
      If Selected[ i ] Then
        sl.add( FEntry.Lines[ i ] );
  End
  Else
    // No range selected, use all lines.
    FEntry.getSourceLines( sl );

End;

/////

Procedure TwegLibNGEntryViewer.jumpToLine( iLine : Integer );
Var
  i : Integer;
Begin

  // Set the item index.
  ItemIndex := iLine;

  // If this is a multi-select list box...
  If MultiSelect Then
    // Unselect all lines apart from the one we're after.
    For i := 0 To Items.Count - 1 Do
      If Selected[ i ] Or ( i = iLine ) Then
        Selected[ i ] := ( i = iLine );
  
End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNGEntryViewer ] );
End;

End.
