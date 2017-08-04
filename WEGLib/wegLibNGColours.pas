{* File.......: wegLibNGColours.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Component for holding the guide display colour settings.
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

{ @abstract(Defines a component for holding the guide display colour settings.)
  @author(Dave Pearson <davep@davep.org>)
  @lastmod($Id$)
  The @name unit provides a component for holding the guide display colour
  settings. }
Unit wegLibNGColours;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  Graphics,
  Registry;

Type

  { Type of a colour ID. }
  TwegLibNGColourID = 0..15;

  { @abstract(Component for holding the guide colour settings.)
    @name is a component that holds the guide display colour settings. It
    also provides a user interface for letting a user modify the colour
    settings. }
  TwegLibNGColours = Class( TComponent )

  Protected

    { Colour for normal text. }
    FNormalText : TwegLibNGColourID;
    { Colour for normal background. }
    FNormalBackground : TwegLibNGColourID;
    { Colour for bold text. }
    FBoldText : TwegLibNGColourID;
    { Colour for bold background. }
    FBoldBackground : TwegLibNGColourID;
    { Colour for reverse text. }
    FReverseText : TwegLibNGColourID;
    { Colour for reverse background. }
    FReverseBackground : TwegLibNGColourID;
    { Colour for underlined text. }
    FUnderlinedText : TwegLibNGColourID;
    { Colour for underlined background. }
    FUnderlinedBackground : TwegLibNGColourID;
    { Colour for selected text. }
    FSelectedText : TwegLibNGColourID;
    { Colour for selected background. }
    FSelectedBackground : TwegLibNGColourID;
    { Colour for focused text. }
    FFocusedText : TwegLibNGColourID;
    { Colour for focused background. }
    FFocusedBackground : TwegLibNGColourID;

    { The array of DOS colours. }
    aDOSMap : Array[ 0..15 ] Of TColor;

    { Check if the colour ID is ok, throw an exception if it isn't. }
    Function colourIDOk( i : Integer ) : Boolean; Virtual;
    { Set a colour in the DOS map. }
    Procedure setDOSColour( i : Integer; col : TColor ); Virtual;
    { Get a colour in the DOS map. }
    Function getDOSColour( i : Integer ) : TColor; Virtual;

  Public

    { @abstract(Access to the DOS map.)
      Access is a wrapper around @link(getDOSColour).
      Assignment is a wrapper around @link(setDOSColour). }
    Property DOSMap[ i : Integer ] : TColor Read getDOSColour Write setDOSColour; Default;

    { Constructor. }
    Constructor create( AOwner : TComponent ); Override;

    { @abstract(Edit the settings.)
      @code(sCaption) is an optinal caption to give the edit dialog
      (@link(TwegLibNGColoursDialog)). }
    Function edit( Const sCaption : String = '' ) : Boolean; Virtual;
    { Set all colour information back to their default values. }
    Procedure defaultColours; Virtual;
    { Clone colours from another instance of this class. }
    Procedure cloneColours( oColours : TwegLibNGColours ); Virtual;
    { Save the settings using the passed registry object. }
    Procedure saveToRegistry( Const sKeyPrefix : String; oRegistry : TRegistry ); Overload; Virtual;
    { Save the settings to the registry. }
    Procedure saveToRegistry( Const sKeyPrefix : String ); Overload; Virtual;
    { Load the settings using the passed registry object. }
    Procedure loadFromRegistry( Const sKeyPrefix : String; oRegistry : TRegistry ); Overload; Virtual;
    { Load the settings from the registry. }
    Procedure loadFromRegistry( Const sKeyPrefix : String ); Overload; Virtual;

  Published

    { @abstract(Colour for normal text.)
      This is a wrapper around @link(FNormalText). }
    Property NormalText : TwegLibNGColourID Read FNormalText Write FNormalText;
    { @abstract(Colour for normal background.)
      This is a wrapper around @link(FNormalBackground). }
    Property NormalBackground : TwegLibNGColourID Read FNormalBackground Write FNormalBackground;
    { @abstract(Colour for bold text.)
      This is a wrapper around @link(FBoldText). }
    Property BoldText : TwegLibNGColourID Read FBoldText Write FBoldText;
    { @abstract(Colour for bold background.)
      This is a wrapper around @link(FBoldBackground). }
    Property BoldBackground : TwegLibNGColourID Read FBoldBackground Write FBoldBackground;
    { @abstract(Colour for reverse text.)
      This is a wrapper around @link(FReverseText). }
    Property ReverseText : TwegLibNGColourID Read FReverseText Write FReverseText;
    { @abstract(Colour for reverse background.)
      This is a wrapper around @link(FReverseBackground). }
    Property ReverseBackground : TwegLibNGColourID Read FReverseBackground Write FReverseBackground;
    { @abstract(Colour for underlined text.)
      This is a wrapper around @link(FUnderlinedText). }
    Property UnderlinedText : TwegLibNGColourID Read FUnderlinedText Write FUnderlinedText;
    { @abstract(Colour for underlined background.)
      This is a wrapper around @link(FUnderlinedBackground). }
    Property UnderlinedBackground : TwegLibNGColourID Read FUnderlinedBackground Write FUnderlinedBackground;
    { @abstract(Colour for selected text.)
      This is a wrapper around @link(FSelectedText). }
    Property SelectedText : TwegLibNGColourID Read FSelectedText Write FSelectedText;
    { @abstract(Colour for selected background.)
      This is a wrapper around @link(FSelectedBackground). }
    Property SelectedBackground : TwegLibNGColourID Read FSelectedBackground Write FSelectedBackground;
    { @abstract(Colour for focused text.)
      This is a wrapper around @link(FFocusedText). }
    Property FocusedText : TwegLibNGColourID Read FFocusedText Write FFocusedText;
    { @abstract(Colour for focused background.)
      This is a wrapper around @link(FFocusedBackground). }
    Property FocusedBackground : TwegLibNGColourID Read FFocusedBackground Write FFocusedBackground;
    { Colour to use for black. }
    Property Black : TColor Index 0 Read getDOSColour Write setDOSColour;
    { Colour to use for brown. }
    Property Blue : TColor Index 1 Read getDOSColour Write setDOSColour;
    { Colour to use for green. }
    Property Green : TColor Index 2 Read getDOSColour Write setDOSColour;
    { Colour to use for cyan. }
    Property Cyan : TColor Index 3 Read getDOSColour Write setDOSColour;
    { Colour to use for red. }
    Property Red : TColor Index 4 Read getDOSColour Write setDOSColour;
    { Colour to use for violet. }
    Property Violet : TColor Index 5 Read getDOSColour Write setDOSColour;
    { Colour to use for brown. }
    Property Brown : TColor Index 6 Read getDOSColour Write setDOSColour;
    { Colour to use for white. }
    Property White : TColor Index 7 Read getDOSColour Write setDOSColour;
    { Colour to use for bright black. }
    Property BrightBlack : TColor Index 8 Read getDOSColour Write setDOSColour;
    { Colour to use for bright blue. }
    Property BrightBlue : TColor Index 9 Read getDOSColour Write setDOSColour;
    { Colour to use for bright green. }
    Property BrightGreen : TColor Index 10 Read getDOSColour Write setDOSColour;
    { Colour to use for bright cyan. }
    Property BrightCyan : TColor Index 11 Read getDOSColour Write setDOSColour;
    { Colour to use for bright red. }
    Property BrightRed : TColor Index 12 Read getDOSColour Write setDOSColour;
    { Colour to use for bright violet. }
    Property BrightViolet : TColor Index 13 Read getDOSColour Write setDOSColour;
    { Colour to use for bright brown. }
    Property BrightBrown : TColor Index 14 Read getDOSColour Write setDOSColour;
    { Colour to use fo bright white. }
    Property BrightWhite : TColor Index 15 Read getDOSColour Write setDOSColour;

  End;

{ Register components with Delphi's IDE. }
Procedure Register;

Implementation

Uses
  SysUtils,
  wegLibNGColoursDialogUnit;

Const
  // Registry key suffix for the dos map.
  REG_KEY_DOSMAP = 'DOSMap';
  // Registry key suffix for the guide colours.
  REG_KEY_GUIDE  = 'Guide';
  // Registry value names for each of the guide colours.
  REG_NORMAL_TEXT           = 'Normal Text';
  REG_NORMAL_BACKGROUND     = 'Normal Background';
  REG_BOLD_TEXT             = 'Bold Text';
  REG_BOLD_BACKGROUND       = 'Bold Background';
  REG_REVERSE_TEXT          = 'Reverse Text';
  REG_REVERSE_BACKGROUND    = 'Reverse Background';
  REG_UNDERLINED_TEXT       = 'Underlined Text';
  REG_UNDERLINED_BACKGROUND = 'Underlined Background';
  REG_SELECTED_TEXT         = 'Selected Text';
  REG_SELECTED_BACKGROUND   = 'Selected Background';
  REG_FOCUSED_TEXT          = 'Focused Text';
  REG_FOCUSED_BACKGROUND    = 'Focused Background';

/////

Constructor TwegLibNGColours.create( AOWner : TComponent );
Begin

  Inherited;

  // Default the colours.
  defaultColours();

End;

/////

Function TwegLibNGColours.colourIDOk( i : Integer ) : Boolean;
ResourceString
  RSBoundsError = 'Colour ID %d is out of bounds (must be %d..%d)';
Begin

  // Within bounds?
  Result := ( i >= Low( aDOSMap ) ) And ( i <= High( aDOSMap ) );

  // If not
  If Not Result Then
    // Throw an exception.
    Raise Exception.createFmt( RSBoundsError, [ i, Low( aDOSMap ), High( aDOSMap ) ] );

End;

/////

Procedure TwegLibNGColours.setDOSColour( i : Integer; col : TColor );
Begin

  If colourIDOk( i ) Then
    aDOSMap[ i ] := col;

End;

/////

Function TwegLibNGColours.getDOSColour( i : Integer ) : TColor;
Begin

  If colourIDOk( i ) Then
    Result := aDOSMap[ i ]
  Else
    // This shouldn't happen, but it keeps the compiler quiet.
    Result := clBlack;

End;

/////

Function TwegLibNGColours.edit( Const sCaption : String ) : Boolean;
Var
  oWorkColours : TwegLibNGColours;
Begin

  With TwegLibNGColoursDialog.create( Owner ) Do
    Try

      // If we were given a caption for the dialog...
      If sCaption <> '' Then
        // ...assign it to the caption of the dialog.
        Caption := sCaption;

      // We work on a temporary instance of our class so that, if the user
      // wants to default the colours, things don't get upset if they then
      // do a cancel after the default.
      oWorkColours := TwegLibNGColours.create( Nil );
      oWorkColours.cloneColours( self );

      Try

        // Give the dialog a pointer to the temp colour settings component.
        oColours := oWorkColours;

        // Fire off the dialog.
        Result := ( showModal() = mrOk );

        // If the user appeared to change something...
        If Result Then
          // ...copy the new colour settings.
          cloneColours( oWorkColours );

      Finally
        oWorkColours.free();
      End;

    Finally
      // Free the dialog.
      free();
    End;

End;

/////

Procedure TwegLibNGColours.defaultColours;
Begin

  // Give each of the colour properties a default colour;
  Black        := $000000;
  Blue         := $800000;
  Green        := $00A800;
  Cyan         := $A8A800;
  Red          := $0000A8;
  Violet       := $A800A8;
  Brown        := $404080;
  White        := $C0C0C0;
  BrightBlack  := $545454;
  BrightBlue   := $FF0000;
  BrightGreen  := $00FF54;
  BrightCyan   := $FFFF54;
  BrightRed    := $0000FF;
  BrightViolet := $FF00FF;
  BrightBrown  := $00FFFF;
  BrightWhite  := $FFFFFF;

  // Set the default pointers into the map for the guide colours.
  FNormalText           := 15;
  FNormalBackground     :=  1;
  FBoldText             := 14;
  FBoldBackground       :=  1;
  FReverseText          := 15;
  FReverseBackground    :=  4;
  FUnderlinedText       := 13;
  FUnderlinedBackground :=  1;
  FSelectedText         :=  7;
  FSelectedBackground   :=  4;
  FFocusedText          := 15;
  FFocusedBackground    :=  4;

End;

/////

Procedure TwegLibNGColours.cloneColours( oColours : TwegLibNGColours );
Var
  i : Integer;
Begin

  // Clone the DOS map.
  For i := Low( TwegLibNGColourID ) To High( TwegLibNGColourID ) Do
    aDOSMap[ i ] := oColours.DOSMap[ i ];

  // Clone the guide colours.
  FNormalText           := oColours.NormalText;
  FNormalBackground     := oColours.NormalBackground;
  FBoldText             := oColours.BoldText;
  FBoldBackground       := oColours.BoldBackground;
  FReverseText          := oColours.ReverseText;
  FReverseBackground    := oColours.ReverseBackground;
  FUnderlinedText       := oColours.UnderlinedText;
  FUnderlinedBackground := oColours.UnderlinedBackground;
  FSelectedText         := oColours.SelectedText;
  FSelectedBackground   := oColours.SelectedBackground;
  FFocusedText          := oColours.FocusedText;
  FFocusedBackground    := oColours.FocusedBackground;

End;

/////

Procedure TwegLibNGColours.saveToRegistry( Const sKeyPrefix : String; oRegistry : TRegistry );
Var
  i : Integer;
Begin

  With oRegistry Do
  Begin

    // First, save the DOS colour map.
    If openKey( IncludeTrailingBackslash( sKeyPrefix ) + REG_KEY_DOSMAP, True ) Then
      Try
        // Save each of the values in the colour map.
        For i := Low( aDOSMap ) To High( aDOSMap ) Do
          writeInteger( IntToStr( i ), aDOSMap[ i ] );
      Finally
        closeKey();
      End;

    // Now save the guide colours.
    If openKey( IncludeTrailingBackslash( sKeyPrefix ) + REG_KEY_GUIDE, True ) Then
      Try
        writeInteger( REG_NORMAL_TEXT,           FNormalText );
        writeInteger( REG_NORMAL_BACKGROUND,     FNormalBackground );
        writeInteger( REG_BOLD_TEXT,             FBoldText );
        writeInteger( REG_BOLD_BACKGROUND,       FBoldBackground );
        writeInteger( REG_REVERSE_TEXT,          FReverseText );
        writeInteger( REG_REVERSE_BACKGROUND,    FReverseBackground );
        writeInteger( REG_UNDERLINED_TEXT,       FUnderlinedText );
        writeInteger( REG_UNDERLINED_BACKGROUND, FUnderlinedBackground );
        writeInteger( REG_SELECTED_TEXT,         FSelectedText );
        writeInteger( REG_SELECTED_BACKGROUND,   FSelectedBackground );
        writeInteger( REG_FOCUSED_TEXT,          FFocusedText );
        writeInteger( REG_FOCUSED_BACKGROUND,    FFocusedBackground );
      Finally
        closeKey();
      End;

  End;

End;

/////

Procedure TwegLibNGColours.saveToRegistry( Const sKeyPrefix : String );
Var
  oReg : TRegistry;
Begin

  // Create a registry object.
  oReg := TRegistry.create();

  Try
    // Now pass off to the other registry saving procedure.
    saveToRegistry( sKeyPrefix, oReg );
  Finally
    // Free the registry object.
    oReg.free();
  End;

End;

/////

Procedure TwegLibNGColours.loadFromRegistry( Const sKeyPrefix : String; oRegistry : TRegistry );
Var
  i : Integer;
Begin

  With oRegistry Do
    Try

      // First, load the DOS colour map.
      If openKey( IncludeTrailingBackslash( sKeyPrefix ) + REG_KEY_DOSMAP, False ) Then
        Try
          // Load each of the values in the colour map.
          For i := Low( aDOSMap ) To High( aDOSMap ) Do
            aDOSMap[ i ] := readInteger( IntToStr( i ) );
        Finally
          closeKey();
        End;

      // Now load the guide colours.
      If openKey( IncludeTrailingBackslash( sKeyPrefix ) + REG_KEY_GUIDE, False ) Then
        Try
          FNormalText           := readInteger( REG_NORMAL_TEXT );
          FNormalBackground     := readInteger( REG_NORMAL_BACKGROUND );
          FBoldText             := readInteger( REG_BOLD_TEXT );
          FBoldBackground       := readInteger( REG_BOLD_BACKGROUND );
          FReverseText          := readInteger( REG_REVERSE_TEXT );
          FReverseBackground    := readInteger( REG_REVERSE_BACKGROUND );
          FUnderlinedText       := readInteger( REG_UNDERLINED_TEXT );
          FUnderlinedBackground := readInteger( REG_UNDERLINED_BACKGROUND );
          FSelectedText         := readInteger( REG_SELECTED_TEXT );
          FSelectedBackground   := readInteger( REG_SELECTED_BACKGROUND );
          FFocusedText          := readInteger( REG_FOCUSED_TEXT );
          FFocusedBackground    := readInteger( REG_FOCUSED_BACKGROUND );
        Finally
          closeKey();
        End;

    Except
      // GNDN.
    End;

End;

/////

Procedure TwegLibNGColours.loadFromRegistry( Const sKeyPrefix : String );
Var
  oReg : TRegistry;
Begin

  // Create a registry object.
  oReg := TRegistry.create();

  Try
    // Now pass off to the other registry loading procedure.
    loadFromRegistry( sKeyPrefix, oReg );
  Finally
    // Free the registry object.
    oReg.free();
  End;

End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNGColours ] );
End;

End.
