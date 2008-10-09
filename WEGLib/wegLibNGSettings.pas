{* File.......: wegLibNGSettings.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id: wegLibNGSettings.pas,v 1.1.1.1 2003/07/16 17:12:57 davep Exp $
 * Description: Component for holding and managing wegLib settings.
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

Unit wegLibNGSettings;

{$R *.DCR}

Interface

Uses
  Classes,
  Controls,
  Graphics,
  Registry;

Type

  {** Component for holding and manading webLib settings }
  TwegLibNGSettings = Class( TComponent )

  Protected

    {** Should we do OEM to ANSI conversion? }
    FOEMToANSI : Boolean;
    {** The default guide directory }
    FDefaultGuideDirectory : String;
    {** Use colour? }
    FUseColour : Boolean;
    {** Font to use for the display of guide entries }
    FGuideFont : TFont;
    {** Should a guide entry jump to a sub-item on a single click? }
    FSingleClickJump : Boolean;

    {** Set the guide font }
    Procedure setGuideFont( fnt : TFont ); Virtual;

  Public

    {** Constructor }
    Constructor create( AOwner : TComponent ); Override;

    {** Destructor }
    Destructor destroy; Override;

    {** Edit the settings }
    Function edit( Const sCaption : String = '' ) : Boolean; Virtual;
    {** Save the settings using the passed registry object }
    Procedure saveToRegistry( Const sKey : String; oRegistry : TRegistry ); Overload; Virtual;
    {** Save the settings to the registry }
    Procedure saveToRegistry( Const sKey : String ); Overload; Virtual;
    {** Load the settings using the passed registry object }
    Procedure loadFromRegistry( Const sKey : String; oRegistry : TRegistry ); Overload; Virtual;
    {** Load the settings from the registry }
    Procedure loadFromRegistry( Const sKey : String ); Overload; Virtual;

  Published

    {** Should we do OEM to ANSI conversion? }
    Property OEMToANSI : Boolean Read FOEMToANSI Write FOEMToANSI;
    {** The default guide directory }
    Property DefaultGuideDirectory : String Read FDefaultGuideDirectory Write FDefaultGuideDirectory;
    {** Use colour? }
    Property UseColour : Boolean Read FUseColour Write FUseColour;
    {** Font to use for the display of guide entries }
    Property GuideFont : TFont Read FGuideFont Write setGuideFont;
    {** Should a guide entry jump to a sub-item on a single click? }
    Property SingleClickJump : Boolean Read FSingleClickJump Write FSingleClickJump;

  End;

{** Register components with Delphi's IDE }
Procedure Register;

Implementation

Uses
  wegLibNGSettingsDialogUnit;

Const
  {** Value name for the OEMToANSI setting }
  REG_OEM_TO_ANSI = 'OEM To ANSI';
  {** Value name for the default guide directory }
  REG_DEFAULT_GUIDE_DIR = 'Default Guide Directory';
  {** Value name for the UseColour setting }
  REG_USE_COLOUR = 'Use Colour';
  {** Value for the GuideFont name setting }
  REG_GUIDE_FONT_NAME = 'Guide Font Name';
  {** Value for the GuideFont charset setting }
  REG_GUIDE_FONT_CHARSET = 'Guide Font CharSet';
  {** Value for the GuideFont pitch setting }
  REG_GUIDE_FONT_PITCH = 'Guide Font Pitch';
  {** Value for the GuideFont size setting }
  REG_GUIDE_FONT_SIZE = 'Guide Font Size';
  {** Value for the GuideFont style setting }
  REG_GUIDE_FONT_STYLE = 'Guide Font Style';
  {** Value name for the SingleClickJump setting }
  REG_SINGLE_CLICK  = 'Single Click To Jump';

/////

Constructor TwegLibNGSettings.create( AOwner : TComponent );
Begin

  Inherited;

  // Create the guide font.
  FGuideFont := TFont.create();
  
End;

/////

Destructor TwegLibNGSettings.destroy;
Begin

  // Free the guide font.
  FGuideFont.free();

  Inherited;

End;

/////

Function TwegLibNGSettings.edit( Const sCaption : String ) : Boolean;
Begin

  With TwegLibNGSettingsDialog.create( Owner ) Do
    Try

      // If we were given a caption for the dialog...
      If sCaption <> '' Then
        // ...assign it to the caption of the dialog.
        Caption := sCaption;

      // Populate the dialog.
      edtGuideDirectory.Text := FDefaultGuideDirectory;
      fdGuideFont.Font       := FGuideFont;
      cbUseColour.Checked    := FUseColour;
      cbOEMToANSI.Checked    := FOEMToANSI;
      cbSingleClick.Checked  := FSingleClickJump;

      // Fire off the dialog.
      Result := showModal() = mrOk;

      // If the user hit ok...
      If Result Then
      Begin
        // ...grab the settings back from the dialog.
        FDefaultGuideDirectory := edtGuideDirectory.Text;
        GuideFont              := fdGuideFont.Font;
        FUseColour             := cbUseColour.Checked;
        FOEMToANSI             := cbOEMToANSI.Checked;
        FSingleClickJump       := cbSingleClick.Checked;
      End;
      
    Finally
      // Free the config dialog.
      free();
    End;

End;

/////

Procedure TwegLibNGSettings.saveToRegistry( Const sKey : String; oRegistry : TRegistry );

  Function StyleToString( fs : TFontStyles ) : String;
  Var
    sl : TStringList;
  Begin

    // Create the list that will hold the styles.
    sl := TStringList.create();

    Try

      // Add symbolic names for each style that applies to the font.
      If fsBold      In fs Then sl.add( 'Bold' );
      If fsItalic    In fs Then sl.add( 'Italic' );
      If fsUnderline In fs Then sl.add( 'Underline' );
      If fsStrikeout In fs Then sl.add( 'Strikeout' );

      // Return the list as a string.
      Result := sl.CommaText;

    Finally
      // Free the string list.
      sl.free();
    End;

  End;
  
Begin

  With oRegistry Do
    // Open/create the key.
    If openKey( sKey, True ) Then
      Try
        // Save each of the settings.
        writeString( REG_DEFAULT_GUIDE_DIR,   FDefaultGuideDirectory );
        writeBool(   REG_USE_COLOUR,          FUseColour );
        writeBool(   REG_OEM_TO_ANSI,         FOEMToANSI );
        writeBool(   REG_SINGLE_CLICK,        FSingleClickJump );
        writeString( REG_GUIDE_FONT_NAME,     FGuideFont.Name );
        writeInteger( REG_GUIDE_FONT_CHARSET, FGuideFont.Charset );
        writeInteger( REG_GUIDE_FONT_PITCH,   Integer( FGuideFont.Pitch ) );
        writeInteger( REG_GUIDE_FONT_SIZE,    FGuideFont.Size );
        writeString( REG_GUIDE_FONT_STYLE,    StyleToString( FGuideFont.Style ) );
      Finally
        // Close the key.
        closeKey();
      End;

End;

/////

Procedure TwegLibNGSettings.saveToRegistry( Const sKey : String );
Var
  oReg : TRegistry;
Begin

  // Create a registry object.
  oReg := TRegistry.create();

  Try
    // Now pass off to the other registry saving procedure.
    saveToRegistry( sKey, oReg );
  Finally
    // Free the registry object.
    oReg.free();
  End;

End;

/////

Procedure TwegLibNGSettings.setGuideFont( fnt : TFont );
Begin
  FGuideFont.assign( fnt );
End;

/////

Procedure TwegLibNGSettings.loadFromRegistry( Const sKey : String; oRegistry : TRegistry );

  Function StringToStyle( Const sStyle : String ) : TFontStyles;
  Var
    sl : TStringList;
  Begin

    // Start off with an empty style.
    Result := [];

    // Creatr the string list that'll be used to convert things back.
    sl := TStringList.create();

    Try

      // Load the string into the list.
      sl.CommaText := sStyle;

      // Look for the symbolic names of each of the styles and add them
      // into the style if the exist.
      If sl.indexOf( 'Bold' )      > -1 Then Result := Result + [ fsBold ];
      If sl.indexOf( 'Italic' )    > -1 Then Result := Result + [ fsItalic ];
      If sl.indexOf( 'Underline' ) > -1 Then Result := Result + [ fsUnderline ];
      If sl.indexOf( 'Strikeout' ) > -1 Then Result := Result + [ fsStrikeout ];
      
    Finally
      sl.free();
    End;
        
  End;
  
Begin

  With oRegistry Do
    // Open the key.
    If openKey( sKey, False ) Then
      Try
        Try
          // Load each of the settings.
          FDefaultGuideDirectory := readString( REG_DEFAULT_GUIDE_DIR );
          FUseColour             := readBool( REG_USE_COLOUR );
          FOEMToANSI             := readBool( REG_OEM_TO_ANSI );
          FSingleClickJump       := readBool( REG_SINGLE_CLICK );
          FGuideFont.Name        := readString( REG_GUIDE_FONT_NAME );
          FGuideFont.Charset     := readInteger( REG_GUIDE_FONT_CHARSET );
          FGuideFont.Pitch       := TFontPitch( readInteger( REG_GUIDE_FONT_PITCH ) );
          FGuideFont.Size        := readInteger( REG_GUIDE_FONT_SIZE );
          FGuideFont.Style       := StringToStyle( readString( REG_GUIDE_FONT_STYLE ) );
        Except
          // GNDN.
        End;
      Finally
        // Close the key.
        closeKey();
      End;

End;

/////

Procedure TwegLibNGSettings.loadFromRegistry( Const sKey : String );
Var
  oReg : TRegistry;
Begin

  // Create a registry object.
  oReg := TRegistry.create();

  Try
    // Now pass off to the other registry load procedure.
    loadFromRegistry( sKey, oReg );
  Finally
    // Free the registry object.
    oReg.free();
  End;

End;

/////

Procedure Register;
Begin
  RegisterComponents( 'org.davep.weglib', [ TwegLibNGSettings ] );
End;

End.
