{* File.......: wegLibNGLineParser.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2004
 * ID.........: $Id$
 * Description: Classes for parsing a line in a Norton Guide.
 * Licence....: GNU General Public Licence (see below)
 *
 * WEGLib - Norton Guide Reader Library for Delphi.
 * Copyright (C) 2004 Dave Pearson <davep@davep.org>
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

Unit wegLibNGLineParser;

Interface

Uses
  Graphics,
  Windows,
  wegLibNGColours;

Type

  {** Base line parser class }
  TwegLibNGLineParser = Class

  Public

    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; bOEMToANSI : Boolean ); Virtual;

  Protected

    {** Handle some text }
    Procedure handleText( Const sText : String ); Virtual;
    {** Handle a colour attribute }
    Procedure handleColour( iColour : Integer ); Virtual;
    {** Handle a normal text request }
    Procedure handleNormal; Virtual;
    {** Handle a bold text request }
    Procedure handleBold; Virtual;
    {** Handle a reverse text request }
    Procedure handleReverse; Virtual;
    {** Handle an underlined text request }
    Procedure handleUnderline; Virtual;
    {** Handle a character }
    Procedure handleChar( c : Char ); Virtual;

  End;

  {** Line parser for stripping our control characters and returning text }
  TwegLibNGLineStripper = Class( TwegLibNGLineParser )

  Protected

    {** Holds the stripped text }
    FStripped : String;

    {** Handle some text }
    Procedure handleText( Const sText : String ); Override;

  Public

    {** Read-only access to the result }
    Property sStripped : String Read FStripped;

    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; bOEMToANSI : Boolean ); Override;

  End;

  {** Line parser for painting a line on a canvas }
  TwegLibNGLinePainter = Class( TwegLibNGLineParser )

  Public

    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; oCanvas : TCanvas; iTop : Integer; iLeft : Integer; bOEMToANSI : Boolean ); Reintroduce; Overload;
    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; oCanvas : TCanvas; rRect : TRect; bOEMToANSI : Boolean ); Reintroduce; Overload;

  Protected

    {** Pointer to the canvas that we're supposed to paint on }
    oCanvas : TCanvas;
    {** Keep tabs of the top of where we're drawing }
    iTop : Integer;
    {** Keeps tabs of the left of where we're drawing }
    iLeft : Integer;

    {** Handle some text }
    Procedure handleText( Const sText : String ); Override;
    {** Handle a normal text request }
    Procedure handleNormal; Override;
    {** Handle a bold text request }
    Procedure handleBold; Override;
    {** Handle a reverse text request }
    Procedure handleReverse; Override;
    {** Handle an underlined text request }
    Procedure handleUnderline; Override;

  End;

  {** Line parser for paiting a line on a canvas -- in colour }
  TwegLibNGLineColourPainter = Class( TwegLibNGLinePainter )

  Public

    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; oCanvas : TCanvas; iTop : Integer; iLeft : Integer; oColours : TwegLibNGColours; bOEMToANSI : Boolean ); Reintroduce; Overload;
    {** Parse a line from a Norton Guide }
    Procedure parse( sRaw : String; oCanvas : TCanvas; rRect : TRect; oColours : TwegLibNGColours; bOEMToANSI : Boolean ); Reintroduce; Overload;

  Protected

    {** Pointer to the NG colour configuration }
    oColours : TwegLibNGColours;

    {** Handle a colour attribute }
    Procedure handleColour( iColour : Integer ); Override;
    {** Handle a normal text request }
    Procedure handleNormal; Override;
    {** Handle a bold text request }
    Procedure handleBold; Override;
    {** Handle a reverse text request }
    Procedure handleReverse; Override;
    {** Handle an underlined text request }
    Procedure handleUnderline; Override;
    
  End;

Implementation

Uses
  wegLibUtils;
  
/////

Procedure TwegLibNGLineParser.parse( sRaw : String; bOEMToANSI : Boolean );

  Function WithOEMToANSI( Const s : String ) : String;
  Begin
    If bOEMToANSI Then Result := wegLibOEMToANSI( s ) Else Result := s;
  End;

Const
  CTRL_CHAR = '^';
Type
  TCtrlMode = ( cmNormal, cmBold, cmUnderline, cmReverse, cmColourAttr );
Var
  cmMode    : TCtrlMode;
  iCtrl     : Integer;
  iAttr     : Integer;
  iLastAttr : Integer;
Begin

  // Initial mode is normal.
  cmMode    := cmNormal;
  iLastAttr := -1;
  handleNormal();

  // Find the first control sequence.
  iCtrl := Pos( CTRL_CHAR, sRaw );

  // Loop over the text, dawing it as we go.
  While ( iCtrl <> 0 ) And ( iCtrl < Length( sRaw ) ) Do
  Begin

    // Handle the text leading up to the control character.
    handleText( WithOEMToANSI( Copy( sRaw, 1, iCtrl - 1 ) ) );

    // Deal with the control character.
    Case sRaw[ iCtrl + 1 ] Of

      // Colour attribute.
      'A', 'a' :
      Begin

        // Get the colour attribute.
        iAttr := wegLibHex2Int( Copy( sRaw, iCtrl + 2, 2 ) );
        
        // If there's already a colour attribute in effect and the new colour
        // is the same as the previous colour...
        If ( cmMode = cmColourAttr ) And ( iAttr = iLastAttr ) Then
        Begin
          // ...it signals that we return to "normal".
          handleNormal();
          cmMode := cmNormal;
        End
        Else
        Begin
          // ...otherwise we start a colour attribute.
          handleColour( iAttr );
          iLastAttr := iAttr;
          cmMode    := cmColourAttr;
        End;

        // Skip along the string.
        Inc( iCtrl, 4 );
        
      End;

      // Bold mode toggle.
      'B', 'b' :
      Begin

        // If we're already bold...
        If cmMode = cmBold Then
        Begin
          // Go back to normal.
          handleNormal();
          cmMode := cmNormal;
        End
        Else
        Begin
          // Go bold.
          handleBold();
          cmMode := cmBold;
        End;

        // Skip along the string.
        Inc( iCtrl, 2 );

      End;
        
      // Character.
      'C', 'c' :
      Begin

        // Handle the character.
        handleChar( WithOEMToANSI( wegLibHex2Char( Copy( sRaw, iCtrl + 2, 2 ) ) )[ 1 ] );

        // Skip along the string.
        Inc( iCtrl, 4 );
        
      End;

      // Normal mode.
      'N', 'n' :
      Begin

        // Go normal.
        handleNormal();
        cmMode := cmNormal;

        // Skip along the string.
        Inc( iCtrl, 2 );
        
      End;

      // Reverse mode toggle.
      'R', 'r' :
      Begin

        // If we're already reversed...
        If cmMode = cmReverse Then
        Begin
          // Go back to normal.
          handleNormal();
          cmMode := cmNormal;
        End
        Else
        Begin
          // Go reverse.
          handleReverse();
          cmMode := cmReverse;
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
          // Go back to normal.
          handleNormal();
          cmMode := cmNormal;
        End
        Else
        Begin
          // Go underlined.
          handleUnderline();
          cmMode := cmUnderline;
        End;

        // Skip along the string.
        Inc( iCtrl, 2 );

      End;

      // The control character.
      CTRL_CHAR :
      Begin

        // Handle it as text.
        handleText( CTRL_CHAR );

        // Skip along the string.
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

  // Handle any remaining text.
  handleText( WithOEMToANSI( sRaw ) );
  
End;

/////

Procedure TwegLibNGLineParser.handleText( Const sText : String );
Begin
End;

/////

Procedure TwegLibNGLineParser.handleColour( iColour : Integer );
Begin
End;

/////

Procedure TwegLibNGLineParser.handleNormal;
Begin
End;

/////

Procedure TwegLibNGLineParser.handleBold;
Begin
End;

/////

Procedure TwegLibNGLineParser.handleReverse;
Begin
End;

/////

Procedure TwegLibNGLineParser.handleUnderline;
Begin
End;

/////

Procedure TwegLibNGLineParser.handleChar( c : Char );
Begin
  handleText( c );
End;

/////

Procedure TwegLibNGLineStripper.parse( sRaw : String; bOEMToANSI : Boolean );
Begin

  // Reset the result.
  FStripped := '';

  // Call the main parser.
  Inherited;

End;

/////

Procedure TwegLibNGLineStripper.handleText( Const sText : String );
Begin
  FStripped := FStripped + sText;
End;

/////

Procedure TwegLibNGLinePainter.parse( sRaw : String; oCanvas : TCanvas; iTop : Integer; iLeft : Integer; bOEMToANSI : Boolean );
Begin

  // Remember the canvas.
  self.oCanvas := oCanvas;
  // Remember the top.
  self.iTop := iTop;
  // Remember the left.
  self.iLeft := iLeft;

  // Call the main parser.
  Inherited parse( sRaw, bOEMToANSI );

End;

/////

Procedure TwegLibNGLinePainter.parse( sRaw : String; oCanvas : TCanvas; rRect : TRect; bOEMToANSI : Boolean );
Begin

  // Remember the canvas.
  self.oCanvas := oCanvas;
  // Remember the top.
  self.iTop := rRect.Top;
  // Remember the left.
  self.iLeft := rRect.Left;

  // Do the default painting for the rectangle.
  handleNormal();
  oCanvas.fillRect( rRect );

  // Call the main parser.
  Inherited parse( sRaw, bOEMToANSI );

End;

/////

Procedure TwegLibNGLinePainter.handleText( Const sText : String );
Begin
  oCanvas.textOut( iLeft, iTop, sText );
  Inc( iLeft, oCanvas.textWidth( sText ) );
End;

/////

Procedure TwegLibNGLinePainter.handleNormal;
Begin
  oCanvas.Brush.Color := clWhite;
  oCanvas.Font.Color  := clBlack;
  oCanvas.Font.Style  := [];
End;

/////

Procedure TwegLibNGLinePainter.handleBold;
Begin
  oCanvas.Font.Style := [ fsBold ];
End;

/////

Procedure TwegLibNGLinePainter.handleReverse;
Begin
  oCanvas.Brush.Color := clBlack;
  oCanvas.Font.Color  := clWhite;
End;

/////

Procedure TwegLibNGLinePainter.handleUnderline;
Begin
  oCanvas.Font.Style := [ fsUnderline ];
End;

/////

Procedure TwegLibNGLineColourPainter.parse( sRaw : String; oCanvas : TCanvas; iTop : Integer; iLeft : Integer; oColours : TwegLibNGColours; bOEMToANSI : Boolean );
Begin

  // Remember the colour configuration.
  self.oColours := oColours;
  
  // Call the main parser.
  Inherited parse( sRaw, oCanvas, iTop, iLeft, bOEMToANSI );

End;

/////

Procedure TwegLibNGLineColourPainter.parse( sRaw : String; oCanvas : TCanvas; rRect : TRect; oColours : TwegLibNGColours; bOEMToANSI : Boolean );
Begin

  // Remember the colour configuration.
  self.oColours := oColours;

  // Call the main parser.
  Inherited parse( sRaw, oCanvas, rRect, bOEMToANSI );

End;

/////

Procedure TwegLibNGLineColourPainter.handleColour( iColour : Integer );
Begin
  oCanvas.Font.Color  := oColours[ iColour And $F ];
  oCanvas.Brush.Color := oColours[ ( iColour And $F0 ) Shr 4 ];
End;

/////

Procedure TwegLibNGLineColourPainter.handleNormal;
Begin
  oCanvas.Font.Color  := oColours[ oColours.NormalText ];
  oCanvas.Brush.Color := oColours[ oColours.NormalBackground ];
End;

/////

Procedure TwegLibNGLineColourPainter.handleBold;
Begin
  oCanvas.Font.Color  := oColours[ oColours.BoldText ];
  oCanvas.Brush.Color := oColours[ oColours.BoldBackground ];
End;

/////

Procedure TwegLibNGLineColourPainter.handleReverse;
Begin
  oCanvas.Font.Color  := oColours[ oColours.ReverseText ];
  oCanvas.Brush.Color := oColours[ oColours.ReverseBackground ];
End;

/////

Procedure TwegLibNGLineColourPainter.handleUnderline;
Begin
  oCanvas.Font.Color  := oColours[ oColours.UnderlinedText ];
  oCanvas.Brush.Color := oColours[ oColours.UnderlinedBackground ];
End;

End.
