{* File.......: wegLibNGColoursDialogUnit.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Dialog for editing wegLibNGColours values.
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

Unit wegLibNGColoursDialogUnit;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  wegLibNGColours,
  ExtCtrls,
  StdCtrls;

Type

  {** Dialog for editing wegLibNGColours values. }
  TwegLibNGColoursDialog = Class( TForm )
    gbDOSColours: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    pnlNormalBlack: TPanel;
    pnlBrightBlack: TPanel;
    pnlNormalBlue: TPanel;
    pnlBrightBlue: TPanel;
    pnlNormalGreen: TPanel;
    pnlBrightGreen: TPanel;
    pnlNormalCyan: TPanel;
    pnlBrightCyan: TPanel;
    pnlNormalRed: TPanel;
    pnlBrightRed: TPanel;
    pnlNormalViolet: TPanel;
    pnlBrightViolet: TPanel;
    pnlNormalBrown: TPanel;
    pnlBrightBrown: TPanel;
    pnlNormalWhite: TPanel;
    pnlBrightWhite: TPanel;
    lblNormal: TLabel;
    lblBright: TLabel;
    cdDOSMap: TColorDialog;
    lblBlack: TLabel;
    lblBlue: TLabel;
    lblGreen: TLabel;
    lblCyan: TLabel;
    lblRed: TLabel;
    lblViolet: TLabel;
    lblBrown: TLabel;
    lblWhite: TLabel;
    gbNGColours: TGroupBox;
    btnDefaults: TButton;
    cbNormalText: TComboBox;
    lblNormalGuide: TLabel;
    lblText: TLabel;
    lblBackground: TLabel;
    lblBold: TLabel;
    lblUnderline: TLabel;
    lblReverse: TLabel;
    lblSelected: TLabel;
    lblFocused: TLabel;
    cbNormalBackground: TComboBox;
    cbBoldText: TComboBox;
    cbBoldBackground: TComboBox;
    cbReverseText: TComboBox;
    cbReverseBackground: TComboBox;
    cbUnderlineText: TComboBox;
    cbUnderlineBackground: TComboBox;
    cbSelectedText: TComboBox;
    cbSelectedBackground: TComboBox;
    cbFocusedText: TComboBox;
    cbFocusedBackground: TComboBox;
    procedure pnlDOSMapEnter(Sender: TObject);
    procedure pnlDOSMapExit(Sender: TObject);
    procedure pnlDOSMapClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnDefaultsClick(Sender: TObject);
    procedure cbNGColourDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  Public

    {** Pointer to the colours settings that we're working with }
    oColours : TwegLibNGColours;

  Protected

    {** Find the DOS map panel associated with the given colour }
    Function findColourPanel( iID : TwegLibNGColourID ) : TPanel;
    {** Populate the dialog }
    Procedure populateDialog;
    {** Force a re-paint of the guide colours }
    Procedure repaintGuideColours;

  End;

Implementation

{$R *.DFM}

/////

Procedure TwegLibNGColoursDialog.pnlDOSMapEnter( Sender : TObject );
Begin
  TPanel( Sender ).Font.Style := [ fsBold ];
End;

/////

Procedure TwegLibNGColoursDialog.pnlDOSMapExit( Sender : TObject );
Begin
  TPanel( Sender ).Font.Style := [];
End;

/////

Procedure TwegLibNGColoursDialog.pnlDOSMapClick( Sender : TObject );
Begin

  With TPanel( Sender ) Do
  Begin

    // Pass the colour of the panel to the colour dialog.
    cdDOSMap.Color := Color;

    // Execute the dialog.
    If cdDOSMap.execute() Then
    Begin

      // If the user selected a colour, give it back to the panel.
      Color := cdDOSMap.Color;

      // Update the DOS map too.
      oColours[ Tag ] := cdDOSMap.Color;

      // This colour change might affect the display of the guide colour
      // combo boxes. Get them to refresh.
      repaintGuideColours();

    End;

  End;

End;

/////

Procedure TwegLibNGColoursDialog.FormShow( Sender : TObject );
Begin

  // Only the normal text list is populated; no point in repeating the text
  // so many times. Copy the colour text to the other lists.
  cbNormalBackground.Items    := cbNormalText.Items;
  cbBoldText.Items            := cbNormalText.Items;
  cbBoldBackground.Items      := cbNormalText.Items;
  cbReverseText.Items         := cbNormalText.Items;
  cbReverseBackground.Items   := cbNormalText.Items;
  cbUnderlineText.Items       := cbNormalText.Items;
  cbUnderlineBackground.Items := cbNormalText.Items;
  cbSelectedText.Items        := cbNormalText.Items;
  cbSelectedBackground.Items  := cbNormalText.Items;
  cbFocusedText.Items         := cbNormalText.Items;
  cbFocusedBackground.Items   := cbNormalText.Items;

  // Populate the dialog.
  populateDialog();

End;

/////

Function TwegLibNGColoursDialog.findColourPanel( iID : TwegLibNGColourID ) : TPanel;
Var
  i : Integer;
Begin

  // Not that we should need to, but Nil means we couldn't find one.
  Result := Nil;

  // Loop over the components on the dialog, looking for the specific
  // colour panel that we're after.
  For i := 0 To ComponentCount - 1 Do
    If Components[ i ] Is TPanel Then
      If TPanel( Components[ i ] ).Tag = iID Then
      Begin
        Result := TPanel( Components[ i ] );
        Break;
      End;

End;

/////

Procedure TwegLibNGColoursDialog.FormKeyPress( Sender : TObject; Var Key : Char );
Begin

  // Let the user press space on one of the DOS map colour panels to simulate
  // a mouse click.
  If ( ActiveControl Is TPanel ) And ( Key = #32 ) Then
    pnlDOSMapClick( ActiveControl );

End;

/////

Procedure TwegLibNGColoursDialog.populateDialog;
Var
  i   : Integer;
  pnl : TPanel;
Begin

  // Init the DOS colour map.
  For i := Low( TwegLibNGColourID ) To High( TwegLibNGColourID ) Do
  Begin

    // Try and find the colour panel that goes with this colour.
    pnl := findColourPanel( i );

    // If we found it...
    If pnl <> Nil Then
    Begin

      // Set the colour
      pnl.Color := oColours[ i ];

      // Also, if the panel deals with one of the darker colours, make sure
      // that the text colour is white.
      If i In [ 0, 1, 2, 3, 4, 5, 6, 8, 9 ] Then
        pnl.Font.Color := clWhite;

    End;

  End;

  // Init the guide colours.
  cbNormalText.ItemIndex          := oColours.NormalText;
  cbNormalBackground.ItemIndex    := oColours.NormalBackground;
  cbBoldText.ItemIndex            := oColours.BoldText;
  cbBoldBackground.ItemIndex      := oColours.BoldBackground;
  cbReverseText.ItemIndex         := oColours.ReverseText;
  cbReverseBackground.ItemIndex   := oColours.ReverseBackground;
  cbUnderlineText.ItemIndex       := oColours.UnderlinedText;
  cbUnderlineBackground.ItemIndex := oColours.UnderlinedBackground;
  cbSelectedText.ItemIndex        := oColours.SelectedText;
  cbSelectedBackground.ItemIndex  := oColours.SelectedBackground;
  cbFocusedText.ItemIndex         := oColours.FocusedText;
  cbFocusedBackground.ItemIndex   := oColours.FocusedBackground;

End;

/////

Procedure TwegLibNGColoursDialog.btnDefaultsClick( Sender : TObject );
Begin

  // Reset to default colours.
  oColours.defaultColours();

  // Re-populate the dialog.
  populateDialog();

  // Repaint the guide colours.
  repaintGuideColours();

End;

/////

Procedure TwegLibNGColoursDialog.cbNGColourDrawItem( Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState );

  Function Squeeze( r : TRect; i : Integer ) : TRect;
  Begin

    Result := r;

    With Result Do
    Begin
      Inc( Top,    i );
      Inc( Left,   i );
      Dec( Bottom, i );
      Dec( Right,  i );
    End;

  End;

Var
  rSample : TRect;
  rText   : TRect;
Begin

  // Get the pointer to the combo box and its canvas.
  With TComboBox( Control ), TComboBox( Control ).Canvas Do
  Begin

    // Size the sample box.
    rSample       := Rect;
    rSample.Right := rSample.Bottom - rSample.Top;

    // Draw the sample box.
    Brush.Color := oColours[ Index ];
    fillrect( Squeeze( rSample, 2 ) );
    Brush.Color := clBlack;
    frameRect( Squeeze( rSample, 1 ) );

    // Decide on the background colour.
    If odSelected In State Then
      Brush.Color := clHighlight
    Else
      Brush.Color := clWindow;

    // Position the text box.
    rText      := Rect;
    rText.Left := rSample.Right;

    // Draw the text.
    fillRect( rText );
    textOut( rText.Left + textWidth( '.' ), rText.Top, Items[ Index ] );

  End;

End;

/////

Procedure TwegLibNGColoursDialog.repaintGuideColours;
Var
  i : Integer;
Begin

  For i := 0 To ComponentCount - 1 Do
    If Components[ i ] Is TComboBox Then
      TComboBox( Components[ i ] ).repaint();

End;

/////

Procedure TwegLibNGColoursDialog.FormClose( Sender : TObject; Var Action : TCloseAction );
Begin

  // Copy the guide colour settings into the colour control.
  oColours.NormalText           := cbNormalText.ItemIndex;
  oColours.NormalBackground     := cbNormalBackground.ItemIndex;
  oColours.BoldText             := cbBoldText.ItemIndex;
  oColours.BoldBackground       := cbBoldBackground.ItemIndex;
  oColours.ReverseText          := cbReverseText.ItemIndex;
  oColours.ReverseBackground    := cbReverseBackground.ItemIndex;
  oColours.UnderlinedText       := cbUnderlineText.ItemIndex;
  oColours.UnderlinedBackground := cbUnderlineBackground.ItemIndex;
  oColours.SelectedText         := cbSelectedText.ItemIndex;
  oColours.SelectedBackground   := cbSelectedBackground.ItemIndex;
  oColours.FocusedText          := cbFocusedText.ItemIndex;
  oColours.FocusedBackground    := cbFocusedBackground.ItemIndex;

End;

End.
