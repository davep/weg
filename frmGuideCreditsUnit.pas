{* File.......: frmGuideCreditsUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Guide credits dialog.
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

Unit frmGuideCreditsUnit;

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
  wegLibNortonGuide,
  StdCtrls;

Type

  {** Guide credits dialog. }
  TfrmGuideCredits = Class( TForm )
    btnOK: TButton;
    memCredits: TMemo;
    lblFileNameLabel: TLabel;
    lblFileName: TLabel;
    lblFileLocationLabel: TLabel;
    lblFileLocation: TLabel;
    lblFileSizeLabel: TLabel;
    lblFileSize: TLabel;
    lblTimeStampLabel: TLabel;
    lblTimeStamp: TLabel;
    btnCopy: TButton;
    procedure FormShow(Sender: TObject);
    procedure memCreditsEnter(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);

  Public

    {** Pointer to the Norton Guide database we're displaying the credits for. }
    oNortonGuide : TwegLibNortonGuide;

  End;

Implementation

Uses
  Clipbrd,
  frmMainUnit;

{$R *.DFM}

/////

Procedure TfrmGuideCredits.FormShow( Sender : TObject );
Begin

  // Set the caption.
  Caption := Caption + ' "' + oNortonGuide.Title + '"';

  // Use the user's font choice.
  memCredits.Font := frmMain.NGSettings.GuideFont;

  // Use the user's colour choices.
  memCredits.Font.Color := frmMain.NGColours.DOSMap[ frmMain.NGColours.NormalText ];
  memCredits.Color      := frmMain.NGColours.DOSMap[ frmMain.NGColours.NormalBackground ];

  // Set the values of the labels.
  lblFileName.Caption     := ExtractFileName( oNortonGuide.Guide );
  lblFileLocation.Caption := ExtractFileDir( oNortonGuide.Guide );
  lblFileSize.Caption     := Format( '%.0n', [ Int( oNortonGuide.Size ) ] );
  lblTimeStamp.Caption    := DateTimeToStr( oNortonGuide.TimeStamp );

  // If there are some credits...
  If Trim( oNortonGuide.Credits ) <> '' Then
    // ...place them in the memo.
    memCredits.Lines.Text := oNortonGuide.Credits
  Else
    // ...hide the copy button, there's nothing to copy.
    btnCopy.Visible := False;


End;

/////

Procedure TfrmGuideCredits.memCreditsEnter( Sender : TObject );
Begin
  btnOK.setFocus();
End;

/////

Procedure TfrmGuideCredits.btnCopyClick( Sender : TObject );
Begin
  Clipboard().asText := memCredits.Lines.Text;
End;

End.
