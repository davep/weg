{* File.......: frmAboutUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: About dialog for WEG.
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

Unit frmAboutUnit;

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
  StdCtrls,
  ExtCtrls;

Type

  {** WEG's About dialog. }
  TfrmAbout = Class( TForm )
    imIcon: TImage;
    lblTitle: TLabel;
    btnOK: TButton;
    lblCopyright: TLabel;
    lblHomePage: TLabel;
    lblSupport: TLabel;
    lblWEGLib: TLabel;
    lblHomePageURL: TLabel;
    lblSupportURL: TLabel;
    btnLicence: TButton;
    btnWarranty: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnLicenceClick(Sender: TObject);
    procedure btnWarrantyClick(Sender: TObject);
    procedure lblFireURL(Sender: TObject);
  End;

Implementation

{$R *.DFM}

Uses
  wegUtils,
  wegLibUtils;

Procedure TfrmAbout.FormShow( Sender : TObject );
Begin
  // Stuff the WEG version number into the dialog.
  lblTitle.Caption := Format( lblTitle.Caption, [ wegVersion() ] );
  // Stuff the WEGLib version number into the dialog.
  lblWEGLib.Caption := Format( lblWEGLib.Caption, [ wegLib_VERSION ] );
End;

/////

Procedure TfrmAbout.btnLicenceClick( Sender : TObject );
ResourceString
  LICENCE_TEXT = 'This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the license, or (at your option) any later version.'#13#10#13#10 +
                 'Please note that because WEG makes use of Delphi''s VCL and Toolbar2000 you should consider the VCL and Toolbar2000 to be "system software" in regard to the "special exception" mentioned in section 3 of the GPL.'#13#10#13#10 +
                 'You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.';
Begin
  MessageDlg( LICENCE_TEXT, mtInformation, [ mbOk ], 0 );
End;

/////

Procedure TfrmAbout.btnWarrantyClick( Sender : TObject );
ResourceString
  WARRANTY_TEXT = 'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.'#13#10#13#10 +
                  'You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.';
Begin
  MessageDlg( WARRANTY_TEXT, mtInformation, [ mbOk ], 0 );
End;

/////

Procedure TfrmAbout.lblFireURL( Sender : TObject );
Begin
  wegFireURL( TLabel( Sender ).Hint );
End;

End.
