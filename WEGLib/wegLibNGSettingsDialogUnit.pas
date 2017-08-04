{* File.......: wegLibNGSettingsDialogUnit.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: Dialog for editing wegLibNGSettings values.
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

Unit wegLibNGSettingsDialogUnit;

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

  {** Dialog for editing wegLibNGSettings values. }
  TwegLibNGSettingsDialog = Class( TForm )
    lblGuideDirectory: TLabel;
    edtGuideDirectory: TEdit;
    btnGuideDirectory: TButton;
    btnFont: TButton;
    pnlFont: TPanel;
    fdGuideFont: TFontDialog;
    cbUseColour: TCheckBox;
    cbOEMToANSI: TCheckBox;
    cbSingleClick: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure refreshDialog(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnGuideDirectoryClick(Sender: TObject);
  End;

Implementation

Uses
  FileCtrl;

{$R *.DFM}

Procedure TwegLibNGSettingsDialog.refreshDialog( Sender : TObject );
Begin
  pnlFont.Font    := fdGuideFont.Font;
  pnlFont.Caption := fdGuideFont.Font.Name;
End;

/////

Procedure TwegLibNGSettingsDialog.btnFontClick( Sender : TObject );
Begin

  If fdGuideFont.execute() Then
    refreshDialog( Sender );

End;

/////

Procedure TwegLibNGSettingsDialog.btnGuideDirectoryClick( Sender : TObject );
ResourceString
  RSCaption = 'Default Guide Directory';
Var
  sDir : String;
Begin

  // Let the user browse for the directory...
  If SelectDirectory( RSCaption, '', sDir ) Then
  Begin
    // ...if they picked one, use it.
    edtGuideDirectory.Text := sDir;
    // Set focus back on the guide directory edit control.
    edtGuideDirectory.setFocus();
  End;

End;

End.
