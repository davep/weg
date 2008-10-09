{* File.......: frmBookmarkNameUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2004
 * ID.........: $Id: frmBookmarkNameUnit.pas,v 1.1 2004/02/26 09:57:00 davep Exp $
 * Description: Bookmark name input form.
 * Licence....: GNU General Public Licence (see below)
 *
 * WEG - Norton Guide Reader for Windows.
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

Unit frmBookmarkNameUnit;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs, StdCtrls;

Type

  {** Bookmark name input form }
  TfrmBookmarkName = Class( TForm )
    lblCaption: TLabel;
    edtCaption: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure refreshDialog(Sender: TObject);
  End;

Implementation

{$R *.DFM}

/////

Procedure TfrmBookmarkName.refreshDialog( Sender : TObject );
Begin
  btnOk.Enabled := Trim( edtCaption.Text ) <> '';
End;

End.
