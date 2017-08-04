{* File.......: frmGuideEntryFindUnit.pas
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Dialog for controlling the finding of text in a guide entry.
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

Unit frmGuideEntryFindUnit;

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

  {** Dialog for controlling the finding of text in a guide entry. }
  TfrmGuideEntryFind = Class( TForm )
    lblFind: TLabel;
    edtFind: TEdit;
    cbCaseSensitive: TCheckBox;
    cbRegularExpression: TCheckBox;
    btnFind: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure refreshDialog(Sender: TObject);
  End;

Implementation

{$R *.DFM}

Procedure TfrmGuideEntryFind.btnCancelClick( Sender : TObject );
Begin
  close();
End;

/////

Procedure TfrmGuideEntryFind.refreshDialog( Sender : TObject );
Begin
  btnFind.Enabled := ( edtFind.Text <> '' );
End;

End.
