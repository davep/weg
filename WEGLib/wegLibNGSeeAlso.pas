{* File.......: wegLibNGSeeAlso.pas
 * System.....: WEGLib - Norton Guide Reader Library for Delphi.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * ID.........: $Id$
 * Description: Class for holding a see-also list.
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

Unit wegLibNGSeeAlso;

Interface

Uses
  Classes,
  wegLibNGMenu;

Type

  {** See also class }
  TwegLibNGSeeAlso = Class( TwegLibNGMenu )

  Public

    {** Load the see-also entries }
    Procedure load( hNG : TFileStream ); Override;

  End;

Implementation

Uses
  wegLibUtils;
  
/////

Procedure TwegLibNGSeeAlso.load( hNG : TFileStream );
ResourceString
  RSSeeAlso = 'See Also';
Var
  iPrompts : Integer;
  i        : Integer;
Begin

  // Set the title of the menu.
  FTitle := RSSeeAlso;
  
  // Get the see-also count.
  iPrompts := wegLibReadWord( hNG );

  // Size the see-also array.
  SetLength( FPrompts, iPrompts );

  // Read the offsets that each of the see-also entries point at.
  For i := 0 To iPrompts - 1 Do
    FPrompts[ i ].lOffset := wegLibReadLong( hNG );

  // Read the prompts of the see-also entries.
  For i := 0 To iPrompts - 1 Do
    If bOEMToANSI Then
      FPrompts[ i ].sPrompt := wegLibOEMToANSI( wegLibExpand( wegLibReadStringZ( hNG, wegLib_MAX_MENU_ITEM_LEN ) ) )
    Else
      FPrompts[ i ].sPrompt := wegLibExpand( wegLibReadStringZ( hNG, wegLib_MAX_MENU_ITEM_LEN ) );

End;

End.
