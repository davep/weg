{* File.......: WEG.dpr
 * System.....: WEG - Norton Guide Reader For Windows.
 * Author.....: Dave Pearson <davep@davep.org>
 * Copyright..: Dave Pearson 2003
 * Description: WEG project file.
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

Program WEG;

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  frmGuideUnit in 'frmGuideUnit.pas' {frmGuide},
  wegUtils in 'wegUtils.pas',
  frmGuideManagerUnit in 'frmGuideManagerUnit.pas' {frmGuideManager},
  frmAboutUnit in 'frmAboutUnit.pas' {frmAbout},
  frmGuideCreditsUnit in 'frmGuideCreditsUnit.pas' {frmGuideCredits},
  frmGuideEntryFindUnit in 'frmGuideEntryFindUnit.pas' {frmGuideEntryFind},
  frmGlobalFindUnit in 'frmGlobalFindUnit.pas' {frmGlobalFind},
  frmBookmarksUnit in 'frmBookmarksUnit.pas' {frmBookmarks},
  frmBookmarkNameUnit in 'frmBookmarkNameUnit.pas' {frmBookmarkName};

{$R *.RES}

Begin
  Application.Initialize;
  Application.Title := 'Expert Guide';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGuideManager, frmGuideManager);
  Application.CreateForm(TfrmGlobalFind, frmGlobalFind);
  Application.CreateForm(TfrmBookmarks, frmBookmarks);
  Application.Run;
End.
