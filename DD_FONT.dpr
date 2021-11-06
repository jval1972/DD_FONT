//------------------------------------------------------------------------------
//
//  DD_FONT: Doom Font Creator
//  Copyright (C) 2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Project file
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

program DD_FONT;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  ff_utils in 'ff_utils.pas',
  ff_binary in 'ff_binary.pas',
  ff_filemenuhistory in 'ff_filemenuhistory.pas',
  ff_undo in 'ff_undo.pas',
  ff_defs in 'ff_defs.pas',
  ff_engine in 'ff_engine.pas',
  ff_slider in 'ff_slider.pas',
  ff_doomfont in 'ff_doomfont.pas',
  ff_tmp in 'ff_tmp.pas',
  ff_palettes in 'ff_palettes.pas',
  ff_wad in 'ff_wad.pas',
  ff_wadwriter in 'ff_wadwriter.pas',
  ff_patch in 'ff_patch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Doom Font Creator';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
