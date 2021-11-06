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
//  Export Font Code
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit ff_export;

interface

uses
  Windows, SysUtils, Classes, Graphics, ff_engine;

procedure FF_ExportFont(const ff1: TFontEngine; const spal: string;
  const s: string; const fixed_pitch: Boolean; const perlin_noise: Boolean;
  const solid: Boolean; const offsx: integer; const offsy: integer;
  out p: Pointer; out size: Integer);

implementation

uses
  Math, ff_palettes, ff_utils, ff_patch;

const
  MAXPATCHWIDTH = 320;

type
  TBitmapLine = array[0..MAXPATCHWIDTH - 1] of LongWord;
  PBitmapLine = ^TBitmapLine;

function Grayscale(const c: LongWord): LongWord;
var
  r, g, b: byte;
  gray: LongWord;
begin
  r := (c shr 16) and $ff;
  g := (c shr 8) and $ff;
  b := c and $ff;
  gray := Trunc(r * 0.299 + g * 0.587 + b * 0.114); // Human perceive
  if gray > 255 then gray := 255;
  result := gray + gray shl 8 + gray shl 16;
end;

procedure FF_ExportFont(const ff1: TFontEngine; const spal: string;
  const s: string; const fixed_pitch: Boolean; const perlin_noise: Boolean;
  const solid: Boolean; const offsx: integer; const offsy: integer;
  out p: Pointer; out size: Integer);
var
  ff: TFontEngine;
  bkColor, fgColor: LongWord;
  bm: TBitmap;
  rpal: rawpalette_p;
  dpal: TDoomPalette;
  start, finish: integer; // Usable palette indexes
  x, y: integer;
  l: PBitmapLine;
  img: PByteArray;
  i: integer;
  c: LongWord;
  gray, invgray: byte;
  bkR, bkG, bkB: LongWord;
  fgR, fgG, fgB: LongWord;
  r, g, b: LongWord;
  pnoise: Double;

  function Interpolate(const a, b, frac: double): double;
  begin
    result := (1.0 - cos(pi * frac)) * 0.5;
    result:= a * (1 - result) + b * result;
  end;

  function Noise(const x,y: double): double;
  var
    n: integer;
  begin
    n := trunc(x + y * 57);
    n := (n shl 13) xor n;
    result := (1.0 - ( (n * (n * n * $EC4D + $131071F) + $5208DD0D) and $7FFFFFFF) / $40000000);
  end;

  function SmoothedNoise(const x, y: double): double;
  var
    corners: double;
    sides: double;
    center: double;
  begin
    corners := (Noise(x - 1, y - 1) + Noise(x + 1, y - 1) + Noise(x - 1, y + 1) + Noise(x + 1, y + 1) ) / 16;
    sides := (Noise(x - 1, y) + Noise(x + 1, y) + Noise(x, y - 1) + Noise(x, y + 1)) / 8;
    center := Noise(x, y) / 4;
    result := corners + sides + center
  end;

  function InterpolatedNoise(const x, y: double): double;
  var
    i1, i2: double;
    v1, v2, v3, v4: double;
    xInt: double;
    yInt: double;
    xFrac: double;
    yFrac: double;
  begin
    xInt := Int(x);
    xFrac := Frac(x);

    yInt := Int(y);
    yFrac := Frac(y);

    v1 := SmoothedNoise(xInt, yInt);
    v2 := SmoothedNoise(xInt + 1, yInt);
    v3 := SmoothedNoise(xInt, yInt + 1);
    v4 := SmoothedNoise(xInt + 1, yInt + 1);

    i1 := Interpolate(v1, v2, xFrac);
    i2 := Interpolate(v3, v4, xFrac);

    result := Interpolate(i1, i2, yFrac);
  end;

  function PerlinNoise(const x, y: integer): double;
  const
    PERSISTENCE = 0.50;
    LOOPCOUNT = 3;
    VARIATION = 16;
  var
    amp: double;
    ii: integer;
    freq: integer;
  begin
    freq := 1;
    result := 0.0;
    for ii := 0 to LOOPCOUNT - 1 do
    begin
      amp := Power(PERSISTENCE, ii);
      result := result + InterpolatedNoise(x * freq, y * freq) * amp;
      freq := freq shl 1;
    end;
    result := result * VARIATION;
  end;

begin
  ff := TFontEngine.Create;
  ff.AttachTo(ff1);

  bkColor := ff1.BackColor;
  bkR := GetRValue(bkColor);
  bkG := GetGValue(bkColor);
  bkB := GetBValue(bkColor);

  fgColor := ff1.FrontColor;
  fgR := GetRValue(fgColor);
  fgG := GetGValue(fgColor);
  fgB := GetBValue(fgColor);

  ff.BackColor := RGB(0, 0, 0);
  ff.FrontColor := RGB(255, 255, 255);

  FF_GetPaletteRange(spal, start, finish);
  if start = 0 then
    start := 1; // Keep 0 index for transparency
  rpal := GetPaletteFromName(spal);
  FF_RawPalette2DoomPalette(rpal, @dpal);

  bm := TBitmap.Create;
  bm.Width := MAXPATCHWIDTH; // Maximum doom screen
  bm.Height := ff.DrawHeight;
  bm.PixelFormat := pf32bit;

  for y := 0 to bm.Height - 1 do
  begin
    l := bm.ScanLine[y];
    for x := 0 to bm.Width - 1 do
      l[x] := 0;
  end;

  // Draw font
  bm.Width := ff.DrawStringToBitmap(bm, s, fixed_pitch);
  if bm.Width > MAXPATCHWIDTH then
    bm.Width := MAXPATCHWIDTH;

  GetMem(img, bm.Width * bm.Height);
  ZeroMemory(img, bm.Width * bm.Height);

  // Convert to image palette
  i := 0;
  for y := 0 to bm.Height - 1 do
  begin
    l := bm.ScanLine[y];
    for x := 0 to bm.Width - 1 do
    begin
      c := Grayscale(l[x]);
      gray := c and $FF;
      if gray <> 0 then
      begin
        invgray := 255 - gray;
        if perlin_noise then
        begin
          pnoise := PerlinNoise(x, y);
          r := GetIntInRange(Round(bkR * invgray / 255 + fgR * gray / 255 + pnoise), 0, 255);
          g := GetIntInRange(Round(bkG * invgray / 255 + fgG * gray / 255 + pnoise), 0, 255);
          b := GetIntInRange(Round(bkB * invgray / 255 + fgB * gray / 255 + pnoise), 0, 255);
        end
        else
        begin
          r := GetIntInRange(Round(bkR * invgray / 255 + fgR * gray / 255), 0, 255);
          g := GetIntInRange(Round(bkG * invgray / 255 + fgG * gray / 255), 0, 255);
          b := GetIntInRange(Round(bkB * invgray / 255 + fgB * gray / 255), 0, 255);
        end;
        img[i] := FF_FindAproxColorIndex(@DispCallByIDProc, RGB(r, g, b), start, finish);
      end;
      Inc(i);
    end;
  end;

  FF_CreateDoomPatch(img, bm.Width, bm.Height, solid, p, size, offsx, offsy);

  FreeMem(img);
  bm.Free;
  ff.Free;
end;

end.
