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
//  Font Engine
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit ff_engine;

interface

uses
  Windows, SysUtils, Classes, Graphics;

type
  TFontEngine = class(TObject)
  private
    fHeight: Integer;
    fPitch: TFontPitch;
    fStyle: TFontStylesBase;
    fCharset: TFontCharset;
    fFontName: TFontDataName;
    fDrawWidth: Integer;
    fDrawHeight: Integer;
    fFontSize: Integer;
    fFrontColor: LongWord;
    fBackColor: LongWord;
    fGridWidth: Integer;
    fGridHeight: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure DrawToCanvas(const C: TCanvas);
    procedure DrawToBitmap(const bm: TBitmap);
    procedure FromFont(const fnt: TFont);
    procedure ToFont(const fnt: TFont);
    procedure SaveToStream(const strm: TStream);
    procedure LoadFromStream(const strm: TStream);
    procedure SaveToFile(const fn: string);
    procedure LoadFromFile(const fn: string);
    property Height: Integer read fHeight write fHeight;
    property Pitch: TFontPitch read fPitch write fPitch;
    property Style: TFontStylesBase read fStyle write fStyle;
    property Charset: TFontCharset read fCharset write fCharset;
    property FontName: TFontDataName read fFontName write fFontName;
    property DrawWidth: integer read fDrawWidth write fDrawWidth;
    property DrawHeight: integer read fDrawHeight write fDrawHeight;
    property FontSize: integer read fFontSize write fFontSize;
    property FrontColor: LongWord read fFrontColor write fFrontColor;
    property BackColor: LongWord read fBackColor write fBackColor;
    property GridWidth: Integer read fGridWidth write fGridWidth;
    property GridHeight: Integer read fGridHeight write fGridHeight;
  end;


implementation

constructor TFontEngine.Create;
begin
  inherited Create;
  Reset;
end;

destructor TFontEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TFontEngine.Reset;
begin
  fHeight := -11;
  fPitch := fpDefault;
  fStyle := [];
  fCharset := DEFAULT_CHARSET;
  fFontName := 'Doom';
  fDrawWidth := 16;
  fDrawHeight := 16;
  fFontSize := 8;
  fFrontColor := RGB(255, 255, 255);
  fBackColor := RGB(0, 0, 0);
  fGridWidth := 16;
  fGridHeight := 16;
end;

procedure TFontEngine.DrawToCanvas(const C: TCanvas);
var
  buf, letter: TBitmap;
  bC, lC: TCanvas;
  gx, gy: integer;
  ch: Char;
begin
  buf := TBitmap.Create;
  buf.Width := fDrawWidth * fGridWidth;
  buf.Height := fDrawHeight * fGridHeight;
  buf.PixelFormat := pf32bit;

  bC := buf.Canvas;
  bC.Pen.Style := psClear;
  bC.Pen.Color := fBackColor;
  bC.Brush.Style := bsSolid;
  bC.Brush.Color := fBackColor;
  bC.FillRect(Rect(0, 0, fDrawWidth * fGridWidth, fDrawHeight * fGridHeight));

  letter := TBitmap.Create;
  letter.Width := fDrawWidth;
  letter.Height := fDrawHeight;
  letter.PixelFormat := pf32bit;

  lC := letter.Canvas;
  lC.Pen.Style := psClear;
  lC.Pen.Color := fBackColor;
  lC.Brush.Style := bsSolid;
  lC.Brush.Color := fBackColor;
  lC.Font.Height := fHeight;
  lC.Font.Pitch := fPitch;
  lC.Font.Style := fStyle;
  lC.Font.Charset := fCharset;
  lC.Font.Color := fFrontColor;
  lC.Font.Name := fFontName;
  lC.Font.Size := fFontSize;

  ch := #0;
  for gy := 0 to fGridHeight - 1 do
    for gx := 0 to fGridWidth - 1 do
    begin
      lC.FillRect(Rect(0, 0, fDrawWidth, fDrawHeight));
      lC.TextOut(0, 0, ch);
      bC.Draw(gx * fDrawWidth, gy * fDrawHeight, letter);
      Inc(ch);
    end;

  C.Draw(0, 0, buf);

  buf.Free;
  letter.Free;
end;

procedure TFontEngine.DrawToBitmap(const bm: TBitmap);
begin
  bm.Width := fDrawWidth * fGridWidth;
  bm.Height := fDrawHeight * fGridHeight;
  bm.PixelFormat := pf32bit;

  DrawToCanvas(bm.Canvas);
end;

procedure TFontEngine.FromFont(const fnt: TFont);
begin
  fHeight := fnt.Height;
  fHeight := fnt.Height;
  fPitch := fnt.Pitch;
  fStyle := fnt.Style;
  fCharset := fnt.Charset;
  fFrontColor := fnt.Color;
  fFontName := fnt.Name;
  fFontSize := fnt.Size;
end;

procedure TFontEngine.ToFont(const fnt: TFont);
begin
  fnt.Height := fHeight;
  fnt.Pitch := fPitch;
  fnt.Style := fStyle;
  fnt.Charset := fCharset;
  fnt.Color := fFrontColor;
  fnt.Name := fFontName;
  fnt.Size := fFontSize;
end;

const
  sHeight = 'Height';
  sPitch = 'Pitch';
  sStyle = 'Style';
  sCharset = 'Charset';
  sFontName = 'FontName';
  sDrawWidth = 'DrawWidth';
  sDrawHeight = 'DrawHeight';
  sFontSize = 'FontSize';
  sFrontColor = 'FrontColor';
  sBackColor = 'BackColor';
  sGridWidth = 'GridWidth';
  sGridHeight = 'GridHeight';

procedure TFontEngine.SaveToStream(const strm: TStream);
var
  sl: TStringList;
  stl: integer;
begin
  sl := TStringList.Create;
  try
    sl.Add(sHeight + '=' + IntToStr(fHeight));

    sl.Add(sPitch + '=' + IntToStr(Ord(fPitch)));

    stl := 0;
    if fsBold in fStyle then
      stl := stl or 1;
    if fsItalic in fStyle then
      stl := stl or 2;
    if fsUnderline in fStyle then
      stl := stl or 4;
    if fsStrikeOut in fStyle then
      stl := stl or 8;
    sl.Add(sStyle + '=' + IntToStr(stl));

    sl.Add(sCharset + '=' + IntToStr(fCharset));

    sl.Add(sFontName + '=' + fFontName);

    sl.Add(sDrawWidth + '=' + IntToStr(fDrawWidth));

    sl.Add(sDrawHeight + '=' + IntToStr(fDrawHeight));

    sl.Add(sFontSize + '=' + IntToStr(fFontSize));

    sl.Add(sFrontColor + '=' + IntToStr(fFrontColor));

    sl.Add(sBackColor + '=' + IntToStr(fBackColor));

    sl.Add(sGridWidth + '=' + IntToStr(fGridWidth));

    sl.Add(sGridHeight + '=' + IntToStr(fGridHeight));

    sl.SaveToStream(strm);
  finally
    sl.Free;
  end;
end;

procedure TFontEngine.LoadFromStream(const strm: TStream);
var
  sl: TStringList;
  svalue: string;
  tmp: integer;

  procedure _load_int(const n: string; const pi: PInteger);
  begin
    svalue := sl.Values[n];
    if svalue <> '' then
      pi^ := StrToIntDef(svalue, pi^);
  end;

begin
  sl := TStringList.Create;
  try
    sl.LoadFromStream(strm);

    _load_int(sHeight, @fHeight);

    tmp := Ord(fPitch);
    _load_int(sPitch, @tmp);
    fPitch := TFontPitch(tmp);

    tmp := 0;
    if fsBold in fStyle then
      tmp := tmp or 1;
    if fsItalic in fStyle then
      tmp := tmp or 2;
    if fsUnderline in fStyle then
      tmp := tmp or 4;
    if fsStrikeOut in fStyle then
      tmp := tmp or 8;
    _load_int(sStyle, @tmp);
    fStyle := [];
    if tmp and 1 <> 0 then
      Include(fStyle, fsBold);
    if tmp and 2 <> 0 then
      Include(fStyle, fsItalic);
    if tmp and 4 <> 0 then
      Include(fStyle, fsUnderline);
    if tmp and 8 <> 0 then
      Include(fStyle, fsStrikeOut);

    tmp := fCharset;
    _load_int(sCharset, @tmp);
    fCharset := tmp;

    svalue := sl.Values[sFontName];
    if svalue <> '' then
      fFontName := svalue;

    _load_int(sDrawWidth, @fDrawWidth);
    _load_int(sDrawHeight, @fDrawHeight);
    _load_int(sFontSize, @fFontSize);
    _load_int(sFrontColor, @fFrontColor);
    _load_int(sBackColor, @fBackColor);
    _load_int(sGridWidth, @fGridWidth);
    _load_int(sGridHeight, @fGridHeight);

  finally
    sl.Free;
  end;
end;

procedure TFontEngine.SaveToFile(const fn: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fn, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TFontEngine.LoadFromFile(const fn: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

end.
