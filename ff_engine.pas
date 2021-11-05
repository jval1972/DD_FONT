//------------------------------------------------------------------------------
//
//  DD_FONT: Font Editor
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
  Windows, Classes, Graphics;

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
    procedure Draw(const C: TCanvas);
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
  fHeight := -11;
  fPitch := fpDefault;
  fStyle := [];
  fCharset := DEFAULT_CHARSET;
  fFontName := 'Tahoma';
  fDrawWidth := 16;
  fDrawHeight := 16;
  fFontSize := 8;
  fFrontColor := RGB(255, 255, 255);
  fBackColor := RGB(0, 0, 0);
  fGridWidth := 16;
  fGridHeight := 16;
end;

destructor TFontEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TFontEngine.Draw(const C: TCanvas);
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
  lC.FillRect(Rect(0, 0, fDrawWidth, fDrawHeight));
  lC.Font.Height := fHeight;
  lC.Font.Pitch := fPitch;
  lC.Font.Style := fStyle;
  lC.Font.Charset := fCharset;
  lC.Font.Color := fFrontColor;
  lC.Font.Name := fFontName;
  lC.Font.Size := fFontSize;

  ch := #0;
  for gy := 0 to fGridHeight - 1 do
    for gx := 0 to fGridWidth do
    begin
      letter.Canvas.TextOut(0, 0, ch);
      bC.Draw(gx * fGridWidth, gy * fGridHeight, letter);
      Inc(ch);
    end;

  C.Draw(0, 0, buf);

  buf.Free;
  letter.Free;
end;

end.
