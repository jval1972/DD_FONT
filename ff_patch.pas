//------------------------------------------------------------------------------
//
//  DD_FONT: Doom Font Creator
//  Copyright (C)2021-2022 by Jim Valavanis
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
//  Doom patch creation
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit ff_patch;

interface

uses
  Windows, SysUtils, Classes, Graphics;

type
  patchheader_t = packed record
    width: smallint; // bounding box size
    height: smallint;
    leftoffset: smallint; // pixels to the left of origin
    topoffset: smallint;  // pixels below the origin
  end;

  column_t = packed record
    topdelta: byte; // -1 is the last post in a column
    length: byte;   // length data bytes follows
  end;
  Pcolumn_t = ^column_t;

procedure FF_CreateDoomPatch(const img: PByteArray; const width, height: integer;
  const solid: boolean; out p: pointer; out size: integer; const offsx: integer = -255; const offsy: integer = -255);

implementation

uses
  ff_utils;

procedure FF_CreateDoomPatch(const img: PByteArray; const width, height: integer;
  const solid: boolean; out p: pointer; out size: integer; const offsx: integer = -255; const offsy: integer = -255);
var
  x, y: integer;
  c: LongWord;
  m, fs: TMemoryStream;
  patch: patchheader_t;
  column: column_t;
  columnofs: TDNumberList;
  columndata: TDByteList;
  i: integer;

  procedure flashcolumnend;
  begin
    column.topdelta := 255;
    column.length := 0;
    m.Write(column, SizeOf(column_t));
  end;

  procedure flashcolumndata;
  var
    bb: byte;
  begin
    if columndata.Count > 0 then
    begin
      column.topdelta := y - columndata.Count;
      column.length := columndata.Count;
      m.Write(column, SizeOf(column_t));
      bb := 0;
      m.Write(bb, SizeOf(bb));
      m.Write(columndata.List^, columndata.Count);
      m.Write(bb, SizeOf(bb));
      columndata.FastClear;
    end;
  end;

begin
  m := TMemoryStream.Create;
  fs := TMemoryStream.Create;
  columnofs := TDNumberList.Create;
  columndata := TDByteList.Create;
  try
    patch.width := width;
    patch.height := height;
    if offsx = -255 then
      patch.leftoffset := width div 2
    else
      patch.leftoffset := offsx;
    if offsy = -255 then
      patch.topoffset := height
    else
      patch.topoffset := offsy;
    fs.Write(patch, SizeOf(patchheader_t));

    for x := 0 to width - 1 do
    begin
      columnofs.Add(m.Position + SizeOf(patchheader_t) + width * SizeOf(integer));
      columndata.FastClear;
      for y := 0 to height - 1 do
      begin
        c := img[x + y * width];
        if not solid then
          if c = 0 then
          begin
            flashcolumndata;
            continue;
          end;
        columndata.Add(c);
      end;
      flashcolumndata;
      flashcolumnend;
    end;

    for i := 0 to columnofs.Count - 1 do
    begin
      x := columnofs.Numbers[i];
      fs.Write(x, SizeOf(integer));
    end;

    size := fs.Size + m.Size;
    GetMem(p, size);

    memcpy(p, fs.Memory, fs.Size);
    memcpy(pointer(integer(p) + fs.Size), m.Memory, m.Size);

  finally
    m.Free;
    columnofs.Free;
    columndata.Free;
    fs.Free;
  end;
end;

end.
