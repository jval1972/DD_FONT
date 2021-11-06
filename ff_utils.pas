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
//  Utility functions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit ff_utils;

interface

uses
  Windows, SysUtils, Graphics, pngimage, jpeg;

function GetIntInRange(const x: Integer; const amin, amax: Integer): Integer;

function I_VersionBuilt(fname: string = ''): string;

function MinI(const a, b: Integer): Integer;

function CopyFile(const sname, dname: string): boolean;

procedure BackupFile(const fname: string);

function MkShortName(const fname: string): string;

procedure SaveImageToDisk(const b: TBitmap; const imgfname: string);

procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);

type
  TDNumberList = class
  private
    fList: PIntegerArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): integer; virtual;
    procedure Put(Index: Integer; const value: integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: integer): integer; overload; virtual;
    procedure Add(const nlist: TDNumberList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: integer): integer; virtual;
    procedure Clear;
    procedure FastClear;
    function Sum: integer;
    property Count: integer read fNumItems;
    property Numbers[Index: Integer]: integer read Get write Put; default;
    property List: PIntegerArray read fList;
  end;

type
  TDByteList = class
  private
    fList: PByteArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): byte; virtual;
    procedure Put(Index: Integer; const value: byte); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: byte): integer; overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: byte): integer; virtual;
    procedure Clear;
    procedure FastClear;
    property Count: integer read fNumItems;
    property Bytes[Index: Integer]: byte read Get write Put; default;
    property List: PByteArray read fList;
  end;

implementation

function GetIntInRange(const x: Integer; const amin, amax: Integer): Integer;
begin
  Result := x;
  if Result < amin then
    Result := amin
  else if Result > amax then
    Result := amax;
end;

function I_VersionBuilt(fname: string = ''): string;
var
  vsize: LongWord;
  zero: LongWord;
  buffer: PByteArray;
  res: pointer;
  len: LongWord;
  i: integer;
begin
  if fname = '' then
    fname := ParamStr(0);
  vsize := GetFileVersionInfoSize(PChar(fname), zero);
  if vsize = 0 then
  begin
    result := '';
    exit;
  end;

  GetMem(buffer, vsize + 1);
  GetFileVersionInfo(PChar(fname), 0, vsize, buffer);
  VerQueryValue(buffer, '\StringFileInfo\040904E4\FileVersion', res, len);
  result := '';
  for i := 0 to len - 1 do
  begin
    if PChar(res)^ = #0 then
      break;
    result := result + PChar(res)^;
    res := pointer(integer(res) + 1);
  end;
  FreeMem(pointer(buffer), vsize + 1);
end;

function MinI(const a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function CopyFile(const sname, dname: string): boolean;
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if FileExists(sname) then
  begin
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    Result := True;
  end
  else
    Result := False;
end;

procedure BackupFile(const fname: string);
var
  fbck: string;
begin
  if not FileExists(fname) then
    Exit;
  fbck := fname + '_bak';
  CopyFile(fname, fbck);
end;

function MkShortName(const fname: string): string;
const
  MAXDISPFNAME = 30;
var
  i: integer;
begin
  if Length(fname) < MAXDISPFNAME then
  begin
    Result := fname;
    exit;
  end;
  Result := '';
  for i := Length(fname) downto Length(fname) - (MAXDISPFNAME - 6) do
    Result := fname[i] + Result;
  Result := '...' + Result;
  for i := 3 downto 1 do
    Result := fname[i] + Result;
end;

procedure SaveImageToDisk(const b: TBitmap; const imgfname: string);
var
  png: TPngObject;
  jpg: TJPEGImage;
  ext: string;
begin
  ext := UpperCase(ExtractFileExt(imgfname));
  if ext = '.PNG' then
  begin
    png := TPngObject.Create;
    png.Assign(b);
    png.SaveToFile(imgfname);
    png.Free;
  end
  else if (ext = '.JPG') or (ext = '.JPEG') then
  begin
    jpg := TJPEGImage.Create;
    jpg.Assign(b);
    jpg.SaveToFile(imgfname);
    jpg.Free;
  end
  else
    b.SaveToFile(imgfname);
end;

procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);
begin
  Move(src0^, dest0^, count0);
end;

////////////////////////////////////////////////////////////////////////////////
// TDNumberList
constructor TDNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TDNumberList.Destroy;
begin
  Clear;
end;

function TDNumberList.Get(Index: Integer): integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

procedure TDNumberList.Put(Index: Integer; const value: integer);
begin
  fList[Index] := value;
end;

function TDNumberList.Add(const value: integer): integer;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    ReallocMem(fList, newrealitems * SizeOf(integer));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

procedure TDNumberList.Add(const nlist: TDNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

function TDNumberList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

function TDNumberList.IndexOf(const value: integer): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TDNumberList.Clear;
begin
  ReallocMem(fList, 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDNumberList.FastClear;
begin
  fNumItems := 0;
end;

function TDNumberList.Sum: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to fNumItems - 1 do
    result := result + fList[i];
end;
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// TDByteList
constructor TDByteList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TDByteList.Destroy;
begin
  Clear;
end;

function TDByteList.Get(Index: Integer): byte;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

procedure TDByteList.Put(Index: Integer; const value: byte);
begin
  fList[Index] := value;
end;

function TDByteList.Add(const value: byte): integer;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    ReallocMem(fList, newrealitems * SizeOf(byte));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

function TDByteList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

function TDByteList.IndexOf(const value: byte): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TDByteList.Clear;
begin
  ReallocMem(fList, 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDByteList.FastClear;
begin
  fNumItems := 0;
end;

end.

