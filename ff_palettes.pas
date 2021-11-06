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
//  Palette Stuff
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit ff_palettes;

interface

const
  spalDOOM = 'DOOM';
  spalHERETIC = 'HERETIC';
  spalHEXEN = 'HEXEN';
  spalSTRIFE = 'STRIFE';
  spalRADIX = 'RADIX';
  spalGLSPEED = 'GLSPEED';

type
  rawpalette_t = packed array[0..767] of Byte;
  rawpalette_p = ^rawpalette_t;

  TDoomPalette = array[0..255] of LongWord;
  PDoomPalette = ^TDoomPalette;

const
  RadixPaletteRaw: rawpalette_t = (
    $00, $00, $00, $C4, $BC, $B8, $BC, $B4, $B0, $B0, $A8, $A4, $AC, $A4, $A4,
    $A4, $9C, $9C, $A0, $98, $94, $98, $90, $8C, $90, $88, $88, $88, $80, $80,
    $84, $7C, $78, $7C, $74, $74, $74, $6C, $6C, $6C, $64, $64, $68, $60, $60,
    $64, $5C, $5C, $60, $58, $58, $5C, $54, $54, $54, $50, $50, $50, $4C, $4C,
    $4C, $48, $48, $48, $44, $44, $44, $40, $40, $40, $3C, $3C, $3C, $38, $38,
    $38, $34, $34, $30, $30, $30, $28, $28, $28, $20, $20, $20, $18, $18, $18,
    $10, $10, $10, $00, $00, $00, $C0, $C0, $CC, $B8, $B8, $C4, $B0, $B0, $BC,
    $AC, $AC, $B4, $A4, $A4, $B0, $98, $98, $A4, $90, $90, $9C, $88, $88, $90,
    $80, $80, $8C, $7C, $7C, $84, $74, $74, $7C, $70, $70, $7C, $6C, $6C, $74,
    $64, $64, $6C, $60, $60, $68, $5C, $5C, $64, $58, $58, $60, $54, $54, $5C,
    $50, $50, $58, $4C, $4C, $54, $48, $48, $4C, $44, $44, $4C, $40, $40, $44,
    $3C, $3C, $40, $38, $38, $3C, $34, $34, $38, $30, $30, $30, $2C, $2C, $30,
    $28, $28, $28, $20, $20, $20, $14, $14, $14, $00, $00, $00, $CC, $B4, $88,
    $C4, $AC, $80, $C0, $A8, $7C, $B8, $A0, $74, $B4, $98, $70, $AC, $94, $68,
    $A8, $8C, $64, $A0, $88, $5C, $9C, $80, $58, $94, $7C, $54, $90, $74, $50,
    $88, $70, $48, $84, $68, $44, $7C, $64, $40, $78, $60, $3C, $70, $58, $38,
    $6C, $54, $34, $64, $4C, $30, $60, $48, $2C, $58, $44, $28, $54, $3C, $24,
    $4C, $38, $20, $48, $34, $1C, $40, $30, $18, $3C, $2C, $14, $38, $24, $14,
    $30, $20, $10, $2C, $1C, $0C, $24, $18, $08, $20, $14, $08, $18, $10, $04,
    $14, $0C, $04, $54, $BC, $AC, $4C, $B0, $A0, $48, $A4, $90, $40, $98, $84,
    $38, $8C, $78, $34, $84, $6C, $2C, $78, $60, $28, $6C, $58, $24, $60, $4C,
    $1C, $54, $40, $18, $4C, $38, $14, $40, $2C, $10, $34, $24, $0C, $28, $1C,
    $08, $1C, $14, $04, $14, $0C, $80, $E8, $64, $74, $D8, $58, $68, $C4, $4C,
    $60, $B4, $44, $54, $A8, $3C, $4C, $98, $34, $44, $88, $2C, $3C, $7C, $24,
    $34, $70, $1C, $2C, $60, $18, $24, $50, $10, $20, $44, $10, $18, $38, $0C,
    $10, $2C, $08, $0C, $20, $04, $08, $14, $04, $FC, $FC, $FC, $FC, $FC, $D0,
    $FC, $FC, $A4, $FC, $FC, $7C, $FC, $FC, $50, $FC, $FC, $24, $FC, $FC, $00,
    $FC, $E8, $00, $F0, $C8, $00, $E4, $B0, $00, $D8, $94, $00, $D0, $7C, $00,
    $C4, $68, $00, $B8, $54, $00, $AC, $40, $00, $A4, $30, $00, $B4, $B8, $FC,
    $A8, $A4, $FC, $8C, $94, $F4, $68, $70, $F4, $58, $5C, $EC, $48, $48, $E4,
    $3C, $38, $DC, $2C, $24, $D4, $1C, $10, $CC, $1C, $08, $C4, $18, $00, $B8,
    $14, $00, $9C, $10, $00, $80, $08, $00, $60, $04, $00, $44, $00, $00, $28,
    $EC, $D8, $D8, $E0, $CC, $CC, $D4, $C0, $C0, $C8, $B4, $B4, $BC, $A8, $A8,
    $B0, $9C, $9C, $A4, $90, $90, $98, $84, $84, $FC, $F4, $78, $F8, $D4, $60,
    $E4, $B8, $4C, $D4, $9C, $3C, $C0, $80, $2C, $B0, $64, $20, $9C, $4C, $14,
    $7C, $30, $10, $A0, $9C, $64, $98, $94, $60, $90, $8C, $58, $84, $80, $54,
    $7C, $78, $4C, $74, $70, $48, $6C, $68, $40, $64, $60, $3C, $58, $54, $34,
    $50, $4C, $30, $48, $44, $28, $40, $3C, $24, $38, $34, $1C, $2C, $28, $18,
    $24, $20, $10, $1C, $18, $0C, $FC, $00, $FC, $E4, $00, $E4, $CC, $00, $CC,
    $B4, $00, $B4, $98, $00, $9C, $80, $00, $84, $68, $00, $6C, $50, $00, $54,
    $FC, $E4, $E4, $FC, $D4, $C4, $FC, $C0, $A8, $FC, $B4, $8C, $FC, $A0, $70,
    $FC, $94, $54, $FC, $80, $38, $FC, $74, $18, $F0, $68, $18, $E8, $64, $10,
    $DC, $5C, $10, $D8, $58, $0C, $CC, $50, $08, $C4, $48, $00, $BC, $40, $00,
    $B4, $3C, $00, $AC, $38, $00, $A0, $34, $00, $98, $30, $00, $8C, $2C, $00,
    $84, $28, $00, $78, $24, $00, $70, $20, $00, $64, $1C, $00, $F0, $BC, $BC,
    $F0, $AC, $AC, $F4, $9C, $9C, $F4, $8C, $8C, $F4, $7C, $7C, $F4, $6C, $6C,
    $F8, $60, $60, $F8, $50, $50, $F8, $40, $40, $F8, $30, $30, $FC, $20, $20,
    $F0, $20, $20, $E0, $1C, $1C, $D4, $1C, $1C, $C4, $18, $18, $B8, $18, $18,
    $A8, $14, $14, $9C, $14, $14, $8C, $10, $10, $80, $10, $10, $70, $0C, $0C,
    $64, $0C, $0C, $54, $08, $08, $48, $08, $08, $38, $04, $04, $2C, $04, $04,
    $1C, $00, $00, $10, $00, $00, $84, $58, $58, $A0, $38, $00, $84, $58, $58,
    $FC, $F8, $FC
  );

  DoomPaletteRaw: rawpalette_t = (
    $00, $00, $00, $1F, $17, $0B, $17, $0F, $07, $4B, $4B, $4B, $FF, $FF, $FF,
    $1B, $1B, $1B, $13, $13, $13, $0B, $0B, $0B, $07, $07, $07, $2F, $37, $1F,
    $23, $2B, $0F, $17, $1F, $07, $0F, $17, $00, $4F, $3B, $2B, $47, $33, $23,
    $3F, $2B, $1B, $FF, $B7, $B7, $F7, $AB, $AB, $F3, $A3, $A3, $EB, $97, $97,
    $E7, $8F, $8F, $DF, $87, $87, $DB, $7B, $7B, $D3, $73, $73, $CB, $6B, $6B,
    $C7, $63, $63, $BF, $5B, $5B, $BB, $57, $57, $B3, $4F, $4F, $AF, $47, $47,
    $A7, $3F, $3F, $A3, $3B, $3B, $9B, $33, $33, $97, $2F, $2F, $8F, $2B, $2B,
    $8B, $23, $23, $83, $1F, $1F, $7F, $1B, $1B, $77, $17, $17, $73, $13, $13,
    $6B, $0F, $0F, $67, $0B, $0B, $5F, $07, $07, $5B, $07, $07, $53, $07, $07,
    $4F, $00, $00, $47, $00, $00, $43, $00, $00, $FF, $EB, $DF, $FF, $E3, $D3,
    $FF, $DB, $C7, $FF, $D3, $BB, $FF, $CF, $B3, $FF, $C7, $A7, $FF, $BF, $9B,
    $FF, $BB, $93, $FF, $B3, $83, $F7, $AB, $7B, $EF, $A3, $73, $E7, $9B, $6B,
    $DF, $93, $63, $D7, $8B, $5B, $CF, $83, $53, $CB, $7F, $4F, $BF, $7B, $4B,
    $B3, $73, $47, $AB, $6F, $43, $A3, $6B, $3F, $9B, $63, $3B, $8F, $5F, $37,
    $87, $57, $33, $7F, $53, $2F, $77, $4F, $2B, $6B, $47, $27, $5F, $43, $23,
    $53, $3F, $1F, $4B, $37, $1B, $3F, $2F, $17, $33, $2B, $13, $2B, $23, $0F,
    $EF, $EF, $EF, $E7, $E7, $E7, $DF, $DF, $DF, $DB, $DB, $DB, $D3, $D3, $D3,
    $CB, $CB, $CB, $C7, $C7, $C7, $BF, $BF, $BF, $B7, $B7, $B7, $B3, $B3, $B3,
    $AB, $AB, $AB, $A7, $A7, $A7, $9F, $9F, $9F, $97, $97, $97, $93, $93, $93,
    $8B, $8B, $8B, $83, $83, $83, $7F, $7F, $7F, $77, $77, $77, $6F, $6F, $6F,
    $6B, $6B, $6B, $63, $63, $63, $5B, $5B, $5B, $57, $57, $57, $4F, $4F, $4F,
    $47, $47, $47, $43, $43, $43, $3B, $3B, $3B, $37, $37, $37, $2F, $2F, $2F,
    $27, $27, $27, $23, $23, $23, $77, $FF, $6F, $6F, $EF, $67, $67, $DF, $5F,
    $5F, $CF, $57, $5B, $BF, $4F, $53, $AF, $47, $4B, $9F, $3F, $43, $93, $37,
    $3F, $83, $2F, $37, $73, $2B, $2F, $63, $23, $27, $53, $1B, $1F, $43, $17,
    $17, $33, $0F, $13, $23, $0B, $0B, $17, $07, $BF, $A7, $8F, $B7, $9F, $87,
    $AF, $97, $7F, $A7, $8F, $77, $9F, $87, $6F, $9B, $7F, $6B, $93, $7B, $63,
    $8B, $73, $5B, $83, $6B, $57, $7B, $63, $4F, $77, $5F, $4B, $6F, $57, $43,
    $67, $53, $3F, $5F, $4B, $37, $57, $43, $33, $53, $3F, $2F, $9F, $83, $63,
    $8F, $77, $53, $83, $6B, $4B, $77, $5F, $3F, $67, $53, $33, $5B, $47, $2B,
    $4F, $3B, $23, $43, $33, $1B, $7B, $7F, $63, $6F, $73, $57, $67, $6B, $4F,
    $5B, $63, $47, $53, $57, $3B, $47, $4F, $33, $3F, $47, $2B, $37, $3F, $27,
    $FF, $FF, $73, $EB, $DB, $57, $D7, $BB, $43, $C3, $9B, $2F, $AF, $7B, $1F,
    $9B, $5B, $13, $87, $43, $07, $73, $2B, $00, $FF, $FF, $FF, $FF, $DB, $DB,
    $FF, $BB, $BB, $FF, $9B, $9B, $FF, $7B, $7B, $FF, $5F, $5F, $FF, $3F, $3F,
    $FF, $1F, $1F, $FF, $00, $00, $EF, $00, $00, $E3, $00, $00, $D7, $00, $00,
    $CB, $00, $00, $BF, $00, $00, $B3, $00, $00, $A7, $00, $00, $9B, $00, $00,
    $8B, $00, $00, $7F, $00, $00, $73, $00, $00, $67, $00, $00, $5B, $00, $00,
    $4F, $00, $00, $43, $00, $00, $E7, $E7, $FF, $C7, $C7, $FF, $AB, $AB, $FF,
    $8F, $8F, $FF, $73, $73, $FF, $53, $53, $FF, $37, $37, $FF, $1B, $1B, $FF,
    $00, $00, $FF, $00, $00, $E3, $00, $00, $CB, $00, $00, $B3, $00, $00, $9B,
    $00, $00, $83, $00, $00, $6B, $00, $00, $53, $FF, $FF, $FF, $FF, $EB, $DB,
    $FF, $D7, $BB, $FF, $C7, $9B, $FF, $B3, $7B, $FF, $A3, $5B, $FF, $8F, $3B,
    $FF, $7F, $1B, $F3, $73, $17, $EB, $6F, $0F, $DF, $67, $0F, $D7, $5F, $0B,
    $CB, $57, $07, $C3, $4F, $00, $B7, $47, $00, $AF, $43, $00, $FF, $FF, $FF,
    $FF, $FF, $D7, $FF, $FF, $B3, $FF, $FF, $8F, $FF, $FF, $6B, $FF, $FF, $47,
    $FF, $FF, $23, $FF, $FF, $00, $A7, $3F, $00, $9F, $37, $00, $93, $2F, $00,
    $87, $23, $00, $4F, $3B, $27, $43, $2F, $1B, $37, $23, $13, $2F, $1B, $0B,
    $00, $00, $53, $00, $00, $47, $00, $00, $3B, $00, $00, $2F, $00, $00, $23,
    $00, $00, $17, $00, $00, $0B, $00, $00, $00, $FF, $9F, $43, $FF, $E7, $4B,
    $FF, $7B, $FF, $FF, $00, $FF, $CF, $00, $CF, $9F, $00, $9B, $6F, $00, $6B,
    $A7, $6B, $6B
  );

  HereticPaletteRaw: rawpalette_t = (
    $02, $02, $02, $02, $02, $02, $10, $10, $10, $18, $18, $18, $1F, $1F, $1F,
    $24, $24, $24, $2C, $2C, $2C, $30, $30, $30, $37, $37, $37, $3F, $3F, $3F,
    $46, $46, $46, $4E, $4E, $4E, $56, $56, $56, $5D, $5D, $5D, $65, $65, $65,
    $6C, $6C, $6C, $74, $74, $74, $7C, $7C, $7C, $83, $83, $83, $8B, $8B, $8B,
    $92, $92, $92, $9A, $9A, $9A, $A2, $A2, $A2, $A9, $A9, $A9, $B1, $B1, $B1,
    $B8, $B8, $B8, $C0, $C0, $C0, $C8, $C8, $C8, $CF, $CF, $CF, $D2, $D2, $D2,
    $D7, $D7, $D7, $DE, $DE, $DE, $E4, $E4, $E4, $EC, $EC, $EC, $F5, $F5, $F5,
    $FF, $FF, $FF, $32, $32, $32, $3B, $3C, $3B, $45, $48, $44, $4E, $50, $4D,
    $58, $5D, $56, $61, $64, $5F, $6D, $70, $68, $74, $7B, $70, $7D, $83, $79,
    $86, $8D, $82, $90, $97, $8B, $99, $A1, $94, $A3, $AB, $9D, $AC, $B5, $A6,
    $B5, $BD, $B0, $BD, $C4, $B9, $14, $10, $24, $18, $18, $2C, $24, $24, $3C,
    $34, $34, $50, $44, $44, $60, $58, $58, $74, $6C, $6C, $88, $7C, $7C, $98,
    $94, $94, $AC, $A4, $A4, $B8, $B4, $B8, $C8, $C0, $C4, $D0, $D0, $D0, $D8,
    $E0, $E0, $E0, $1B, $0F, $08, $26, $14, $0B, $31, $1B, $0E, $3D, $1F, $0E,
    $41, $23, $12, $4A, $25, $13, $53, $2B, $13, $57, $2F, $17, $5F, $33, $1B,
    $67, $3B, $1F, $73, $43, $23, $7B, $4B, $27, $83, $53, $2F, $8F, $5B, $33,
    $97, $63, $3B, $A0, $6C, $40, $AF, $74, $4A, $B4, $7E, $51, $C0, $87, $5B,
    $CC, $8F, $5D, $D5, $97, $67, $D8, $9F, $73, $DC, $A7, $7E, $DF, $AF, $8A,
    $E3, $B7, $95, $E6, $BE, $A1, $E9, $C6, $AC, $ED, $CE, $B8, $F0, $D6, $C3,
    $3E, $28, $0B, $4B, $32, $10, $54, $3B, $17, $5F, $43, $1E, $67, $4B, $26,
    $6E, $53, $2F, $7B, $5F, $37, $89, $6B, $3E, $96, $76, $4B, $A3, $81, $54,
    $AB, $89, $5C, $B4, $92, $65, $BC, $9A, $6D, $C4, $A2, $75, $CC, $AA, $7D,
    $D0, $B0, $85, $25, $14, $04, $2F, $18, $04, $39, $1C, $06, $44, $21, $04,
    $4C, $24, $03, $54, $28, $00, $61, $2F, $02, $72, $36, $00, $7D, $3F, $06,
    $8D, $4B, $09, $9B, $53, $11, $A2, $5F, $15, $A9, $67, $1A, $B4, $71, $20,
    $BC, $7C, $14, $CC, $88, $18, $DC, $94, $1C, $EC, $A0, $17, $F4, $AC, $2F,
    $FC, $BB, $39, $FC, $C2, $46, $FB, $C9, $53, $FB, $D0, $61, $FB, $D6, $6E,
    $FB, $DD, $7B, $FA, $E4, $88, $9D, $33, $04, $AA, $41, $02, $B9, $56, $04,
    $D5, $76, $04, $EC, $A4, $03, $F8, $BE, $03, $FF, $D8, $2B, $FF, $FF, $00,
    $43, $00, $00, $4F, $00, $00, $5B, $00, $00, $67, $00, $00, $73, $00, $00,
    $7F, $00, $00, $8B, $00, $00, $9B, $00, $00, $A7, $00, $00, $B3, $00, $00,
    $BF, $00, $00, $CB, $00, $00, $D7, $00, $00, $E3, $00, $00, $EF, $00, $00,
    $FF, $00, $00, $FF, $34, $34, $FF, $4A, $4A, $FF, $5F, $5F, $FF, $7B, $7B,
    $FF, $9B, $9B, $FF, $B3, $B3, $FF, $C9, $C9, $FF, $D7, $D7, $3C, $0C, $58,
    $50, $08, $6C, $68, $08, $80, $80, $00, $90, $98, $00, $B0, $B8, $00, $E0,
    $D8, $2C, $FC, $E0, $78, $F0, $25, $06, $81, $3C, $21, $93, $52, $3D, $A5,
    $69, $58, $B7, $80, $74, $C9, $97, $8F, $DB, $AD, $AB, $ED, $C4, $C6, $FF,
    $02, $04, $29, $02, $05, $31, $06, $08, $39, $02, $05, $41, $02, $05, $4F,
    $00, $04, $58, $00, $04, $60, $00, $04, $68, $02, $05, $79, $02, $05, $89,
    $06, $09, $9F, $0C, $10, $B8, $20, $28, $C8, $38, $3C, $DC, $50, $50, $FD,
    $50, $6C, $FC, $50, $88, $FC, $50, $A4, $FC, $50, $C4, $FC, $48, $DC, $FC,
    $50, $EC, $FC, $54, $FC, $FC, $98, $FC, $FC, $BC, $FC, $F4, $0B, $17, $07,
    $13, $23, $0B, $17, $33, $0F, $1F, $43, $17, $27, $53, $1B, $2F, $63, $23,
    $37, $73, $2B, $3F, $83, $2F, $43, $93, $37, $4B, $9F, $3F, $53, $AF, $47,
    $5B, $BF, $4F, $5F, $CF, $57, $67, $DF, $5F, $6F, $EF, $67, $77, $FF, $6F,
    $17, $1F, $17, $1B, $23, $1B, $1F, $2B, $1F, $23, $33, $23, $2B, $37, $2B,
    $2F, $3F, $2F, $33, $47, $33, $3B, $4B, $37, $3F, $53, $3B, $43, $5B, $43,
    $4B, $5F, $47, $4F, $67, $4B, $57, $6F, $4F, $5B, $73, $53, $5F, $7B, $57,
    $67, $83, $5F, $FF, $DF, $00, $FF, $BF, $00, $FF, $9F, $00, $FF, $7F, $00,
    $FF, $5F, $00, $FF, $3F, $00, $F4, $0E, $03, $37, $00, $00, $2F, $00, $00,
    $27, $00, $00, $17, $00, $00, $0F, $0F, $0F, $0B, $0B, $0B, $07, $07, $07,
    $FF, $FF, $FF
  );

  HexenPaletteRaw: rawpalette_t = (
    $02, $02, $02, $04, $04, $04, $0F, $0F, $0F, $13, $13, $13, $1B, $1B, $1B,
    $1C, $1C, $1C, $21, $21, $21, $27, $27, $27, $2D, $2D, $2D, $33, $33, $33,
    $39, $39, $39, $3F, $3F, $3F, $45, $45, $45, $4B, $4B, $4B, $51, $51, $51,
    $56, $56, $56, $5C, $5C, $5C, $62, $62, $62, $68, $68, $68, $70, $70, $70,
    $79, $79, $79, $82, $82, $82, $8B, $8B, $8B, $93, $93, $93, $9D, $9D, $9D,
    $A6, $A6, $A6, $B0, $B0, $B0, $B9, $B9, $B9, $C2, $C2, $C2, $CB, $CB, $CB,
    $D4, $D4, $D4, $DD, $DD, $DD, $E6, $E6, $E6, $1D, $20, $1D, $26, $28, $25,
    $32, $32, $32, $3B, $3C, $3B, $45, $48, $44, $4E, $50, $4D, $58, $5D, $56,
    $61, $64, $5F, $6D, $70, $68, $74, $7B, $70, $7D, $83, $79, $86, $8D, $82,
    $90, $97, $8B, $99, $A1, $94, $A3, $AB, $9D, $AC, $B5, $A6, $B5, $BD, $B0,
    $BD, $C4, $B9, $16, $1D, $16, $1B, $24, $1B, $1F, $2B, $1F, $23, $33, $23,
    $2B, $37, $2B, $2F, $3F, $2F, $33, $47, $33, $3B, $4B, $37, $3F, $53, $3B,
    $43, $5B, $43, $4B, $5F, $47, $4F, $67, $4B, $57, $6F, $4F, $5B, $73, $53,
    $5F, $7B, $57, $67, $83, $5F, $14, $10, $24, $1E, $1A, $2E, $28, $24, $39,
    $32, $2E, $43, $3B, $39, $4E, $45, $43, $58, $4F, $4D, $63, $59, $57, $6D,
    $63, $61, $78, $6D, $6B, $82, $76, $76, $8D, $80, $80, $97, $8A, $8A, $A2,
    $94, $94, $AC, $3E, $28, $0B, $4B, $32, $10, $54, $3B, $17, $5F, $43, $1E,
    $67, $4B, $26, $6E, $53, $2F, $7B, $5F, $37, $89, $6B, $3E, $96, $76, $4B,
    $A3, $81, $54, $AB, $89, $5C, $B4, $92, $65, $BC, $9A, $6D, $C4, $A2, $75,
    $CC, $AA, $7D, $D0, $B0, $85, $1B, $0F, $08, $26, $14, $0B, $31, $1B, $0E,
    $3D, $1F, $0E, $41, $23, $12, $4A, $25, $13, $53, $2B, $13, $57, $2F, $17,
    $5F, $33, $1B, $67, $3B, $1F, $73, $43, $23, $7B, $4B, $27, $83, $53, $2F,
    $8F, $5B, $33, $97, $63, $3B, $A0, $6C, $40, $AF, $74, $4A, $B4, $7E, $51,
    $C0, $87, $5B, $CC, $8F, $5D, $D5, $97, $67, $D8, $9F, $73, $DC, $A7, $7E,
    $DF, $AF, $8A, $E3, $B7, $95, $25, $14, $04, $2F, $18, $04, $39, $1C, $06,
    $44, $21, $04, $4C, $24, $03, $54, $28, $00, $61, $2F, $02, $72, $36, $00,
    $7D, $3F, $06, $8D, $4B, $09, $9B, $53, $11, $A2, $5F, $15, $A9, $67, $1A,
    $B4, $71, $20, $BC, $7C, $14, $CC, $88, $18, $DC, $94, $1C, $EC, $A0, $17,
    $F4, $AC, $2F, $FC, $BB, $39, $FC, $C2, $46, $FB, $C9, $53, $FB, $D0, $61,
    $FB, $DD, $7B, $02, $04, $29, $02, $05, $31, $06, $08, $39, $02, $05, $41,
    $02, $05, $4F, $00, $04, $58, $00, $04, $60, $00, $04, $68, $04, $06, $79,
    $02, $05, $89, $14, $17, $98, $26, $29, $A7, $38, $3B, $B5, $4A, $4D, $C4,
    $5B, $5E, $D3, $6D, $70, $E2, $7F, $82, $F0, $91, $94, $FF, $1F, $04, $04,
    $27, $00, $00, $2F, $00, $00, $37, $00, $00, $43, $00, $00, $4F, $00, $00,
    $5B, $00, $00, $67, $00, $00, $73, $00, $00, $7F, $00, $00, $8B, $00, $00,
    $9B, $00, $00, $A7, $00, $00, $B9, $00, $00, $CA, $00, $00, $DC, $00, $00,
    $ED, $00, $00, $FF, $00, $00, $FF, $2E, $2E, $FF, $5B, $5B, $FF, $89, $89,
    $FF, $AB, $AB, $14, $10, $04, $0D, $18, $09, $11, $21, $0C, $15, $29, $0E,
    $18, $32, $11, $1C, $39, $14, $20, $41, $18, $23, $49, $1C, $27, $50, $1F,
    $2C, $56, $25, $2E, $5F, $26, $33, $68, $2B, $3C, $7A, $33, $44, $8B, $3A,
    $4D, $9D, $42, $55, $AE, $49, $5E, $C0, $51, $9D, $33, $04, $AA, $41, $02,
    $B9, $56, $04, $D5, $77, $06, $EA, $93, $05, $FF, $B2, $06, $FF, $C3, $1A,
    $FF, $D8, $2D, $04, $85, $04, $08, $AF, $08, $02, $D7, $02, $03, $EA, $03,
    $2A, $FC, $2A, $79, $FF, $79, $03, $03, $B8, $0F, $29, $DC, $1C, $50, $E2,
    $29, $77, $E9, $36, $9E, $EF, $43, $C5, $F6, $50, $EC, $FC, $F4, $0E, $03,
    $FF, $3F, $00, $FF, $5F, $00, $FF, $7F, $00, $FF, $9F, $00, $FF, $C3, $1A,
    $FF, $DF, $00, $2B, $0D, $40, $3D, $0E, $59, $5A, $0F, $7A, $78, $10, $9C,
    $95, $10, $BD, $B2, $11, $DE, $C5, $4A, $E8, $D7, $81, $F3, $EA, $A9, $FD,
    $3D, $10, $10, $5A, $24, $21, $76, $38, $31, $93, $4D, $42, $B0, $61, $53,
    $CC, $75, $63, $47, $35, $02, $51, $3F, $06, $60, $48, $00, $6C, $50, $00,
    $78, $58, $00, $80, $60, $00, $95, $70, $01, $B5, $88, $03, $D4, $A0, $04,
    $FF, $FF, $FF
  );

  StrifePaletteRaw: rawpalette_t = (
    $00, $00, $00, $E7, $E3, $E3, $DF, $DB, $DB, $D7, $D3, $D3, $CF, $CB, $CB,
    $C7, $C3, $C3, $BF, $BF, $BF, $B7, $B7, $B7, $B3, $AF, $AF, $AB, $A7, $A7,
    $A3, $9F, $9F, $9B, $97, $97, $93, $93, $93, $8B, $8B, $8B, $83, $83, $83,
    $7B, $7B, $7B, $77, $73, $73, $6F, $6F, $6F, $67, $67, $67, $5F, $5F, $5F,
    $57, $57, $57, $4F, $4F, $4F, $47, $47, $47, $43, $3F, $3F, $3B, $3B, $3B,
    $33, $33, $33, $2B, $2B, $2B, $23, $23, $23, $1B, $1B, $1B, $13, $13, $13,
    $0B, $0B, $0B, $07, $07, $07, $BB, $BF, $B7, $B3, $B7, $AB, $A7, $B3, $9F,
    $A3, $AB, $93, $9B, $A7, $8B, $93, $9F, $7F, $8B, $9B, $77, $83, $93, $6B,
    $7F, $8F, $67, $77, $87, $5B, $73, $83, $53, $6B, $7B, $4B, $67, $77, $43,
    $63, $6F, $3F, $5B, $6B, $37, $57, $63, $2F, $53, $5F, $2B, $4B, $57, $23,
    $47, $53, $1F, $43, $4B, $1B, $3F, $47, $17, $3B, $3F, $13, $33, $3B, $0F,
    $2F, $33, $0B, $2B, $2F, $07, $27, $2B, $07, $1F, $23, $07, $1B, $1F, $00,
    $17, $17, $00, $0F, $13, $00, $0B, $0B, $00, $07, $07, $00, $DB, $2B, $2B,
    $CB, $23, $23, $BF, $1F, $1F, $AF, $1B, $1B, $A3, $17, $17, $93, $13, $13,
    $87, $0F, $0F, $77, $0B, $0B, $6B, $07, $07, $5B, $07, $07, $4F, $00, $00,
    $3F, $00, $00, $33, $00, $00, $27, $00, $00, $17, $00, $00, $0B, $00, $00,
    $EB, $E7, $00, $E7, $D3, $00, $D7, $B3, $00, $C7, $97, $00, $B7, $7F, $00,
    $A7, $67, $00, $97, $53, $00, $87, $3F, $00, $77, $2F, $00, $67, $23, $00,
    $57, $17, $00, $47, $0B, $00, $37, $07, $00, $27, $00, $00, $17, $00, $00,
    $0B, $00, $00, $B7, $E7, $7F, $A3, $D7, $6F, $8F, $C7, $5F, $7F, $B7, $4F,
    $6B, $AB, $43, $5B, $9B, $37, $4B, $8B, $2B, $3F, $7B, $23, $2F, $6F, $1B,
    $23, $5F, $13, $17, $4F, $0B, $0F, $43, $07, $07, $33, $07, $00, $23, $00,
    $00, $13, $00, $00, $07, $00, $C7, $CF, $FF, $B7, $BB, $EF, $A3, $AB, $DB,
    $97, $9B, $CB, $87, $8B, $BB, $7B, $7F, $AB, $6B, $6F, $9B, $5F, $63, $8B,
    $53, $53, $7B, $43, $47, $6B, $37, $3B, $5B, $2F, $2F, $4B, $23, $23, $3B,
    $17, $17, $2B, $0F, $0F, $1B, $00, $00, $0B, $C7, $BF, $93, $B3, $AB, $83,
    $A7, $9B, $77, $9B, $8B, $6F, $8F, $7F, $63, $83, $6F, $5B, $77, $63, $4F,
    $6B, $57, $47, $5B, $47, $3B, $4F, $3B, $33, $43, $2F, $2B, $37, $27, $23,
    $2B, $1B, $1B, $1F, $13, $13, $13, $0B, $0B, $07, $07, $00, $8F, $C3, $D3,
    $7B, $B3, $C3, $6B, $A7, $B7, $5B, $9B, $A7, $4B, $8B, $9B, $3B, $7F, $8B,
    $2F, $73, $7F, $23, $67, $73, $1B, $5B, $63, $13, $4F, $57, $0B, $43, $47,
    $07, $37, $3B, $00, $2B, $2B, $00, $1F, $1F, $00, $13, $13, $00, $07, $07,
    $D3, $BF, $AF, $CB, $B3, $A3, $C3, $AB, $97, $BF, $9F, $8F, $B7, $97, $83,
    $AF, $8F, $7B, $AB, $87, $73, $A3, $7B, $67, $9B, $73, $5F, $97, $6B, $57,
    $8F, $63, $4F, $8B, $5B, $47, $83, $53, $43, $7B, $4B, $3B, $77, $43, $33,
    $6F, $3B, $2F, $67, $37, $27, $63, $2F, $23, $5B, $2B, $1F, $53, $23, $1B,
    $4F, $1F, $17, $47, $1B, $13, $3F, $13, $0F, $3B, $0F, $0B, $33, $0B, $07,
    $2B, $07, $07, $27, $07, $00, $1F, $00, $00, $1B, $00, $00, $13, $00, $00,
    $0B, $00, $00, $07, $00, $00, $D3, $C7, $BB, $CB, $BF, $B3, $C3, $B7, $AB,
    $BF, $AF, $A3, $B7, $A7, $9B, $AF, $9F, $93, $AB, $97, $8B, $A3, $8F, $87,
    $9B, $8B, $7F, $97, $83, $77, $8F, $7B, $6F, $87, $73, $6B, $83, $6B, $63,
    $7B, $67, $5F, $73, $5F, $57, $6F, $57, $53, $67, $53, $4B, $5F, $4B, $47,
    $5B, $43, $3F, $53, $3F, $3B, $4F, $37, $33, $47, $33, $2F, $3F, $2B, $2B,
    $3B, $27, $27, $33, $23, $1F, $2B, $1B, $1B, $27, $17, $17, $1F, $13, $13,
    $17, $0F, $0F, $13, $0B, $0B, $0B, $07, $07, $07, $07, $00, $EF, $EF, $00,
    $E7, $D7, $00, $E3, $BF, $00, $DB, $AB, $00, $D7, $97, $00, $D3, $83, $00,
    $CB, $6F, $00, $C7, $5B, $00, $BF, $4B, $00, $BB, $3B, $00, $B7, $2B, $00,
    $FF, $00, $00, $DF, $00, $00, $BF, $00, $00, $9F, $00, $00, $7F, $00, $00,
    $00, $00, $00, $8B, $C7, $67, $6B, $AB, $4B, $4F, $8F, $37, $37, $73, $23,
    $23, $57, $13, $13, $3F, $0B, $D7, $DF, $FF, $BB, $CB, $F7, $8F, $A7, $DB,
    $63, $83, $C3, $3F, $5B, $A7, $CB, $CB, $CB, $D7, $D7, $D7, $DF, $DF, $DF,
    $EB, $EB, $EB
  );

  GLSpeedPaletteRaw: rawpalette_t = (
    $00, $00, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $00, $FC, $00, $FC, $E0, $C8, $EC, $C8, $B0, $E0, $B8, $9C, $D4, $A4, $84,
    $C8, $90, $74, $BC, $7C, $60, $B0, $6C, $50, $A4, $58, $44, $98, $48, $34,
    $88, $3C, $28, $7C, $2C, $1C, $70, $20, $14, $64, $14, $0C, $58, $08, $04,
    $4C, $04, $00, $40, $00, $00, $FC, $D8, $D8, $EC, $AC, $AC, $DC, $84, $84,
    $CC, $60, $60, $BC, $40, $40, $AC, $24, $24, $9C, $0C, $0C, $8C, $00, $00,
    $FC, $A8, $5C, $FC, $98, $40, $FC, $88, $20, $FC, $78, $00, $E4, $6C, $00,
    $CC, $60, $00, $B4, $54, $00, $9C, $4C, $00, $FC, $FC, $D8, $F8, $F4, $C0,
    $F4, $F0, $AC, $F0, $E8, $94, $EC, $E4, $80, $E8, $DC, $6C, $E4, $D4, $58,
    $E0, $CC, $48, $CC, $B8, $38, $B8, $A8, $28, $A4, $98, $1C, $90, $88, $14,
    $7C, $74, $0C, $68, $64, $04, $54, $50, $00, $40, $40, $00, $E4, $E4, $B4,
    $D8, $D8, $A0, $CC, $CC, $90, $C0, $C0, $80, $B4, $B4, $70, $A8, $AC, $64,
    $9C, $A0, $58, $90, $94, $48, $84, $88, $3C, $78, $7C, $34, $70, $74, $28,
    $64, $68, $20, $58, $5C, $18, $4C, $50, $10, $40, $44, $0C, $38, $3C, $08,
    $B0, $CC, $00, $9C, $C0, $00, $8C, $B8, $00, $7C, $B0, $00, $6C, $A4, $00,
    $5C, $9C, $00, $50, $94, $00, $44, $88, $00, $38, $80, $00, $30, $78, $00,
    $24, $6C, $00, $1C, $64, $00, $14, $5C, $00, $0C, $50, $00, $08, $48, $00,
    $04, $40, $00, $EC, $F4, $F8, $D0, $E4, $EC, $B4, $D0, $DC, $A0, $C4, $D0,
    $88, $B8, $C4, $74, $AC, $B8, $60, $9C, $AC, $50, $94, $A0, $40, $88, $94,
    $30, $7C, $88, $24, $70, $7C, $18, $68, $70, $10, $5C, $64, $08, $54, $58,
    $00, $48, $4C, $00, $40, $40, $D8, $EC, $FC, $C0, $DC, $F0, $AC, $CC, $E8,
    $9C, $BC, $E0, $88, $A8, $D8, $78, $98, $D0, $68, $88, $C4, $58, $78, $BC,
    $4C, $68, $B4, $40, $58, $AC, $34, $48, $A4, $28, $38, $98, $1C, $28, $90,
    $14, $1C, $88, $0C, $10, $80, $04, $04, $78, $C4, $90, $68, $B8, $84, $5C,
    $B0, $7C, $50, $A4, $74, $44, $9C, $6C, $3C, $90, $64, $30, $88, $60, $28,
    $7C, $58, $20, $74, $50, $18, $68, $48, $14, $60, $44, $0C, $54, $3C, $08,
    $4C, $38, $04, $44, $30, $00, $38, $28, $00, $30, $24, $00, $FC, $F4, $DC,
    $F0, $E4, $C8, $E8, $D8, $B8, $E0, $C8, $A8, $D4, $BC, $98, $CC, $AC, $88,
    $C4, $9C, $78, $BC, $90, $6C, $B0, $84, $60, $A4, $78, $54, $98, $6C, $4C,
    $8C, $60, $44, $80, $54, $3C, $74, $4C, $30, $6C, $40, $2C, $60, $38, $24,
    $EC, $EC, $EC, $E4, $E4, $E4, $DC, $DC, $DC, $D4, $D4, $D4, $CC, $CC, $CC,
    $C4, $C4, $C4, $BC, $BC, $BC, $B4, $B4, $B4, $AC, $AC, $AC, $A4, $A4, $A4,
    $9C, $9C, $9C, $94, $94, $94, $8C, $8C, $8C, $88, $88, $88, $80, $80, $80,
    $78, $78, $78, $70, $70, $70, $68, $68, $68, $60, $60, $60, $58, $58, $58,
    $50, $50, $50, $48, $48, $48, $40, $40, $40, $38, $38, $38, $30, $30, $30,
    $28, $28, $28, $24, $24, $24, $1C, $1C, $1C, $14, $14, $14, $0C, $0C, $0C,
    $04, $04, $04, $00, $00, $00, $EC, $C8, $C8, $E0, $B0, $B0, $D8, $A0, $A0,
    $D0, $8C, $8C, $C8, $7C, $7C, $C0, $68, $68, $B4, $5C, $5C, $AC, $4C, $4C,
    $A4, $40, $40, $9C, $30, $30, $94, $24, $24, $88, $1C, $1C, $80, $10, $10,
    $78, $08, $08, $70, $04, $04, $68, $00, $00, $FC, $FC, $B8, $FC, $F4, $88,
    $FC, $E4, $58, $FC, $D0, $28, $FC, $B4, $00, $DC, $98, $00, $BC, $80, $00,
    $9C, $68, $00, $7C, $54, $00, $EC, $C8, $FC, $C8, $94, $E0, $AC, $6C, $C4,
    $90, $48, $A8, $74, $28, $8C, $5C, $14, $70, $44, $04, $54, $DC, $E0, $FC,
    $CC, $CC, $F0, $BC, $C0, $E4, $B0, $B0, $D8, $A4, $A0, $CC, $98, $94, $C4,
    $90, $88, $B8, $84, $7C, $AC, $7C, $70, $A0, $74, $64, $94, $6C, $58, $8C,
    $64, $50, $80, $5C, $44, $74, $54, $3C, $68, $4C, $34, $5C, $44, $2C, $54,
    $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00, $00, $FC, $00,
    $FC, $40, $FC
  );

function GetPaletteFromName(const spal: string): rawpalette_p;

procedure FF_RawPalette2DoomPalette(const rowpal: pointer; const palette: PDoomPalette);

function FF_FindAproxColorIndex(const pal: PDoomPalette; const c: LongWord;
  const start: integer = 0; const finish: integer = 255): integer;

implementation

uses
  Windows;

function GetPaletteFromName(const spal: string): rawpalette_p;
begin
  if spal = spalDOOM then
    Result := @DoomPaletteRaw
  else if spal = spalHERETIC then
    Result := @HereticPaletteRaw
  else if spal = spalHEXEN then
    Result := @HexenPaletteRaw
  else if spal = spalSTRIFE then
    Result := @StrifePaletteRaw
  else if spal = spalRADIX then
    Result := @RadixPaletteRaw
  else if spal = spalGLSPEED then
    Result := @GLSpeedPaletteRaw
  else
    Result := nil;
end;

procedure FF_RawPalette2DoomPalette(const rowpal: pointer; const palette: PDoomPalette);
var
  pb: PByte;
  i: integer;
  r, g, b: byte;
begin
  pb := rowpal;
  for i := 0 to 255 do
  begin
    b := pb^; inc(pb);
    g := pb^; inc(pb);
    r := pb^; inc(pb);
    palette[i] := RGB(r, g, b);
  end;
end;

function FF_FindAproxColorIndex(const pal: PDoomPalette; const c: LongWord;
  const start: integer = 0; const finish: integer = 255): integer;
var
  r, g, b: integer;
  rc, gc, bc: integer;
  dr, dg, db: integer;
  i: integer;
  cc: LongWord;
  dist: LongWord;
  mindist: LongWord;
begin
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  result := start;
  mindist := LongWord($ffffffff);
  for i := start to finish do
  begin
    cc := pal[i];
    rc := cc and $FF;
    gc := (cc shr 8) and $FF;
    bc := (cc shr 16) and $FF;
    dr := r - rc;
    dg := g - gc;
    db := b - bc;
    dist := dr * dr + dg * dg + db * db;
    if dist < mindist then
    begin
      result := i;
      if dist = 0 then
        exit
      else
        mindist := dist;
    end;
  end;
end;

end.

