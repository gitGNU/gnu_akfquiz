{
* chsys (unit)
*
* $Id: chconv.pas,v 1.4 2006/10/11 06:26:22 akf Exp $
*
* Copyright (c) 2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Tables taken from GNU LIBICONV Library
* Copyright (C) 1999-2001 Free Software Foundation, Inc.
*
* Environment: FreePascal or GNU-Pascal
*
* This file is part of AKFQuiz
*
* AKFQuiz is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* AKFQuiz is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA
*}

{$R+} 

{$IfDef FPC}
  {$Mode Delphi}
  {$Smartlink on}
  {$LongStrings on}
{$EndIf}

{ right-to-left chars: U+0590..U+07BF (?) }
{ 0590...05FF Hebrew
  0600...06FF Arabic
  0700...074F Syriac
  0750...077F Arabic Supplement
  0780...07BF Thaana
}

 
unit chconv;

interface

const MaxCharsPerLine = 2048;

type Unicode = Cardinal; { Large integer value - larger than 3 Bytes }

type UnicodeString = record
                       Length: integer;
		       content: array[1..MaxCharsPerLine] of Unicode
		       end;

type UnicodeEncoder = function(c: char): Unicode;

const 
  ISO2Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0104, $02d8, $0141, $00a4, $013d, $015a, $00a7,
  $00a8, $0160, $015e, $0164, $0179, $00ad, $017d, $017b,
  { $b0 }
  $00b0, $0105, $02db, $0142, $00b4, $013e, $015b, $02c7,
  $00b8, $0161, $015f, $0165, $017a, $02dd, $017e, $017c,
  { $c0 }
  $0154, $00c1, $00c2, $0102, $00c4, $0139, $0106, $00c7,
  $010c, $00c9, $0118, $00cb, $011a, $00cd, $00ce, $010e,
  { $d0 }
  $0110, $0143, $0147, $00d3, $00d4, $0150, $00d6, $00d7,
  $0158, $016e, $00da, $0170, $00dc, $00dd, $0162, $00df,
  { $e0 }
  $0155, $00e1, $00e2, $0103, $00e4, $013a, $0107, $00e7,
  $010d, $00e9, $0119, $00eb, $011b, $00ed, $00ee, $010f,
  { $f0 }
  $0111, $0144, $0148, $00f3, $00f4, $0151, $00f6, $00f7,
  $0159, $016f, $00fa, $0171, $00fc, $00fd, $0163, $02d9);

const 
  ISO3Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0126, $02d8, $00a3, $00a4, $fffd, $0124, $00a7,
  $00a8, $0130, $015e, $011e, $0134, $00ad, $fffd, $017b,
  { $b0 }
  $00b0, $0127, $00b2, $00b3, $00b4, $00b5, $0125, $00b7,
  $00b8, $0131, $015f, $011f, $0135, $00bd, $fffd, $017c,
  { $c0 }
  $00c0, $00c1, $00c2, $fffd, $00c4, $010a, $0108, $00c7,
  $00c8, $00c9, $00ca, $00cb, $00cc, $00cd, $00ce, $00cf,
  { $d0 }
  $fffd, $00d1, $00d2, $00d3, $00d4, $0120, $00d6, $00d7,
  $011c, $00d9, $00da, $00db, $00dc, $016c, $015c, $00df,
  { $e0 }
  $00e0, $00e1, $00e2, $fffd, $00e4, $010b, $0109, $00e7,
  $00e8, $00e9, $00ea, $00eb, $00ec, $00ed, $00ee, $00ef,
  { $f0 }
  $fffd, $00f1, $00f2, $00f3, $00f4, $0121, $00f6, $00f7,
  $011d, $00f9, $00fa, $00fb, $00fc, $016d, $015d, $02d9);

const 
  ISO4Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0104, $0138, $0156, $00a4, $0128, $013b, $00a7,
  $00a8, $0160, $0112, $0122, $0166, $00ad, $017d, $00af,
  { $b0 }
  $00b0, $0105, $02db, $0157, $00b4, $0129, $013c, $02c7,
  $00b8, $0161, $0113, $0123, $0167, $014a, $017e, $014b,
  { $c0 }
  $0100, $00c1, $00c2, $00c3, $00c4, $00c5, $00c6, $012e,
  $010c, $00c9, $0118, $00cb, $0116, $00cd, $00ce, $012a,
  { $d0 }
  $0110, $0145, $014c, $0136, $00d4, $00d5, $00d6, $00d7,
  $00d8, $0172, $00da, $00db, $00dc, $0168, $016a, $00df,
  { $e0 }
  $0101, $00e1, $00e2, $00e3, $00e4, $00e5, $00e6, $012f,
  $010d, $00e9, $0119, $00eb, $0117, $00ed, $00ee, $012b,
  { $f0 }
  $0111, $0146, $014d, $0137, $00f4, $00f5, $00f6, $00f7,
  $00f8, $0173, $00fa, $00fb, $00fc, $0169, $016b, $02d9);

const 
  ISO5Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0401, $0402, $0403, $0404, $0405, $0406, $0407,
  $0408, $0409, $040a, $040b, $040c, $00ad, $040e, $040f,
  { $b0 }
  $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
  $0418, $0419, $041a, $041b, $041c, $041d, $041e, $041f,
  { $c0 }
  $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
  $0428, $0429, $042a, $042b, $042c, $042d, $042e, $042f,
  { $d0 }
  $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
  $0438, $0439, $043a, $043b, $043c, $043d, $043e, $043f,
  { $e0 }
  $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
  $0448, $0449, $044a, $044b, $044c, $044d, $044e, $044f,
  { $f0 }
  $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457,
  $0458, $0459, $045a, $045b, $045c, $00a7, $045e, $045f);

const 
  ISO6Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $fffd, $fffd, $fffd, $00a4, $fffd, $fffd, $fffd,
  $fffd, $fffd, $fffd, $fffd, $060c, $00ad, $fffd, $fffd,
  { $b0 }
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd,
  $fffd, $fffd, $fffd, $061b, $fffd, $fffd, $fffd, $061f,
  { $c0 }
  $fffd, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
  $0628, $0629, $062a, $062b, $062c, $062d, $062e, $062f,
  { $d0 }
  $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637,
  $0638, $0639, $063a, $fffd, $fffd, $fffd, $fffd, $fffd,
  { $e0 }
  $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647,
  $0648, $0649, $064a, $064b, $064c, $064d, $064e, $064f,
  { $f0 }
  $0650, $0651, $0652, $fffd, $fffd, $fffd, $fffd, $fffd,
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd);

const 
  ISO7Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $2018, $2019, $00a3, $fffd, $fffd, $00a6, $00a7,
  $00a8, $00a9, $fffd, $00ab, $00ac, $00ad, $fffd, $2015,
  { $b0 }
  $00b0, $00b1, $00b2, $00b3, $0384, $0385, $0386, $00b7,
  $0388, $0389, $038a, $00bb, $038c, $00bd, $038e, $038f,
  { $c0 }
  $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
  $0398, $0399, $039a, $039b, $039c, $039d, $039e, $039f,
  { $d0 }
  $03a0, $03a1, $fffd, $03a3, $03a4, $03a5, $03a6, $03a7,
  $03a8, $03a9, $03aa, $03ab, $03ac, $03ad, $03ae, $03af,
  { $e0 }
  $03b0, $03b1, $03b2, $03b3, $03b4, $03b5, $03b6, $03b7,
  $03b8, $03b9, $03ba, $03bb, $03bc, $03bd, $03be, $03bf,
  { $f0 }
  $03c0, $03c1, $03c2, $03c3, $03c4, $03c5, $03c6, $03c7,
  $03c8, $03c9, $03ca, $03cb, $03cc, $03cd, $03ce, $fffd);

const 
  ISO8Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $fffd, $00a2, $00a3, $00a4, $00a5, $00a6, $00a7,
  $00a8, $00a9, $00d7, $00ab, $00ac, $00ad, $00ae, $00af,
  { $b0 }
  $00b0, $00b1, $00b2, $00b3, $00b4, $00b5, $00b6, $00b7,
  $00b8, $00b9, $00f7, $00bb, $00bc, $00bd, $00be, $fffd,
  { $c0 }
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd,
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd,
  { $d0 }
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd,
  $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $fffd, $2017,
  { $e0 }
  $05d0, $05d1, $05d2, $05d3, $05d4, $05d5, $05d6, $05d7,
  $05d8, $05d9, $05da, $05db, $05dc, $05dd, $05de, $05df,
  { $f0 }
  $05e0, $05e1, $05e2, $05e3, $05e4, $05e5, $05e6, $05e7,
  $05e8, $05e9, $05ea, $fffd, $fffd, $200e, $200f, $fffd);

const 
  ISO9Unicode: array[#$D0..#$FF] of Unicode = (
  { $d0 }
  $011e, $00d1, $00d2, $00d3, $00d4, $00d5, $00d6, $00d7,
  $00d8, $00d9, $00da, $00db, $00dc, $0130, $015e, $00df,
  { $e0 }
  $00e0, $00e1, $00e2, $00e3, $00e4, $00e5, $00e6, $00e7,
  $00e8, $00e9, $00ea, $00eb, $00ec, $00ed, $00ee, $00ef,
  { $f0 }
  $011f, $00f1, $00f2, $00f3, $00f4, $00f5, $00f6, $00f7,
  $00f8, $00f9, $00fa, $00fb, $00fc, $0131, $015f, $00ff);

const 
  ISO10Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0104, $0112, $0122, $012a, $0128, $0136, $00a7,
  $013b, $0110, $0160, $0166, $017d, $00ad, $016a, $014a,
  { $b0 }
  $00b0, $0105, $0113, $0123, $012b, $0129, $0137, $00b7,
  $013c, $0111, $0161, $0167, $017e, $2015, $016b, $014b,
  { $c0 }
  $0100, $00c1, $00c2, $00c3, $00c4, $00c5, $00c6, $012e,
  $010c, $00c9, $0118, $00cb, $0116, $00cd, $00ce, $00cf,
  { $d0 }
  $00d0, $0145, $014c, $00d3, $00d4, $00d5, $00d6, $0168,
  $00d8, $0172, $00da, $00db, $00dc, $00dd, $00de, $00df,
  { $e0 }
  $0101, $00e1, $00e2, $00e3, $00e4, $00e5, $00e6, $012f,
  $010d, $00e9, $0119, $00eb, $0117, $00ed, $00ee, $00ef,
  { $f0 }
  $00f0, $0146, $014d, $00f3, $00f4, $00f5, $00f6, $0169,
  $00f8, $0173, $00fa, $00fb, $00fc, $00fd, $00fe, $0138);

const 
  ISO13Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $201d, $00a2, $00a3, $00a4, $201e, $00a6, $00a7,
  $00d8, $00a9, $0156, $00ab, $00ac, $00ad, $00ae, $00c6,
  { $b0 }
  $00b0, $00b1, $00b2, $00b3, $201c, $00b5, $00b6, $00b7,
  $00f8, $00b9, $0157, $00bb, $00bc, $00bd, $00be, $00e6,
  { $c0 }
  $0104, $012e, $0100, $0106, $00c4, $00c5, $0118, $0112,
  $010c, $00c9, $0179, $0116, $0122, $0136, $012a, $013b,
  { $d0 }
  $0160, $0143, $0145, $00d3, $014c, $00d5, $00d6, $00d7,
  $0172, $0141, $015a, $016a, $00dc, $017b, $017d, $00df,
  { $e0 }
  $0105, $012f, $0101, $0107, $00e4, $00e5, $0119, $0113,
  $010d, $00e9, $017a, $0117, $0123, $0137, $012b, $013c,
  { $f0 }
  $0161, $0144, $0146, $00f3, $014d, $00f5, $00f6, $00f7,
  $0173, $0142, $015b, $016b, $00fc, $017c, $017e, $2019);

const 
  ISO14Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $1e02, $1e03, $00a3, $010a, $010b, $1e0a, $00a7,
  $1e80, $00a9, $1e82, $1e0b, $1ef2, $00ad, $00ae, $0178,
  { $b0 }
  $1e1e, $1e1f, $0120, $0121, $1e40, $1e41, $00b6, $1e56,
  $1e81, $1e57, $1e83, $1e60, $1ef3, $1e84, $1e85, $1e61,
  { $c0 }
  $00c0, $00c1, $00c2, $00c3, $00c4, $00c5, $00c6, $00c7,
  $00c8, $00c9, $00ca, $00cb, $00cc, $00cd, $00ce, $00cf,
  { $d0 }
  $0174, $00d1, $00d2, $00d3, $00d4, $00d5, $00d6, $1e6a,
  $00d8, $00d9, $00da, $00db, $00dc, $00dd, $0176, $00df,
  { $e0 }
  $00e0, $00e1, $00e2, $00e3, $00e4, $00e5, $00e6, $00e7,
  $00e8, $00e9, $00ea, $00eb, $00ec, $00ed, $00ee, $00ef,
  { $f0 }
  $0175, $00f1, $00f2, $00f3, $00f4, $00f5, $00f6, $1e6b,
  $00f8, $00f9, $00fa, $00fb, $00fc, $00fd, $0177, $00ff);

const 
  ISO15Unicode: array[#$A0..#$BF] of Unicode = (
  { $a0 }
  $00a0, $00a1, $00a2, $00a3, $20ac, $00a5, $0160, $00a7,
  $0161, $00a9, $00aa, $00ab, $00ac, $00ad, $00ae, $00af,
  { $b0 }
  $00b0, $00b1, $00b2, $00b3, $017d, $00b5, $00b6, $00b7,
  $017e, $00b9, $00ba, $00bb, $0152, $0153, $0178, $00bf);

const 
  ISO16Unicode: array[#$A0..#$FF] of Unicode = (
  { $a0 }
  $00a0, $0104, $0105, $0141, $20ac, $201e, $0160, $00a7,
  $0161, $00a9, $0218, $00ab, $0179, $00ad, $017a, $017b,
  { $b0 }
  $00b0, $00b1, $010c, $0142, $017d, $201d, $00b6, $00b7,
  $017e, $010d, $0219, $00bb, $0152, $0153, $0178, $017c,
  { $c0 }
  $00c0, $00c1, $00c2, $0102, $00c4, $0106, $00c6, $00c7,
  $00c8, $00c9, $00ca, $00cb, $00cc, $00cd, $00ce, $00cf,
  { $d0 }
  $0110, $0143, $00d2, $00d3, $00d4, $0150, $00d6, $015a,
  $0170, $00d9, $00da, $00db, $00dc, $0118, $021a, $00df,
  { $e0 }
  $00e0, $00e1, $00e2, $0103, $00e4, $0107, $00e6, $00e7,
  $00e8, $00e9, $00ea, $00eb, $00ec, $00ed, $00ee, $00ef,
  { $f0 }
  $0111, $0144, $00f2, $00f3, $00f4, $0151, $00f6, $015b,
  $0171, $00f9, $00fa, $00fb, $00fc, $0119, $021b, $00ff);

const 
  CP1252Unicode: array[#$80..#$9F] of Unicode = (
  { $80 }
  $20ac, $fffd, $201a, $0192, $201e, $2026, $2020, $2021,
  $02c6, $2030, $0160, $2039, $0152, $fffd, $017d, $fffd,
  { $90 }
  $fffd, $2018, $2019, $201c, $201d, $2022, $2013, $2014,
  $02dc, $2122, $0161, $203a, $0153, $fffd, $017e, $0178);

const 
  CP850Unicode: array[#$80..#$FF] of Unicode = (
  { $80 }
  $00c7, $00fc, $00e9, $00e2, $00e4, $00e0, $00e5, $00e7,
  $00ea, $00eb, $00e8, $00ef, $00ee, $00ec, $00c4, $00c5,
  { $90 }
  $00c9, $00e6, $00c6, $00f4, $00f6, $00f2, $00fb, $00f9,
  $00ff, $00d6, $00dc, $00f8, $00a3, $00d8, $00d7, $0192,
  { $a0 }
  $00e1, $00ed, $00f3, $00fa, $00f1, $00d1, $00aa, $00ba,
  $00bf, $00ae, $00ac, $00bd, $00bc, $00a1, $00ab, $00bb,
  { $b0 }
  $2591, $2592, $2593, $2502, $2524, $00c1, $00c2, $00c0,
  $00a9, $2563, $2551, $2557, $255d, $00a2, $00a5, $2510,
  { $c0 }
  $2514, $2534, $252c, $251c, $2500, $253c, $00e3, $00c3,
  $255a, $2554, $2569, $2566, $2560, $2550, $256c, $00a4,
  { $d0 }
  $00f0, $00d0, $00ca, $00cb, $00c8, $0131, $00cd, $00ce,
  $00cf, $2518, $250c, $2588, $2584, $00a6, $00cc, $2580,
  { $e0 }
  $00d3, $00df, $00d4, $00d2, $00f5, $00d5, $00b5, $00fe,
  $00de, $00da, $00db, $00d9, $00fd, $00dd, $00af, $00b4,
  { $f0 }
  $00ad, $00b1, $2017, $00be, $00b6, $00a7, $00f7, $00b8,
  $00b0, $00a8, $00b7, $00b9, $00b3, $00b2, $25a0, $00a0);

const 
  KOI8RUnicode: array[#$80..#$FF] of Unicode = (
  { $80 }
  $2500, $2502, $250c, $2510, $2514, $2518, $251c, $2524,
  $252c, $2534, $253c, $2580, $2584, $2588, $258c, $2590,
  { $90 }
  $2591, $2592, $2593, $2320, $25a0, $2219, $221a, $2248,
  $2264, $2265, $00a0, $2321, $00b0, $00b2, $00b7, $00f7,
  { $a0 }
  $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556,
  $2557, $2558, $2559, $255a, $255b, $255c, $255d, $255e,
  { $b0 }
  $255f, $2560, $2561, $0401, $2562, $2563, $2564, $2565,
  $2566, $2567, $2568, $2569, $256a, $256b, $256c, $00a9,
  { $c0 }
  $044e, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
  $0445, $0438, $0439, $043a, $043b, $043c, $043d, $043e,
  { $d0 }
  $043f, $044f, $0440, $0441, $0442, $0443, $0436, $0432,
  $044c, $044b, $0437, $0448, $044d, $0449, $0447, $044a,
  { $e0 }
  $042e, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
  $0425, $0418, $0419, $041a, $041b, $041c, $041d, $041e,
  { $f0 }
  $041f, $042f, $0420, $0421, $0422, $0423, $0416, $0412,
  $042c, $042b, $0417, $0428, $042d, $0429, $0427, $042a);

const 
  KOI8UUnicode: array[#$80..#$FF] of Unicode = (
  { $80 }
  $2500, $2502, $250c, $2510, $2514, $2518, $251c, $2524,
  $252c, $2534, $253c, $2580, $2584, $2588, $258c, $2590,
  { $90 }
  $2591, $2592, $2593, $2320, $25a0, $2219, $221a, $2248,
  $2264, $2265, $00a0, $2321, $00b0, $00b2, $00b7, $00f7,
  { $a0 }
  $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457,
  $2557, $2558, $2559, $255a, $255b, $0491, $255d, $255e,
  { $b0 }
  $255f, $2560, $2561, $0401, $0404, $2563, $0406, $0407,
  $2566, $2567, $2568, $2569, $256a, $0490, $256c, $00a9,
  { $c0 }
  $044e, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
  $0445, $0438, $0439, $043a, $043b, $043c, $043d, $043e,
  { $d0 }
  $043f, $044f, $0440, $0441, $0442, $0443, $0436, $0432,
  $044c, $044b, $0437, $0448, $044d, $0449, $0447, $044a,
  { $e0 }
  $042e, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
  $0425, $0418, $0419, $041a, $041b, $041c, $041d, $041e,
  { $f0 }
  $041f, $042f, $0420, $0421, $0422, $0423, $0416, $0412,
  $042c, $042b, $0417, $0428, $042d, $0429, $0427, $042a);

const 
  KOI8RUUnicode: array[#$80..#$FF] of Unicode = (
  { $80 }
  $2500, $2502, $250c, $2510, $2514, $2518, $251c, $2524,
  $252c, $2534, $253c, $2580, $2584, $2588, $258c, $2590,
  { $90 }
  $2591, $2592, $2593, $2320, $25a0, $2219, $221a, $2248,
  $2264, $2265, $00a0, $2321, $00b0, $00b2, $00b7, $00f7,
  { $a0 }
  $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457,
  $2557, $2558, $2559, $255a, $255b, $0491, $045e, $255e,
  { $b0 }
  $255f, $2560, $2561, $0401, $0404, $2563, $0406, $0407,
  $2566, $2567, $2568, $2569, $256a, $0490, $040e, $00a9,
  { $c0 }
  $044e, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
  $0445, $0438, $0439, $043a, $043b, $043c, $043d, $043e,
  { $d0 }
  $043f, $044f, $0440, $0441, $0442, $0443, $0436, $0432,
  $044c, $044b, $0437, $0448, $044d, $0449, $0447, $044a,
  { $e0 }
  $042e, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
  $0425, $0418, $0419, $041a, $041b, $041c, $041d, $041e,
  { $f0 }
  $041f, $042f, $0420, $0421, $0422, $0423, $0416, $0412,
  $042c, $042b, $0417, $0428, $042d, $0429, $0427, $042a);


function ISO1toUnicode(c: char): Unicode; { western Europe (old) }
function ISO2toUnicode(c: char): Unicode; { east Europe }
function ISO3toUnicode(c: char): Unicode; { south Europe (old) }
function ISO4toUnicode(c: char): Unicode; { baltic }
function ISO5toUnicode(c: char): Unicode; { kyrillic }
function ISO6toUnicode(c: char): Unicode; { arabic }
function ISO7toUnicode(c: char): Unicode; { greek }
function ISO8toUnicode(c: char): Unicode; { hebrew }
function ISO9toUnicode(c: char): Unicode; { turkish }
function ISO10toUnicode(c: char): Unicode; { nordic }
function ISO11toUnicode(c: char): Unicode; { thai }
function ISO13toUnicode(c: char): Unicode; { baltic (new) }
function ISO14toUnicode(c: char): Unicode; { celtic }
function ISO15toUnicode(c: char): Unicode; { western Europe (new) }
function ISO16toUnicode(c: char): Unicode; { south east Europe }

{ other }
function KOI8RtoUnicode(c: char): Unicode; { kyrillic, RFC 1489 }
function KOI8UtoUnicode(c: char): Unicode; { kyrillic, RFC 2319 }
function KOI8RUtoUnicode(c: char): Unicode; { kyrillic }

{ Windows specific: }
function CP1252toUnicode(c: char): Unicode; { western Europe }

{ DOS specific: }
function CP850toUnicode(c: char): Unicode; { western Europe }


implementation

function ISO1toUnicode(c: char): Unicode; { western Europe }
begin
{ ISO1 is a subset to unicode, so ord() reveals the unicode number }
ISO1toUnicode := ord(c)
end;

function ISO2toUnicode(c: char): Unicode; { east Europe }
begin
if c>=#$A0 
  then ISO2toUnicode := ISO2Unicode[c]
  else ISO2toUnicode := ord(c)
end;

function ISO3toUnicode(c: char): Unicode; { south Europe (old) }
begin
if c>=#$A0 
  then ISO3toUnicode := ISO3Unicode[c]
  else ISO3toUnicode := ord(c)
end;

function ISO4toUnicode(c: char): Unicode; { baltic }
begin
if c>=#$A0 
  then ISO4toUnicode := ISO4Unicode[c]
  else ISO4toUnicode := ord(c)
end;

function ISO5toUnicode(c: char): Unicode; { kyrillic }
begin
if c>=#$A0 
  then ISO5toUnicode := ISO5Unicode[c]
  else ISO5toUnicode := ord(c)
end;

function ISO6toUnicode(c: char): Unicode; { arabic }
begin
if c>=#$A0 
  then ISO6toUnicode := ISO6Unicode[c]
  else ISO6toUnicode := ord(c)
end;

function ISO7toUnicode(c: char): Unicode; { greek }
begin
if c>=#$A0 
  then ISO7toUnicode := ISO7Unicode[c]
  else ISO7toUnicode := ord(c)
end;

function ISO8toUnicode(c: char): Unicode; { hebrew }
begin
if c>=#$A0 
  then ISO8toUnicode := ISO8Unicode[c]
  else ISO8toUnicode := ord(c)
end;

function ISO9toUnicode(c: char): Unicode; { turkish }
begin
if c>=#$D0 
  then ISO9toUnicode := ISO9Unicode[c]
  else ISO9toUnicode := ord(c)
end;

function ISO10toUnicode(c: char): Unicode; { nordic }
begin
if c>=#$A0 
  then ISO10toUnicode := ISO10Unicode[c]
  else ISO10toUnicode := ord(c)
end;

function ISO11toUnicode(c: char): Unicode; { thai }
var u: Unicode;
begin
if c<#$A1 
  then u := ord(c)
  else u := ord(c) + $0D60;

ISO11toUnicode := u
end;

function ISO13toUnicode(c: char): Unicode; { baltic (new) }
begin
if c>=#$A0 
  then ISO13toUnicode := ISO13Unicode[c]
  else ISO13toUnicode := ord(c)
end;

function ISO14toUnicode(c: char): Unicode; { celtic }
begin
if c>=#$A0 
  then ISO14toUnicode := ISO14Unicode[c]
  else ISO14toUnicode := ord(c)
end;

function ISO15toUnicode(c: char): Unicode; { western Europe (new) }
begin
if (c>=#$A0) and (c<=#$BF)
  then ISO15toUnicode := ISO15Unicode[c]
  else ISO15toUnicode := ord(c)
end;

function ISO16toUnicode(c: char): Unicode; { south east Europe }
begin
if c>=#$A0 
  then ISO16toUnicode := ISO16Unicode[c]
  else ISO16toUnicode := ord(c)
end;

function KOI8RtoUnicode(c: char): Unicode;
begin
if c>=#$80 
  then KOI8RtoUnicode := KOI8RUnicode[c]
  else KOI8RtoUnicode := ord(c)
end;

function KOI8UtoUnicode(c: char): Unicode;
begin
if c>=#$80 
  then KOI8UtoUnicode := KOI8UUnicode[c]
  else KOI8UtoUnicode := ord(c)
end;

function KOI8RUtoUnicode(c: char): Unicode;
begin
if c>=#$80 
  then KOI8RUtoUnicode := KOI8RUUnicode[c]
  else KOI8RUtoUnicode := ord(c)
end;

function CP1252toUnicode(c: char): Unicode;
begin
if (c>=#$80) and (c<=#$9F)
  then CP1252toUnicode := CP1252Unicode[c]
  else CP1252toUnicode := ord(c)
end;

function CP850toUnicode(c: char): Unicode; { western Europe }
begin
if c>=#$80 
  then CP850toUnicode := CP850Unicode[c]
  else CP850toUnicode := ord(c)
end;

procedure ident(const s: string);
begin
end;

begin
ident('$Id: chconv.pas,v 1.4 2006/10/11 06:26:22 akf Exp $')
end.
