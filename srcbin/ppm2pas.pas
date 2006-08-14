{
* ppm2pas
* converts from a PPM (P6) file into FreePascal code
*
* Copyright (c) 2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: FreePascal
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*}

program ppm2pas(input, output, stderr, f);

{$I+}

type ImgInfo = record
                  Width   : longint;
		  Height  : longint;
		  reserved: longint;
		  Image   : array[0..$FFFFFFF] of word;
		  end;

var Buffer : pointer;

{$IfDef FPC}
  const nl = LineEnding;
  var Comment : AnsiString;
{$Else}
  const nl = LineBreak;
  var Comment : String(2048);
{$EndIf}

procedure version;
begin
WriteLn('ppm2pas 0.2');
WriteLn;
WriteLn('Copyright (C) 2006 AKFoerster');
WriteLn;
WriteLn('License: GPL v2 or later');
WriteLn;
WriteLn('This program comes with NO WARRANTY, to the extent permitted by law.');
WriteLn('You may redistribute it under the terms of the GNU General Public License;');
WriteLn('see the file named COPYING for details.');
WriteLn;
WriteLn('Written by Andreas K. Foerster');
Halt
end;

procedure help;
begin
WriteLn;
WriteLn('Converts raw ppm files (P4-P6) into code for FreePascal');
WriteLn;
WriteLn('Usage: ppm2pas file constname');
WriteLn;
WriteLn('Report bugs to <akfquiz@akfoerster.de>.');
halt
end;

{ strips from # comments }
function stripcomment(x: string): shortstring;
var i: integer;
begin
i := pos('#', x);
if i=0
  then
    stripcomment:=x
  else
    begin
    if Comment<>'' then Comment := Comment + nl;
    Comment := Comment + copy(x, i+1, length(x)-i);
    stripcomment := copy(x, 1, pred(i))
    end
end;

function getfirstnumber(s: string): integer;
var r,e : integer;
begin
s := copy(s, 1, pos(' ', s)-1);
val(s, r, e);
getfirstnumber := r
end;

function getsecondnumber(s: string): integer;
var r,e : integer;
begin
delete(s, 1, pos(' ', s));
val(s, r, e);
getsecondnumber := r
end;

procedure Fehler(s: string);
begin
WriteLn(stderr, s);
Halt(1);
end;

procedure LiesDatei(Datei: string);
var f : text;
    s : string;
    format: byte;
    x, y: longint;
    i : longint;
    r,g,b : char;
begin
Assign(f, Datei);
{$I-}
Reset(f);
{$I+}
if IOResult<>0 then Fehler('Can''t open file');

repeat
  ReadLn(f, s);    { Type }
  s := stripComment(s)
until (s<>'') or eof(f);

if (s[1]<>'P') or (s[2]<'4') or (s[2]>'6') then 
   Fehler('unsupported filetype');

Format := Ord(s[2])-48; { 4=mono; 5=gray; 6=color }

repeat 
  ReadLn(f, s); { Size }
  s := stripComment(s)
until s<>'';

x := getfirstnumber(s); 
y := getsecondnumber(s); 

if (Format<>1) and (Format<>4) then { if not mono bitmap }
  repeat
    ReadLn(f, s);   { Max value - ignored }
    s := stripComment(s)
  until s<>'';

GetMem(Buffer, (x*y*SizeOf(word))+12);

With ImgInfo(Buffer^) do
  begin
  Width := x {$IfDef VER1_0} -1 {$EndIf};
  Height := y {$IfDef VER1_0} -1 {$EndIf};
  reserved := 0;
  end;

for i:=0 to x*y-1 do
    begin
    Read(f, r, g, b);
    ImgInfo(Buffer^).Image[i] := 
        ((byte(r) shr 3) shl 11) or 
	((byte(g) shr 2) shl 5) or 
	(byte(b) shr 3);
    end;
    
Close(f)
end;

procedure SchreibePas(name: string);
var datasize: longInt;
    i : longint;
begin
With ImgInfo(Buffer^) do 
  datasize := (Width {$IfDef VER1_0} +1 {$EndIf}) *
              (Height {$IfDef VER1_0} +1 {$EndIf});

WriteLn('{ Bitmap for FPC }');
WriteLn('{ made with ppm2pas }');
WriteLn;
if Comment<>'' then
  begin
  WriteLn('{', Comment, ' }');
  WriteLn
  end;
WriteLn('const ', name);
WriteLn('        : packed record');
WriteLn('             Width    : LongInt;');
WriteLn('             Height   : LongInt;');
WriteLn('             reserved : LongInt;');
WriteLn('             Image    : array[0..',datasize-1,'] of word;');
WriteLn('             end');

With ImgInfo(Buffer^) do 
   begin
   WriteLn('= (Width:',Width,'; Height:',Height,'; reserved:0;');
   WriteLn('Image:(');
   Write('$',HexStr(Image[0],4));
   For i := 1 to datasize-1 do
      begin
      Write(',');
      if (i mod (72 div 6))=0 then WriteLn;
      Write('$',HexStr(Image[i], 4));
      end;
   WriteLn('));');
   end;
WriteLn;
end;

begin
Comment := '';

if ParamStr(1)='--help' then help;
if ParamStr(1)='--version' then version;

if ParamCount<>2 then help;

LiesDatei(ParamStr(1));
SchreibePas(ParamStr(2));
end.
