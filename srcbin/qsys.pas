{
* qsys (unit)
*
* $Id: qsys.pas,v 1.13 2006/10/13 07:36:18 akf Exp $
*
* Copyright (c) 2004, 2005, 2006 Andreas K. Foerster <akfquiz@akfoerster.de>
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

{ system specific stuff for AKFQuiz }

{$X+}
{$I-}

{$IfDef FPC}
  {$Mode Delphi}
  {$Smartlink on}
  {$LongStrings on}
{$EndIf}


{ compatiblity definition }
{$IfDef _WIN32} {$Define Win32} {$EndIf}


unit qsys;

interface

{$IfDef __GPC__}

  import GPC (GetEnv => GetEnvironmentVariable,
              FSearch => FileSearch,
	      FExpand => ExpandFilename,
              DirSeparator => DirectorySeparator,
              LineBreak => LineEnding);
         chconv;
  {$IfDef Win32}
         Winprocs (GetEnvironmentVariable => WGetEnvironmentVariable); 
	 WinTypes;
  {$EndIf}

{$Else}

    uses
    {$IfDef Win32}
       Windows,
    {$EndIf} { Win32 }
    SysUtils, chconv;

{$EndIf} { __GPC__ }

{$IfDef __GPC__}
  type 
    Uint8  = Cardinal attribute (Size =  8);
    Uint16 = Cardinal attribute (Size = 16);
    Sint16 = Integer  attribute (Size = 16);
    Uint32 = Cardinal attribute (Size = 32);
    Sint32 = Integer  attribute (Size = 32);
    pByte  = ^Uint8;
  
  {$if __GPC_RELEASE__ < 20041218}
    type CInteger = Integer;
  {$EndIf} { __GPC_RELEASE__ }
  
{$Else} { not __GPC__ }

  type 
    CInteger = LongInt; { @@@ for 32-Bit systems }
    CString  = PChar;
    Uint8    = byte;
    Uint16   = word;
    Sint16   = SmallInt;
    Uint32   = cardinal;
    Sint32   = LongInt;
    pByte    = ^Uint8;

{$EndIf} { not __GPC__ }

{$IfDef FPC}
    type mystring = ansistring; { Delphi dialect (unlimited length) }
{$Else}
    type mystring = string(2048); { Extended Pascal }
{$EndIf}

{ strings are implemented differently in Pascal dialects. }
type string75  = string[75];
type string80  = string[80];
type string255 = string[255];

type shortstring = string255; { needed by GPC }

type Tshowentry = procedure(const dir, f : string);
type Tconverter = function(const s: string): mystring;

{ Displays for which converters are there }
type DisplayType = (ISOdisplay, OEMdisplay, UTF8display);

type 
  PquizfileList = ^TquizfileList;
  TquizfileList = record
                  filename : mystring;
                  title    : string255;
                  next     : PquizfileList
                  end;

{ only used on very few systems - on most it's autodetected }
{$IfDef UNIX}
  const defaultPrefix = '/usr/local';
{$Else}
  const defaultPrefix = '';
{$EndIf}

const quizext  = '.akfquiz';
const quizext2 = '.aqz';

const platform =
{$IfDef __GPC__} 'GPC'; {$EndIf}
{$IfDef FPC}     'FPC '+{$I %FPCTARGETOS%}+'/'+{$I %FPCTARGETCPU%};
{$EndIf}

{ what platforms have charset IBM850 as default? }
{$IfDef DPMI}       {$Define IBM850} {$EndIf}
{$IfDef __OS_DOS__} {$Define IBM850} {$EndIf}
{$IfDef MSDOS}      {$Define IBM850} {$EndIf}

{ IBM850 is supported by more webbrowsers than IBM437,
  but in most every case it's okay to pretend it's 850 }

{$IfDef IBM850}
  const sys_charset = 'IBM850'; { see comment above }
{$Else}
  {$IfDef Win32}
    const sys_charset = 'WINDOWS-1252';
  {$Else}
    const sys_charset = 'ISO-8859-1';
  {$EndIf}
{$EndIf}

const TAB = chr(9);
const nl = LineEnding;

{ HTML 4.01 is the last HTML definition
  Transitional is needed, because of the "target" attribute }
const HTML4DTD = 'http://www.w3.org/TR/REC-html4/loose.dtd';
const HTMLDoctype =
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'+nl+
    '"'+HTML4DTD+'">';

{ Attetion:
  XHTML conflicts with the JavaScript implementation! }
const XHTML1DTD = 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd';
const XHTMLDoctype =
    '<?xml version="1.0" encoding="iso-8859-1"?>'+nl+
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'+nl+
    '"'+XHTML1DTD+'">';

const xmlns='http://www.w3.org/1999/xhtml';

var 
  IntroSignal,    { for Titlescreen }
  QuestionSignal, { for filechooser }
  RightSignal,
  FalseSignal,
  NeutralSignal,
  InfoSignal,  { for result screen }
  ErrorSignal: procedure;

var quizfileList, quizfileListEnd : PquizfileList;

function makeUpcase(x: string): mystring;
function stripWhitespace(x: string): mystring;
function StrToInt(s: string; fallback: integer): integer;
function Min(a, b: LongInt): LongInt;
function Max(a, b: LongInt): LongInt;
{ remove to last dot }
function stripext(const s: string): mystring;

function getnextdir(var rest: mystring): mystring;
function getquizpath: mystring;
function getquizdir: mystring; { first dir from QUIZPATH }
function useDirSeparator(const s: string): mystring;
function basename(const s: string): mystring;
function dirname(const s: string): mystring;
function getQuizTitle(const x: string): mystring;

{ searches quizfile: }
function getquizfile(var s: mystring): boolean; 
function quizfileExists(const s: string): boolean;
function gethtmlname(const s: mystring): mystring;
function ListEntries(const dir, ext: string; showentry: Tshowentry): boolean;

procedure freeQuizFileList;
procedure addQuizFileList(const dir, ext: string);
procedure fillQuizfileList; { uses quizpath! }
function searchQuizfileList(searchNr: integer): mystring;

procedure nobreak;
procedure setLFNsupport;

procedure setUnknownChar(ch: Unicode);
procedure clearUstr(var s: UnicodeString);
procedure addToUStr(var s: UnicodeString; u: Unicode);
procedure reverseUStr(var s: UnicodeString);
function UTF8toUStr(const s: mystring): UnicodeString;

{ for Tconverter }
{ OEM means IBM850, but it can also be used for IBM437 }

function ISO1toUTF8(const s: string): mystring;
function ISO2toUTF8(const s: string): mystring;
function ISO3toUTF8(const s: string): mystring;
function ISO4toUTF8(const s: string): mystring;
function ISO5toUTF8(const s: string): mystring;
function ISO6toUTF8(const s: string): mystring;
function ISO7toUTF8(const s: string): mystring;
function ISO8toUTF8(const s: string): mystring;
function ISO9toUTF8(const s: string): mystring;
function ISO10toUTF8(const s: string): mystring;
function ISO11toUTF8(const s: string): mystring;
function ISO13toUTF8(const s: string): mystring;
function ISO14toUTF8(const s: string): mystring;
function ISO15toUTF8(const s: string): mystring;
function ISO16toUTF8(const s: string): mystring;
function CP1252toUTF8(const s: string): mystring;
function OEMtoUTF8(const s: string): mystring;
function KOI8RtoUTF8(const s: string): mystring;
function KOI8UtoUTF8(const s: string): mystring;
function KOI8RUtoUTF8(const s: string): mystring;

function UTF8toISO1(const s: string): mystring;
function UTF8toISO15(const s: string): mystring;
function ISO1toOEM(const s: string): mystring;
function OEMtoISO1(const s: string): mystring;
function UTF8toOEM(const s: string): mystring;
function CP1252toISO15(const s: string): mystring;
{ HTML numeric entities: }
function ISO1toHTML(const s: string): mystring;
function UTF8toHTML(const s: string): mystring;
function forceASCII(const s: string): mystring;
function noconversion(const s: string): mystring;

function csToUTF8(const cs, txt: mystring): mystring;

{ length of real characters in a UTF8 string }
function UTF8Length(const s: string): LongInt;

procedure handleBiDi(var s: UnicodeString);

function checkISO(const s: mystring): boolean;
function whichISO(const s: mystring): integer;
function checkUTF8(const s: mystring): boolean;
function checkOEM(const s: mystring): boolean;
function checkASCII(const s: mystring): boolean;
function checkCP1252(const s: mystring): boolean;
function checkKOI8R(const s: mystring): boolean;
function checkKOI8U(const s: mystring): boolean;
function checkKOI8RU(const s: mystring): boolean;

{ check what the display supports }
function checkOEMdisplay: boolean;
function checkUTF8display: boolean;
function checkDisplay: DisplayType;
function getSystemLanguage: mystring;

{ procedures for signal variables }
procedure NoSignal;
procedure SystemBeep;

{ assign all signals: }
procedure DisableSignals;
procedure useBeepSignals; { use system beeps (#7) }

{ show time from given seconds }
function ShowTime(sec: LongInt): mystring; 
function GetSecs: LongInt;

function showDateTime: mystring;

procedure ident(const s: string);

{$IfDef __GPC__}
  function IntToStr(i: LongInt): mystring;
{$EndIf}



implementation

{ I'm still using FPC 1.0.10 under DOS due to problems with newer versions }
{$IfDef FPC}{$IfDef VER1_0}
  {$INFO 1_0}
  uses DOS; { for GetEnv }
{$EndIf}{$EndIf}

var QUIZPATH : mystring;
var unknownChar: Unicode = ord('?');

{$IfDef FPC}{$IfDef VER1_0}
  function GetEnvironmentVariable(s: string): mystring;
  begin
  GetEnvironmentVariable := GetEnv(s)
  end;
{$EndIf}{$EndIf}

function Min(a, b: LongInt): LongInt;
begin
if a<b then Min := a else Min := b
end;

function Max(a, b: LongInt): LongInt;
begin
if a>b then Max := a else Max := b
end;

function StrToInt(s: string; fallback: integer): integer;
var value, code: integer;
begin
val(s, value, code);
if code=0 
  then StrToInt := value
  else StrToInt := fallback
end;

{ removes leading and trailing spaces and tabs }
function stripWhitespace(x: string): mystring;
var len, f, t: integer;
begin
len := length(x);
f := 1;
t := len;
while (f<len) and ((x[f]=' ') or (x[f]=TAB)) do f := succ(f);
while (t>1) and ((x[t]=' ') or (x[t]=TAB) or (x[t]=chr(13)))
  do t := pred(t);
if f<=t 
  then stripWhitespace := copy(x, f, t)
  else stripWhitespace := ''
end;

function makeUpcase(x: string): mystring;
begin
{$IfDef __GPC__}
  makeUpcase := UpcaseStr(x)
{$Else}
  makeUpcase := Uppercase(x)
{$EndIf}
end;

procedure NoSignal;
begin end;

procedure SystemBeep;
begin Write(#7) end;

procedure DisableSignals;
begin
IntroSignal    := NoSignal;
QuestionSignal := NoSignal;
RightSignal    := NoSignal;
FalseSignal    := NoSignal;
NeutralSignal  := NoSignal;
InfoSignal     := NoSignal;
ErrorSignal    := NoSignal
end;

procedure useBeepSignals;
begin
IntroSignal    := NoSignal;
QuestionSignal := NoSignal;
RightSignal    := NoSignal;
FalseSignal    := SystemBeep;
NeutralSignal  := NoSignal;
InfoSignal     := SystemBeep;
ErrorSignal    := SystemBeep
end;

function getSystemLanguage: mystring;
var l: mystring;
begin
l := GetEnvironmentVariable('LANG');

{$IfDef Win32}
  if l='' then
    case (GetUserDefaultLangID and $03FF) of
      { GPC doesn't have the LANG constants yet }
      $36 : l := 'af';
      $04 : l := 'zh';
      $05 : l := 'cs';
      $06 : l := 'da';
      $13 : l := 'nl';
      $09 : l := 'en';
      $0B : l := 'fi';
      $0C : l := 'fr';
      $07 : l := 'de';
      $08 : l := 'el';
      $0D : l := 'he';
      $10 : l := 'it';
      $11 : l := 'jp';
      $14 : l := 'no';
      $15 : l := 'pl';
      $16 : l := 'pt';
      $19 : l := 'ru';
      $0A : l := 'es';
      $1D : l := 'sv';
      $1F : l := 'tr';
      { ... to be continued @@ }
      end;
{$EndIf}

getSystemLanguage := l
end;

function useDirSeparator(const s: string): mystring;
begin
{$IfDef __GPC__}
  useDirSeparator := ForceAddDirSeparator(s)
{$Else}
  useDirSeparator := IncludeTrailingPathDelimiter(s)
{$EndIf}
end;

function basename(const s: string): mystring;
begin
{$IfDef __GPC__}
  basename := NameExtFromPath(s)
{$Else}
  basename := ExtractFileName(s)
{$EndIf}
end;

function dirname(const s: string): mystring;
begin
{$IfDef __GPC__}
  dirname := DirFromPath(s)
{$Else}
  dirname := ExtractFilePath(s)
{$EndIf}
end;

{$IfDef NoRelocation}

  function getprefix: mystring;
  begin
  getprefix := defaultPrefix
  end;

{$Else} { Relocation }

  function searchExecutable(const s: mystring): mystring;
  begin
  searchExecutable := FileSearch(s, GetEnvironmentVariable('PATH'))
  end;

  function getprefix: mystring;
  var s: mystring;
  begin
  s := ParamStr(0);
  if s='' 
    then getprefix := defaultPrefix
    else begin
         if pos(DirectorySeparator, s)=0 { no directory given }
           then s := searchExecutable(s);
         getprefix := ExpandFileName(dirname(s)+'..')
         end
  end;
 
{$EndIf} { NoRelocation }


{ search for Parameter -d }
function getparamdir: mystring;
var i, count : integer;
begin
{ search for Parameter -d }
count := ParamCount;
i:=1;
while (i<count) 
      and (ParamStr(i)<>'-d') 
      and (ParamStr(i)<>'-D') 
        do inc(i);
if (i<count) and ((ParamStr(i)='-d') or (ParamStr(i)='-D')) 
  then getparamdir := ParamStr(i+1)
  else getparamdir := ''
end;

function getnextdir(var rest: mystring): mystring;
var p: integer;
begin
p := pos(PathSeparator, rest);
if p<>0 
  then begin
       getnextdir := copy(rest, 1, p-1);
       delete(rest, 1, p)
       end
  else begin
       getnextdir := rest;
       rest := ''
       end
end;

function getquizpath: mystring;
begin
getquizpath := QUIZPATH
end;

function getquizdir: mystring;
var d: mystring;
begin
{ must be in a separate variable for getnextdir changes the content }
d := QUIZPATH;

{ only one directory! }
getquizdir := getnextdir(d)
end;

function getquizfile(var s: mystring): boolean;
var e, path: mystring;
begin
{ only search, when s hasn't a path yet }
if pos(DirectorySeparator, s)<>0 
  then getquizfile := FileExists(s)
  else begin
       path := QUIZPATH;
       e := FileSearch(s, path);
       if e='' then
          e := FileSearch(s+quizext, path);
       if e='' then
          e := FileSearch(s+quizext2, path);
       s := e;
       getquizfile := (e<>'')
       end
end;

function quizfileExists(const s: string): boolean;
begin
quizfileExists := FileExists(s) and not DirectoryExists(s)
end;

{ remove to last dot, but leave the path information untouched }
function stripext(const s: string): mystring;
var i: integer;
begin
i:=length(s);
while (i>1) and (s[i]<>'.') do dec(i);
dec(i);
if i>1 then stripext := copy(s, 1, i)
       else stripext := s
end;

{$IfDef __GPC__}

  function gethtmlname(const s: mystring): mystring;
  var path, name, ext: mystring;
  begin
  FSplit(s, path, name, ext);
  gethtmlname := name + '.html'
  end;

{$Else}

  function gethtmlname(const s: mystring): mystring;
  begin
  gethtmlname := ChangeFileExt(ExtractFileName(s), '.html')
  end;

{$EndIf}

{ gets string after colon, leading spaces stripped }
function getvalue(x: string): mystring;
var i: integer;
begin
i := succ(pos(':', x));
while (x[i]=' ') or (x[i]=TAB) do inc(i);
getvalue := copy(x, i, length(x)-i+1);
end;

function getQuizTitle(const x: string): MyString;
var 
  inp: text;
  s, e : String80;
  title,
  charset: String80;
  ignore : integer;
{$IfDef FPC}
  var Buffer : array[1..1024] of char;
{$EndIf}
begin
title := '';
charset := '';
Assign(inp, x);
{$IfDef FPC}
  SetTextBuf(inp, Buffer);
{$EndIf}
reset(inp);
repeat
  ReadLn(inp, s);
  s := stripWhitespace(s);
  e := makeUpcase(s);
  if (pos('TITLE:', e)<>0) or
     (pos('TITEL:', e)<>0) then title := getvalue(s);
  if (pos('ENCODING:',e)=1) or
     (pos('KODIERUNG:',e)=1) or
     (pos('CHARSET:',e)=1) or
     (pos('ZEICHENSATZ:',e)=1) then charset := getvalue(s)
until ((title<>'') and (charset<>'')) or EOF(inp) or (IOResult<>0);
close(inp);
ignore := IOResult; { ignore errors }

if title<>'' 
  then 
    begin
    if charset='' 
      then charset:='US-ASCII'
      else charset := makeUpcase(charset);
    if checkASCII(charset)  then title := forceASCII(title);
    if checkISO(charset)    then title := ISO1ToUTF8(title);
    if checkCP1252(charset) then title := CP1252ToUTF8(title);
    if checkOEM(charset)    then title := OEMtoUTF8(title); 
    getQuizTitle := title 
    end
  else getQuizTitle := x { no title -> filename }
end;

{$IfDef __GPC__}
  function ListEntries(const dir, ext: string; showentry: Tshowentry): boolean;
  var
    d: DirPtr;
    s: TString;
    found: boolean;
  begin
  found := false;
  if dir=''
    then d := OpenDir('.')
    else d := OpenDir(dir);
  if IOResult=0 then
    begin
    s := ReadDir(d);
    while s<>'' do
      begin
      if (s[length(s)-length(ext)+1 .. length(s)] = ext)
        {$IfDef DJGPP}
	  or ((length(ext)>4) and 
	      (s[length(s)-3 .. length(s)] = ext[1..4]))
	{$EndIf}
        then
         begin
         showentry(dir, s);
         found := true
         end;
      s := ReadDir(d)
      end
    end;
  CloseDir(d);
  ListEntries := found
  end;

{$Else} { not GPC }

  function ListEntries(const dir, ext: string; showentry: Tshowentry): boolean;
  var
    info  : TSearchRec;
    rslt  : LongInt;
    found : boolean;
  begin
  found := false;

  if dir=''
     then rslt := SysUtils.FindFirst('*'+ext, FaAnyFile, info)
     else rslt := SysUtils.FindFirst(dir+DirectorySeparator+'*'+ext, FaAnyFile, info);
  if rslt=0 then found := true;

  while rslt=0 do
     begin
     showentry(dir, info.name);
     rslt := SysUtils.FindNext(info)
     end;
  SysUtils.FindClose(info);

  ListEntries := found
  end;
{$EndIf} { not __GPC__}

procedure freeQuizfileList;
var previous: PquizfileList;
begin
while quizfileList<>NIL do
  begin
  previous := quizfileList;
  quizfileList := quizfileList^.next;
  dispose(previous)
  end;
quizFileListEnd := NIL
end;

procedure addQuizFileListEntry(const dir, f : string);
var previous : PquizfileList;
begin
previous := quizfileListEnd;
new(quizfileListEnd);

if quizfileList=NIL 
  then quizfileList := quizfileListEnd
  else previous^.next := quizfileListEnd;

with quizfileListEnd^ do
  begin
  if dir='' 
    then filename := f
    else filename := dir+DirectorySeparator+f;
  title := getQuizTitle(quizfileListEnd^.filename);
  next := NIL
  end
end;

procedure addQuizFileList(const dir, ext: string);
var ignore: boolean;
begin
ignore := ListEntries(dir, ext, addQuizFileListEntry)
end;

procedure fillQuizfileList; { uses quizpath! }
var dir, path: mystring;
begin
if quizfileList<>NIL then freeQuizfileList;

path := getquizpath;
while path<>'' do
  begin
  dir := getnextdir(path);
  addQuizfileList(dir, quizext);
  addQuizfileList(dir, quizext2);
  end
end;

function searchQuizfileList(searchNr: integer): mystring;
var
  entry : PquizfileList;
  Nr: integer;
begin
Nr := 1;
entry := quizfileList;
while (Nr<>searchNr) and (entry<>NIL) do
  begin
  entry := entry^.next;
  inc(Nr)
  end;

{ hopefully found }
if entry<>NIL 
  then searchQuizfileList := entry^.filename
  else searchQuizfileList := '';
end;

procedure nobreak;
begin
{ SetCBreak(false) } {@@@ too DOS specific }
end;

procedure setLFNsupport;
begin
{$IfDef FPC}
  {$IfDef Go32v2}
    LFNsupport := true;
    FileNameCaseSensitive := true
  {$EndIf}
{$EndIf}
end;

procedure setUnknownChar(ch: Unicode);
begin
unknownChar := ch
end;


function ISO8859ControlChar(u: Unicode): boolean;
begin
ISO8859ControlChar := ((u and $FFFFFF7F) < $20)
end;

function ASCIIControlChar(u: Unicode): boolean;
begin
ASCIIControlChar := (u < $20)
end;

{ functions for Tconvert }

function noconversion(const s: string): mystring;
begin
noconversion := s
end;

function forceASCII(const s: string): mystring;
var 
  i: integer;
  e: mystring;
begin
e := s;
for i := 1 to Length(e) do 
  if (e[i]<#32) or (e[i]>#127) then e[i] := chr(unknownChar);
forceASCII := e
end;

function EncodeUTF8(u: Unicode): string80;
begin
case u of
  $000000..$00007F : EncodeUTF8 := chr(u);
  $000080..$0007FF : EncodeUTF8 := chr($C0 or (u shr 6)) +
                                   chr($80 or (u and $3F));
  $000800..$00FFFF : EncodeUTF8 := chr($E0 or (u shr (2*6))) +
                                   chr($80 or ((u shr 6) and $3F)) +
                                   chr($80 or (u and $3F));
  $010000..$1FFFFF : EncodeUTF8 := chr($F0 or (u shr (3*6))) +
                                   chr($80 or ((u shr (2*6)) and $3F)) +
                                   chr($80 or ((u shr 6) and $3F)) +
                                   chr($80 or (u and $3F));
  otherwise EncodeUTF8 := chr(unknownChar)
  end
end;

{ gets the Unicode value of specified position in the UTF-8 string
  the position will be set to the next char
  RFC 3629, ISO 10646 }
function getUTF8Char(const s: mystring; var p: integer): Unicode;
var u : Unicode;
begin
getUTF8Char := unknownChar;
u := unknownChar;

{ attention: do not use this decoder in security critical areas
  it also decodes invalid UTF-8 encodings }

if (s='') or (p>length(s)) then exit;

{ skip followup-bytes }
while ((ord(s[p]) and $C0)=$80) and (p<=length(s)) do inc(p);
if p>length(s) then exit;

case ord(s[p]) of
     $00..$7F : begin { 1 byte encoding }
                u := ord(s[p]);
                inc(p, 1)
                end;
     $C2..$DF : begin { 2 byte encoding }
                u := (ord(s[p]) and $1F) shl 6;
                inc(p);
                u := u or (ord(s[p]) and $3F);
                inc(p)
                end;
     $E0..$EF : begin { 3 byte encoding }
                u := (ord(s[p]) and $0F) shl (2*6);
                inc(p);
                u := u or ((ord(s[p]) and $3F) shl 6);
                inc(p);
                u := u or (ord(s[p]) and $3F);
                inc(p);
                end;
     $F0..$F7 : begin { 4 byte encoding }
                u := (ord(s[p]) and $07) shl (3*6);
                inc(p);
                u := u or ((ord(s[p]) and $3F) shl (2*6));
                inc(p);
                u := u or ((ord(s[p]) and $3F) shl 6);
                inc(p);
                u := u or (ord(s[p]) and $3F);
                inc(p)
                end;
     otherwise inc(p) { skip unknown char anyway }
     end; { case }

getUTF8Char := u
end;

procedure clearUStr(var s: UnicodeString);
begin
s.length := 0
end;

procedure addToUStr(var s: UnicodeString; u: Unicode);
begin
inc(s.length);
s.content[s.length] := u
end;

procedure reverseUStr(var s: UnicodeString);
var 
  t: UnicodeString;
  i: integer;
begin
t := s;
for i := 0 to s.length-1 do
  s.content[i+1] := t.content[t.length-i]
end;

function UTF8toUStr(const s: mystring): UnicodeString;
var 
  p: integer;
  e: UnicodeString;
begin
p := 1;
clearUStr(e);
while (p<=length(s)) and (e.length<MaxCharsPerLine) do
  addToUStr(e, getUTF8Char(s, p));

UTF8toUStr := e
end;


{$IfDef FPC}

  { speedup for AnsiString }
  function AnyToUTF8(const s: string; encoder: UnicodeEncoder): mystring;
  var 
    e: mystring;
    rl: integer;
    i, j: integer;
    UTF8Seq: string80;
  begin
  { stringbuffer - larger than needed }
  SetLength(e, 6*length(s));
  rl := 0; { resulting length of string }
  
  for i := 1 to length(s) do
    begin
    UTF8Seq := EncodeUTF8(encoder(s[i]));
    for j := 1 to length(UTF8Seq) do
      begin inc(rl); e[rl] := UTF8Seq[j] end
    end;
 
  AnyToUTF8 := copy(e, 1, rl)
  end;

{$Else}

  function AnyToUTF8(const s: string; encoder: UnicodeEncoder): mystring;
    var
    i : integer;
    e : mystring;
  begin
  e := '';
  for i := 1 to length(s) do
    e := e + EncodeUTF8(encoder(s[i]));
  AnyToUTF8 := e
  end;

{$EndIf}

function ISO1toUTF8(const s: string): mystring;
begin ISO1toUTF8 := AnyToUTF8(s, ISO1toUnicode) end;

function ISO2toUTF8(const s: string): mystring;
begin ISO2toUTF8 := AnyToUTF8(s, ISO2toUnicode) end;

function ISO3toUTF8(const s: string): mystring;
begin ISO3toUTF8 := AnyToUTF8(s, ISO3toUnicode) end;

function ISO4toUTF8(const s: string): mystring;
begin ISO4toUTF8 := AnyToUTF8(s, ISO4toUnicode) end;

function ISO5toUTF8(const s: string): mystring;
begin ISO5toUTF8 := AnyToUTF8(s, ISO5toUnicode) end;

function ISO6toUTF8(const s: string): mystring;
begin ISO6toUTF8 := AnyToUTF8(s, ISO6toUnicode) end;

function ISO7toUTF8(const s: string): mystring;
begin ISO7toUTF8 := AnyToUTF8(s, ISO7toUnicode) end;

function ISO8toUTF8(const s: string): mystring;
begin ISO8toUTF8 := AnyToUTF8(s, ISO8toUnicode) end;

function ISO9toUTF8(const s: string): mystring;
begin ISO9toUTF8 := AnyToUTF8(s, ISO9toUnicode) end;

function ISO10toUTF8(const s: string): mystring;
begin ISO10toUTF8 := AnyToUTF8(s, ISO10toUnicode) end;

function ISO11toUTF8(const s: string): mystring;
begin ISO11toUTF8 := AnyToUTF8(s, ISO11toUnicode) end;

function ISO13toUTF8(const s: string): mystring;
begin ISO13toUTF8 := AnyToUTF8(s, ISO13toUnicode) end;

function ISO14toUTF8(const s: string): mystring;
begin ISO14toUTF8 := AnyToUTF8(s, ISO14toUnicode) end;

function ISO15toUTF8(const s: string): mystring;
begin ISO15toUTF8 := AnyToUTF8(s, ISO15toUnicode) end;

function ISO16toUTF8(const s: string): mystring;
begin ISO16toUTF8 := AnyToUTF8(s, ISO16toUnicode) end;

function CP1252toUTF8(const s: string): mystring;
begin CP1252toUTF8 := AnyToUTF8(s, CP1252toUnicode) end;

function OEMtoUTF8(const s: string): mystring;
begin OEMtoUTF8 := AnyToUTF8(s, CP850toUnicode) end;

function KOI8RtoUTF8(const s: string): mystring;
begin KOI8RtoUTF8 := AnyToUTF8(s, KOI8RtoUnicode) end;

function KOI8UtoUTF8(const s: string): mystring;
begin KOI8UtoUTF8 := AnyToUTF8(s, KOI8UtoUnicode) end;

function KOI8RUtoUTF8(const s: string): mystring;
begin KOI8RUtoUTF8 := AnyToUTF8(s, KOI8RUtoUnicode) end;

function UTF8toHTML(const s: string): mystring;
var 
  p: integer;
  e : mystring;
  u: Unicode;
begin
p := 1;
e := '';

while p<=length(s) do 
  begin
  u := getUTF8Char(s, p);
  if u<=127 
    then e := e + chr(u) { ASCII }
    else e := e + '&#' + IntToStr(u) + ';'
  end;

UTF8toHTML := e
end;

function UTF8toISO1(const s: string): mystring;
var
  p: integer;
  e: mystring;
  u: Unicode;
begin
p := 1;
e := '';

while p<=length(s) do
  begin
  u := getUTF8Char(s, p);
  if u<=255 
    then e := e + chr(u)
    else e := e + chr(unknownChar)
  end;

UTF8toISO1 := e
end;

function UTF8toISO15(const s: string): mystring;
var
  p: integer;
  e: mystring;
  u: Unicode;
begin
p := 1;
e := '';

while p<=length(s) do
  begin
  u := getUTF8Char(s, p);
  if u<=255 
    then e := e + chr(u) { simply ignore replaced chars }
    else 
      case u of
        $20AC : e := e + chr($A4); { euro }
        $0160 : e := e + chr($A6); { S with caron }
        $0161 : e := e + chr($A8); { s with caron }
        $017D : e := e + chr($B4); { Z with caron }
        $017E : e := e + chr($B8); { z with caron }
        $0152 : e := e + chr($BC); { ligature OE }
        $0153 : e := e + chr($BD); { ligature oe }
        $0178 : e := e + chr($BE); { Y with diaeresis }
        otherwise e := e + chr(unknownChar)
        end
  end;

UTF8toISO15 := e
end;

function ISO1toHTML(const s: string): mystring;
var
  i : integer;
  e : mystring;
begin
{ HTML entities are coded as unicode.
  But ISO1 code-numbers are equivalent with unicode (subset) }
e := '';
for i := 1 to length(s) do
  if s[i] < #$80
     then e := e + s[i]
     else e := e + '&#' + IntToStr(ord(s[i])) + ';';

ISO1toHTML := e
end;

function ISO1toOEM(const s: string): mystring;
var
  i : integer;
  e : mystring;
  c : char;
begin
e := s;
for i := 1 to length(e) do
  if e[i] >= #$80 then
    for c:=#$80 to #$FF do
      if CP850Unicode[c]=ord(e[i]) then e[i] := c;
ISO1toOEM := e
end;


function OEMtoISO1(const s: string): mystring;
var
  i : integer;
  e : mystring;
begin
e := s;
for i := 1 to length(e) do
  if e[i] >= #$80 then
    if CP850Unicode[e[i]]<=$00FF
      then e[i] := chr(CP850Unicode[e[i]])
      else e[i] := chr(unknownChar);
OEMtoISO1 := e
end;

function CP1252toISO15(const s: string): mystring;
var
  i: integer;
  e: mystring;
begin
e := s;
for i := 1 to length(e) do
  case ord(e[i]) of
    $80 : e[i] := chr($A4);
    $8A : e[i] := chr($A6);
    $9A : e[i] := chr($A8);
    $8E : e[i] := chr($B4);
    $9E : e[i] := chr($B8);
    $8C : e[i] := chr($BC);
    $9C : e[i] := chr($BD);
    $9F : e[i] := chr($BE);
    { similar looking chars: }
    $A6 : e[i] := '|';
    $A8 : e[i] := '"';
    $B4 : e[i] := '''';
    $B8 : e[i] := ',';
    { unsupported: }
    $A4, $BC, $BD, $BE : e[i] := chr(unknownChar);
    end;
CP1252toISO15 := e
end;

function UTF8toOEM(const s: string): mystring;
var
  p: integer;
  e: mystring;
  c: char;
  u: Unicode;
begin
p := 1;
e := '';

while p<=length(s) do
  begin
  u := getUTF8Char(s, p);
  if u<=$0080
    then e := e + chr(u)
    else for c:=#$80 to #$FF do
           if CP850Unicode[c]=u then e := e + c
  end;

UTF8toOEM := e
end;

function csToUTF8(const cs, txt: mystring): mystring;
begin
csToUTF8 := txt; { default: no conversion }

if checkISO(cs) then
  case whichISO(cs) of
     1: csToUTF8 := ISO1ToUTF8(txt);
     2: csToUTF8 := ISO2ToUTF8(txt);
     3: csToUTF8 := ISO3ToUTF8(txt);
     4: csToUTF8 := ISO4ToUTF8(txt);
     5: csToUTF8 := ISO5ToUTF8(txt);
     6: csToUTF8 := ISO6ToUTF8(txt);
     7: csToUTF8 := ISO7ToUTF8(txt);
     8: csToUTF8 := ISO8ToUTF8(txt);
     9: csToUTF8 := ISO9ToUTF8(txt);
    10: csToUTF8 := ISO10ToUTF8(txt);
    11: csToUTF8 := ISO11ToUTF8(txt);
    13: csToUTF8 := ISO13ToUTF8(txt);
    14: csToUTF8 := ISO14ToUTF8(txt);
    15: csToUTF8 := ISO15ToUTF8(txt);
    16: csToUTF8 := ISO16ToUTF8(txt);
    otherwise csToUTF8 := txt;
    end;
 
if checkCP1252(cs) then csToUTF8 := CP1252toUTF8(txt);
if checkOEM(cs)    then csToUTF8 := OEMtoUTF8(txt);
if checkASCII(cs)  then csToUTF8 := forceASCII(txt);
{ UTF8 needn't be checked }
end;

function UTF8Length(const s: string): LongInt;
var i, res: LongInt;
begin
res := 0;
{ count ASCII bytes and start bytes, ignore the rest }
for i := 1 to length(s) do
   if (ord(s[i])<=127) or (ord(s[i])>=$C0) then inc(res);
UTF8Length := res
end;

{ far from being complete! @@@ }
{ assumes main direction to be left to  right }
procedure handleBiDi(var s: UnicodeString);
const 
  BiDiNeutral = [ord(' '), ord(':'), ord('.'), ord('"'), ord(''''), 
                 ord('!'), ord('?')];
const 
  LeftToRight = false;
  RightToLeft = true;
var 
 e: UnicodeString;
 u: Unicode;
 i, j, k : integer;
begin
clearUStr(e);

i:=0;
repeat
  inc(i);
  u := s.content[i];

  if (u >= $0590) and (u <= $07BF) then
    begin
    j := i; { move j to end of RightToLeft chars: @@@ }
    u := s.content[j];
    while (j<=s.length) and
          (((u >= $0590) and (u <= $07BF)) or 
            (u in BiDiNeutral)) do 
      begin inc(j); u := s.content[j] end;
    for k:=j-1 downto i do addToUStr(e, s.content[k]);
    i := j;
    u := s.content[i]
    end;
    
  addToUStr(e, u);
until i>=s.length;

s := e
end;

function checkISO(const s: mystring): boolean;
begin
checkISO := (pos('ISO-8859',s)<>0) or
            (pos('ISO 8859',s)<>0) or 
            (pos('ISO_8859',s)<>0) or
            (pos('LATIN',s)<>0) 
end;

function whichISO(const s: mystring): integer;
var 
  e: mystring;
  value, code: integer;
begin
whichISO := 0;
if not checkISO(s) then exit;
e := s;

if pos('ISO',e)=1 then 
  begin
  Delete(e, 1, length('ISO 8859-'));
  val(e, value, code);
  if code=0 then whichISO := value;
  exit
  end;
  
if pos('LATIN',e)=1 then 
  begin
  if pos('LATIN-',e)=1 
    then Delete(e, 1, length('LATIN-'))
    else Delete(e, 1, length('LATIN'));
  val(e, value, code);
  if code<>0 then exit;

  case value of
    1  : whichISO := 1;
    2  : whichISO := 2;
    3  : whichISO := 3;
    4  : whichISO := 4;
    5  : whichISO := 9;
    6  : whichISO := 10;
    7  : whichISO := 13;
    8  : whichISO := 14;
    9  : whichISO := 15;
    10 : whichISO := 16;
    otherwise whichISO := 0; { unknown }
    end
  end;
  
end;

function checkCP1252(const s: mystring): boolean;
begin
checkCP1252 := (s='WINDOWS-1252') or (s='CP1252')
end;

function checkUTF8(const s: mystring): boolean;
begin
checkUTF8 := (pos('UTF-8',s)<>0) or 
             (pos('UTF8',s)<>0)
end;

function checkOEM(const s: mystring): boolean;
begin
checkOEM := (pos('DOS',s)<>0) or { 'DOS' is not official! }
            (pos('CP850',s)<>0) or 
            (pos('CP437',s)<>0) or
            (pos('IBM850',s)<>0) or 
            (pos('IBM437',s)<>0) or
            (s='850') or 
            (s='437')
end;

function checkASCII(const s: mystring): boolean;
begin
checkASCII := (s='ASCII') or 
              (s='US-ASCII')
end;

function checkKOI8R(const s: mystring): boolean;
begin
checkKOI8R := (s='KOI8-R') or (s='KOI8R') or (s='KOI8')
end;

function checkKOI8U(const s: mystring): boolean;
begin
checkKOI8U := (s='KOI8-U') or (s='KOI8U')
end;

function checkKOI8RU(const s: mystring): boolean;
begin
checkKOI8RU := (s='KOI8-RU') or (s='KOI8RU')
end;

function checkUTF8display: boolean;
var
 UTF8 : boolean;
 s: mystring;
begin
UTF8 := false;

s := makeUpcase(GetEnvironmentVariable('MM_CHARSET'));
if (s='UTF-8') or (s='UTF8') then UTF8 := true;

s := makeUpcase(GetEnvironmentVariable('LANG'));
if (pos('UTF-8', s)<>0) or (pos('UTF8', s)<>0) then UTF8 := true;

checkUTF8display := UTF8
end;

function checkOEMdisplay: boolean;
var s: mystring;
begin
{ Windows uses different charsets in the Editor and in console mode }
{$IfDef Win32} {$Define OEM} {$EndIf}
{$IfDef DPMI} {$Define OEM} {$EndIf}
{$IfDef __OS_DOS__} {$Define OEM} {$EndIf}
{$IfDef MSDOS}      {$Define OEM} {$EndIf}

{$IfDef OEM}
  checkOEMdisplay := true;
{$Else}
  s := makeUpcase(GetEnvironmentVariable('MM_CHARSET'));
  checkOEMdisplay := (s='DOS') { 'DOS' is not official! }
                     or (s='CP850') or (s='CP437')
                     or (s='IBM850')or (s='IBM437')
                     or (s='850') or (s='437')
{$EndIf}
end;

function checkDisplay: DisplayType;
begin
checkDisplay := ISOdisplay; { set a default }
if checkOEMdisplay then checkDisplay := OEMdisplay;
if checkUTF8display then checkDisplay := UTF8display;
end;

function IntTo2Str(i: LongInt): mystring;
begin
if i<10 
  then IntTo2Str := '0'+IntToStr(i)
  else IntTo2Str := IntToStr(i)
end;


function ShowTime(sec: LongInt): mystring;
var h, m, s: integer;
begin
h := sec div (60*60);
m := (sec div 60) mod 60;
s := sec mod 60;
if h=0 
  then ShowTime := IntTo2Str(m)+':'+IntTo2Str(s)
  else ShowTime := IntToStr(h)+':'+IntTo2Str(m)+':'+IntTo2Str(s)
end;

function GetSecs: LongInt;
begin
{$IfDef FPC}
  GetSecs := trunc(TimestampToMSecs(DateTimeToTimestamp(Now))) div 1000
{$Else}
  GetSecs := GetMicroSecondTime div 1000000
{$EndIf}
end;

{$IfDef FPC}

  function showDateTime: mystring;
  var s: mystring;
  begin
  DateTimeToString(s, 'yyyy-mm-dd, hh:nn:ss', now);
  showDateTime := s
  end;

{$Else} { not FPC }

  function showDateTime: mystring;
  var t: TimeStamp;
  begin
  GetTimeStamp(t); { ISO-10206 }
  showDateTime := FormatTime(t, '%F, %T') { GNU-Pascal }
  end;

{$EndIf} { not FPC }

{$IfDef __GPC__}
  function IntToStr(i: LongInt): mystring;
  begin
  IntToStr := Integer2String(i)
  end;
{$EndIf}

{ dummy function, just to keep the string from beeing stripped }
procedure ident(const s: string);
begin
end;


INITIALIZATION

  ident('$Id: qsys.pas,v 1.13 2006/10/13 07:36:18 akf Exp $');
  disableSignals; { initializes Signals }
  
  quizfileList := NIL;
  quizfileListEnd := NIL;

  QUIZPATH := getParamDir; { highest priority }
  if QUIZPATH='' then QUIZPATH := GetEnvironmentVariable('QUIZPATH');
  if QUIZPATH='' then 
    QUIZPATH := getprefix+DirectorySeparator+'share'+DirectorySeparator+
                'akfquiz'+PathSeparator+'.';

  {$IfDef Go32v2}
    { Compiler checks LFNsupport just on drive C: - that's stupid! :-( }
    if GetEnvironmentVariable('LFN')<>'' then setLFNsupport;

    {$IfDef ForceLFN}
      setLFNsupport;
      {$Info LFN support enforced}
    {$EndIf}
  {$EndIf}


FINALIZATION

  freeQuizFileList

end.
