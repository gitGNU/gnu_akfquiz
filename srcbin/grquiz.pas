{
* grquiz
* graphics oriented program for AKFQuiz
*
* $Id: grquiz.pas,v 1.22 2007/07/02 10:14:10 akf Exp $
*
* Copyright (c) 2005-2006,2007 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: 
* FreePascal or GNU Pascal and libSDL-1.2
* or
* GNU Pascal and GRX library with GPC support (no other addon needed)
*
* This file is part of AKFQuiz
*
* AKFQuiz is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AKFQuiz is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Windows} {$EndIf}
{$IfDef Win32} {$Define Windows} {$EndIf}

{$IfDef Windows}
  {$R w32/grquiz.res}
  {$UnDef Beeps} { important }
{$EndIf}

{$IfDef DPMI}
  {$UnDef SDL}
  {$Define Beeps}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}

  {$IfDef FPC} {$IfDef Unix} {$IfNDef SDL}
     { Under FPC + Unix SVGAlib is used }
     {$Define FPCSVGALIB}
     {$Define grNoEscKey} { cannot distiguish Esc from Esc-sequences }
     {$Define Beeps}
  {$EndIf} {$EndIf} {$EndIf}

  {$IfDef Windows}
    {$AppType GUI}
    {$Define SDL}
  {$EndIf}
{$EndIf}

{$I-}
{$X+}
{$R+} { Range checking }

program grquiz(input, output, stderr);

{$IfDef FPC}
  uses 
  {$IfNDef SDL} 
    graph, clgrph,
  {$Else} { SDL }
    sdlgrph, sdlsnd,
  {$EndIf}
  sysutils, qsys, uakfquiz, qmsgs, chconv;
{$EndIf}

{$IfDef __GPC__}
  import uakfquiz; qmsgs; qsys; chconv; 
         {$IfDef SDL}
	   sdlgrph; sdlsnd;
	 {$Else}
           graph; grx; clgrph;
	   {$Define GRX}
	 {$EndIf}
{$EndIf}


{$IfDef __GPC__}
  {$IfDef SDL}
  
  {$I quizhg.inc}
  {$IfNDef NoTitleImage} {$I titimg.inc} {$EndIf}
  
  {$Else} { not SDL }
    {$L quizhg.o}
    {$If __GPC_RELEASE__ >= 20030303}
      var AKFQuizHg: array[0..$FFFF] of byte; external name 'AKFQuizHg';
    {$Else} {@@@ FIXME: inermediate versions with other syntax }
      var AKFQuizHg: asmname 'AKFQuizHg' array[0..$FFFF] of byte;
    {$EndIf} { __GPC_RELEASE__ }

    {$IfNDef NoTitleImage}
      {$L titimg.o}
      {$If __GPC_RELEASE__ >= 20030303}
        var TitleImage:  array[0..$FFFF] of byte; external name 'TitleImage';
      {$Else} {@@@ FIXME: inermediate versions with other syntax }
        var TitleImage: asmname 'TitleImage' array[0..$FFFF] of byte;
      {$EndIf} { __GPC_RELEASE__ }
    {$EndIf} { NoTitleImage }
  {$EndIf} { not SDL }
{$EndIf} { __GPC__ }

{$IfDef FPC}
  {$I quizhg.inc}
  {$IfNDef NoTitleImage}
    {$I titimg.inc}
  {$EndIf} { NoTitleImage }
{$EndIf} { FPC }

{$I icons.inc}
{$I font.inc}

type keyset = set of char;

type TgrfImage = 
  packed record
         Width    : Sint32;
         Height   : Sint32;
         reserved : Sint32;
         Image    : array[0..ScreenWidth*ScreenHeight] of Uint16;
         end;

{ GNU compliant format }
const PrgVersion = 'grquiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const TitleImageName = 'AKFoerster';

const linespace = fontheight; { you may add something }

const AnswerKeys = [ '1'..'9', 'A'..'Z' ];

const unknownChar = 0; { inverse questionmark }

const
  Esc   = chr(27);
  Enter = chr(13);

{ Unix uses #27 + something for any function key,
  that's still so with SVGALib (not unter X11),
  so it's too difficult to filter out the Esc key as such :-( }

{$IFDEF grNoEscKey}
  const
    MaxAnswers = 25; { Q is reserved }
    ExitKey = 'Q'; { capital letter! }
    ExitKeyName = '['+ExitKey+']';
{$Else}
  const MaxAnswers = 35; { see AnswerKeys }
  const
    ExitKey = Esc;
    ExitKeyName = '[Esc]';
{$EndIf}


type
  Tgrquiz =
    object(Takfquiz)
      readerror : boolean;

      { only temporarily used: }
      AnsPoints : array[1..MaxAnswers] of pointsType;

      constructor init(infile: string);
      procedure resetQuiz;                        virtual;
      procedure wait;
      procedure StartQuiz;                        virtual;
      procedure setcharset(cs: string);           virtual;
      procedure writeQuizLn(const s: string);
      procedure processParagraph;
      procedure processComment;                   virtual;
      procedure processHint;                      virtual;
      procedure processAssessment;                virtual;
      procedure processAssessmentPercent;         virtual;
      procedure processQuestion;                  virtual;
      procedure processMulti;                     virtual;
      function  getAnswer(keys: keyset): integer;
      procedure processAnswer;                    virtual;
      procedure processMultiAnswer;               virtual;
      function  showAnswers(showdefault: boolean): boolean;
      procedure nextanswer;
      procedure evaluate;                         virtual;
      procedure EndQuiz;                          virtual;
      procedure error;                            virtual;
    end;

var infile: mystring;
var quiz: Tgrquiz;

var TextColor, BkColor, Color: word;

var 
  MaxX, MaxY : TscreenPos;
  MaxLength  : TscreenPos;

var
  loop       : boolean;
  unstopable : boolean;
  fullscreen : boolean;
  useSound   : boolean;
  justTitle  : boolean;
  usemouse   : boolean;


procedure buildscreen;
begin
drawBackground(AKFQuizHg);

TextColor := GetPixel(TextColorX, TextColorY);
BkColor   := GetPixel(BackColorX, BackColorY);

Color := TextColor;
setColors(TextColor, BkColor)
end;


procedure initializeGraphics;
begin
initializeGraphicMode(PrgVersion, AKFQuizName, fullscreen); 
setExitKey(ExitKey);

buildscreen;

defineTextArea(marginLeft, marginTop, 
               GetMaxX-marginRight, GetMaxY-marginBottom, useTextArea);
MaxX := GetMaxX - marginLeft - marginRight;
MaxY := GetMaxY - marginTop - marginBottom;

setUnknownChar(unknownChar);
setmsgconverter(noconversion) { we use a UTF-8 capable GrfWrite }
end;

procedure WriteUStr(const s: UnicodeString);
var 
  i, ch : integer;
  x, y: TscreenPos;
  ly, lx: byte;
begin
x := GetX;
y := GetY;

LockScreen;
for i := 1 to s.length do
  begin
  if s.content[i]=13 then continue;
  if s.content[i]=10 then 
    begin x := 0; inc(y, linespace); continue end;
  
  ch := UnicodeToFont(s.content[i], unknownChar);
  for ly:=1 to fontheight do
    for lx:=0 to 7 do
      if (font[ch, ly] and (1 shl (7-lx)))<>0 then 
         PutPixel(x+lx, y+ly, color);
  inc(x, 8)
  end;
UnlockScreen;

{ move to end of line }
MoveTo(x, y)
end;

procedure WriteUStrRTL(const s: UnicodeString);
var 
  i, ch : integer;
  x, y: TscreenPos;
  ly, lx: byte;
begin
x := GetX;
y := GetY;

if x=0 then x := MaxX-8;

LockScreen;
for i := s.length downto 1 do
  begin
  if s.content[i]=13 then continue;
  if s.content[i]=10 then 
    begin x := MaxX-8; inc(y, linespace); continue end;
  
  ch := UnicodeToFont(s.content[i], unknownChar);
  for ly:=1 to fontheight do
    for lx:=0 to 7 do
      if (font[ch, ly] and (1 shl (7-lx)))<>0 then 
         PutPixel(x+lx, y+ly, color);
  dec(x, 8)
  end;
UnlockScreen;

{ move to end of line }
MoveTo(x, y)
end;

procedure GrfWrite(const s: string);
begin
if s<>'' then WriteUStr(UTF8ToUStr(s))
end;

procedure GrfWriteRTL(const s: string);
begin
if s<>'' then WriteUStrRTL(UTF8ToUStr(s))
end;

function GetTextWidth(const s: string): integer;
begin
GetTextWidth := Length(s)*8
end;

function waitkey(const s: string): boolean;
var c: char;
begin
if GetY=0 
  then waitkey := true
  else begin
       MoveTo(0, MaxY - fontheight);
       GrfWrite(s);
       c := GetKey;
       ClearTextArea;
       waitkey := ( Upcase(c) <> ExitKey )
       end
end;

procedure GrLn;
begin
MoveTo(0, GetY + linespace);

if GetY >= MaxY - (2*linespace) then
  begin
  MoveTo(0, GetY + linespace);
  waitkey(msg_anykey(''))
  end
end;

procedure GrfWriteLn(const s: string);
begin GrfWrite(s); GrLn end;

procedure GRReadLn(var s: string);
var
  c: char;
  cwidth: integer;
begin
s := '';
repeat
  c := GetKey;
  if (c >= #32) and ( c<>#127) then { normal char }
    begin
    s := s + c;
    GrfWrite(c)
    end;
  if ((c=#127) or (c=#8)) and (s<>'') then
     begin
     c := s[length(s)]; { get last char }
     s := copy(s, 1, Length(s)-1); { remove last char from s }
     cwidth := GetTextWidth(c); { automatic typecast }
     MoveTo(GetX - cwidth, GetY);
     Color := BkColor;
     GrfWrite(c);
     MoveTo(GetX - cwidth, GetY);
     Color := TextColor
     end;
  if not ((c in [#32 .. #255, #10, #13]) or (c = ExitKey)) then ErrorSignal
until (c=#13) or (c=#10) or (c=ExitKey);

if c = ExitKey then s := '' { return nothing }
end;

procedure Symbol(correct: boolean);
var 
  x, y, sx, sy: TscreenPos;
  col : word;
begin
if correct 
  then col := GetRGBColor($00, $AA, $00)   { green }
  else col := GetRGBColor($AA, $00, $00);  { red }

x := GetX + 16; { a little behind the text }
y := GetY;      { same upper limit as text-line }

LockScreen;
for sy := 1 to 18 do
  for sx := 1 to 18 do
    if (correct and (icon_right[sy, sx] <> ' ')) or 
       (not correct and (icon_wrong[sy, sx] <> ' ')) then 
      PutPixel(x+sx, y+sy, col);
UnlockScreen;
GrLn
end;

{$IfDef GRX}

  procedure getImageSize(protected var img; var width, height: integer);
  var maxval: integer;
  begin
  GrQueryPnmBuffer(img, width, height, maxval)
  end;

{$Else}

  procedure getImageSize(const img; var width, height: integer);
  begin
  width := TgrfImage(img).Width;
  height := TgrfImage(img).Height
  end;

{$EndIf}


{$IfNDef NoTitleImage}

  procedure showTitleImage;
  var width, height: integer;
  begin
  getImageSize(TitleImage, width, height);

  {$IfDef SDL}
    ShowTransparentImage(MaxX-width, 0, TitleImage, TitleImage.Image[0]);
  {$Else}
    ShowImage(MaxX-width, 0, TitleImage);
  {$EndIf}

  MoveTo(MaxX-(width div 2)-(Length(TitleImageName) * 8 div 2), height+3);
  GrfWrite(TitleImageName);
  MoveTo(0, 0);
  end;
{$EndIf} { NoTitleImage }

procedure InfoScreen;
begin
{$IfNDef NoTitleImage}
  ShowTitleImage;
{$EndIf} { NoTitleImage }

GrfWriteLn(PrgVersion);
GrfWriteLn('Copyright © ' + AKFQuizCopyright);
GrLn;
case lang of
  deutsch : GrfWriteLn('Beenden: '+ExitKeyName
             {$IfDef SDL} +' - Vollbild-Modus umschalten: [F11]' {$EndIf}
             );
  otherwise GrfWriteLn('Stop: '+ExitKeyName
             {$IfDef SDL} +' - toggle fullscreen mode: [F11]' {$EndIf}
             );
  end;
GrLn;
GrfWriteLn(msg_contributions);
GrfWriteLn(Contributors);
GrLn;
GrfWriteLn(msg_License + msg_GPL);
GrfWriteLn(msg_noWarranty);
GrLn;

{$IfDef Advertisement}
  GrfWriteLn(msg_advertisement);
{$EndIf}

GrfWrite(msg_homepage);

IntroSignal;

if unstopable or justTitle
  then waitkey(msg_anykey(''))
  else if not waitkey(msg_anykey(ExitKeyName)) then 
          begin
          endGraphics;
          Halt
          end
end;

procedure CharsetScreen;
var
  ch : integer;
  x, y: TscreenPos;
  ly, lx: byte;
begin
x := GetX;
y := GetY;
LockScreen;
for ch:=0 to fontMaxChar do
  begin
  for ly:=1 to fontheight do
    for lx:=0 to 7 do
      if (font[ch, ly] and (1 shl (7-lx)))<>0 then 
         PutPixel(x+lx, y+ly, color);
  inc(x, 8);
  
  if ((ch+1) mod 32) = 0 then
    begin
    inc(y, fontheight);
    x := 0;
    end
  end;
UnlockScreen;
MoveTo(x, y);
GrLn;
waitkey(msg_anykey(''))
end;

function askfile: mystring;
var
  Nr, offset : word;
  c, maxkey : char;
  okay: boolean;
  keys : keyset;
  fileMaxY: TscreenPos;
  entry : PquizfileList;
begin
okay := true;
offset := 0;
Nr := 1;

{$IfDef Advertisement}
  GrfWriteLn(msg_advertisement);
{$EndIf}
GrLn;

if quizfileList=NIL then fillQuizfileList;
entry := quizfileList;

{ leave enough space at the bottom 
  also for "more..." }
fileMaxY := MaxY - (6*linespace);

{ no entries }
if entry=NIL then 
  begin
  GrfWriteLn(msg_noquizfound);
  GrLn;
  waitkey(msg_anykey(''));
  Halt
  end;

{ just one entry -> don't ask stupid questions }
if entry^.next=NIL then
  begin
  askfile := entry^.filename;
  ClearTextArea;
  exit
  end;
  
okay := false;
while (entry<>NIL) and (not okay) do
  begin
  while (entry<>NIL) and (Nr<=MaxAnswers-1) and (GetY<fileMaxY) do
    begin
    answerstarts(Nr);
    with entry^ do
      if language=''
        then GrfWriteLn(ValueToKey(Nr)+') '
	                + copy(entry^.title, 1, MaxLength-3))
        else GrfWriteLn(ValueToKey(Nr)+') ' + language + ': '
	                + copy(entry^.title, 1, 
			            MaxLength-5-length(language)));
    answerends(Nr);
    entry := entry^.next;
    inc(Nr)
    end;

  if (entry=NIL) and (offset=0)
    then dec(Nr) { all entries shown }
    else begin { more... }
         answerstarts(Nr);
         GrfWriteLn(ValueToKey(Nr)+') '+msg_more);
         answerends(Nr)
         end;

  maxKey := ValueToKey(Nr);
  if Nr<10 
    then keys := [ '1'..maxKey, ExitKey ]
    else keys := [ '1'..'9', 'A'..maxKey, ExitKey ];

  GrLn;
  QuestionSignal;
  GrfWrite('> ');
  
  repeat
    c := Upcase(GetKey);
    okay := c in keys;
    if not okay then ErrorSignal;
  until okay;
  
  if (c=maxkey) and ((entry<>NIL) or (offset<>0)) then { more...}
    begin
    okay := false;
    offset := word(offset + Nr - 1);
    Nr := 1;
    ClearTextArea;
    if entry=NIL then { start from the beginning }
      begin
      entry := quizfileList;
      offset := 0
      end
    end
  end;

if (c=ExitKey) and not unstopable then
   begin
   endGraphics;
   Halt
   end;

ClearTextArea;

{ get the result }
askfile := searchQuizfileList(KeyToValue(c) + offset)
end;

{---------------------------------------------------------------------}
constructor Tgrquiz.init(infile: string);
begin
inherited init(infile);
showmouse(usemouse)
end;

procedure Tgrquiz.resetQuiz;
begin
inherited resetQuiz;
readerror := false
end;

procedure Tgrquiz.setcharset(cs: string);
begin
inherited setcharset(cs);

cs := makeUpcase(cs);

if checkOEM(cs)    then setconverter(OEMtoUTF8);
if checkUTF8(cs)   then setconverter(noconversion);
if checkCP1252(cs) then setconverter(CP1252ToUTF8);
if checkASCII(cs)  then setconverter(forceASCII);
if checkKOI8R(cs)  then setconverter(KOI8RToUTF8);
if checkKOI8U(cs)  then setconverter(KOI8UToUTF8);
if checkKOI8RU(cs) then setconverter(KOI8RUToUTF8);

if checkISO(cs) then
  case whichISO(cs) of
     0: error;
     1: setconverter(ISO1ToUTF8);
     2: setconverter(ISO2ToUTF8);
     3: setconverter(ISO3ToUTF8);
     4: setconverter(ISO4ToUTF8);
     5: setconverter(ISO5ToUTF8);
     6: setconverter(ISO6ToUTF8);
     7: setconverter(ISO7ToUTF8);
     8: setconverter(ISO8ToUTF8);
     9: setconverter(ISO9ToUTF8);
    10: setconverter(ISO10ToUTF8);
    11: setconverter(ISO11ToUTF8);
    13: setconverter(ISO13ToUTF8);
    14: setconverter(ISO14ToUTF8);
    15: setconverter(ISO15ToUTF8);
    16: setconverter(ISO16ToUTF8);
    end
end;

procedure Tgrquiz.writeQuizLn(const s: string);
var e : UnicodeString;
begin
if s='' then begin GrLn; exit end;

e := UTF8ToUStr(s);

if BiDi then handleBiDi(e);

if rtl 
  then WriteUStrRTL(e)
  else WriteUStr(e); 
GrLn
end;

procedure Tgrquiz.wait;
begin
if not quit then
   quit := not waitkey(msg_anykey(ExitKeyName));
end;

procedure Tgrquiz.StartQuiz;
begin
inherited StartQuiz;

{@@@ RTL may be different }
ClearTextArea;
GrLn;
GrLn;
GrfWriteLn(msg_quiz + title);

if author<>'' then
  GrfWriteLn(msg_author + author);
if copyright<>'' then
  GrfWriteLn('Copyright: ' + copyright);
if authorURI<>'' then
  GrfWriteLn(msg_authorURI + authorURI);
if translator<>'' then
  GrfWriteLn(msg_translator + translator);
if edited<>'' then
  GrfWriteLn(msg_edited + edited);
if license<>'' then
  GrfWriteLn(msg_license + license);
if licenseURI<>'' then
  GrfWriteLn(msg_licenseURI + licenseURI);
if timelimit>0 then
  GrfWriteLn(msg_timelimit + showTime(timelimit));
GrLn;
{$IfDef Advertisement}
  GrfWriteLn(msg_advertisement);
  GrLn;
{$EndIf}

wait;
StartTime := GetSecs
end;

procedure Tgrquiz.processParagraph;
var s, rest, outs : mystring;
begin
outs := '';
s := readLine;
while s<>'' do
   begin
   if s='.'
     then begin
          WriteQuizLn(outs);
	  outs := '';
	  GrLn
          end
     else begin
          rest := s;
          while rest<>'' do
	    begin
	    if outs=''
	      then outs := format(rest, MaxLength-Length(outs), MaxLength)
	      else outs := outs + ' ' +
                           format(rest, MaxLength-Length(outs), MaxLength);
            if rest<>'' then
	      begin
	      WriteQuizLn(outs);
	      outs := ''
	      end
	    end
          end;
   s := readLine
   end;
WriteQuizLn(outs)
end;

procedure Tgrquiz.processComment;
begin
ClearTextArea;
processParagraph;
GrLn;
wait
end;

procedure Tgrquiz.processHint;
begin
processComment { handle like a Comment }
end;

procedure Tgrquiz.processAssessment;
begin
processComment
end;

procedure Tgrquiz.processAssessmentPercent;
var rest: mystring;
begin
rest := readAssessmentPercent;
while rest<>'' do
    GrfWriteLn(format(rest, MaxLength, MaxLength));
GrLn;
wait
end;

procedure Tgrquiz.processQuestion;
begin
ClearTextArea;
inherited processQuestion;

processParagraph;
processAnswer
end;

procedure Tgrquiz.processMulti;
begin
ClearTextArea;
inherited processMulti;

processParagraph;
processMultiAnswer
end;

procedure Tgrquiz.nextanswer;
begin
if answerNr>0 then answerEnds(answerNr);
inc(answerNr);

if answerNr>MaxAnswers then 
   begin 
   error; 
   answerNr := MaxAnswers { to avoid range overruns }
   end;
answerStarts(answerNr);
end;

function Tgrquiz.getAnswer(keys: keyset): integer;
var
  c : char;
  okay: boolean;
begin
repeat
  c := Upcase(GetKey);
  okay := c in keys;
  if not okay then ErrorSignal;
until okay;

if c=ExitKey then quit := true;

if c in AnswerKeys
  then begin GrfWrite(c); getAnswer := KeyToValue(c) end
  else getAnswer := -1
end;

function Tgrquiz.showAnswers(showdefault: boolean): boolean;
var
  s, ans: mystring;
  value: pointsType;
  startpos: TscreenPos;
  answerMaxY: TscreenPos;
begin
GrLn;

{ leave enough space at the bottom 
  also for "more..." and the default answer }
answerMaxY := MaxY - (8*linespace);

{ enough space for at least starting the answer section? }
if GetY >= answerMaxY then wait;

answerNr := 0;
readAnswer(value, s);
while (s <> '') and (GetY < answerMaxY) and (answerNr <= MaxAnswers-1) do
  begin
  ans := s;
  nextanswer;
  AnsPoints[answerNr] := value;
  if rtl 
    then GrfWriteRTL(' (' + ValueToKey(answerNr))
    else GrfWrite(ValueToKey(answerNr) + ') ');
  startpos := GetX;
  WriteQuizLn(format(ans, MaxLength-3, MaxLength-3));
  while ans<>'' do
     begin
     MoveTo(startpos, GetY);
     WriteQuizLn(format(ans, MaxLength-3, MaxLength-3))
     end;
  if GetY<answerMaxY 
    then readAnswer(value, s)
    else 
      if nextLineEmpty 
        then s := ''
        else s := '...' { Dummy }
  end;

if s <> ''
  then begin { more answers to be shown }
       nextanswer;
       AnsPoints[answerNr] := 0;
       if rtl 
         then GrfWriteRTL(msg_more + ' (' + ValueToKey(answerNr))
         else GrfWrite(ValueToKey(answerNr) + ') ' + msg_more);
       GrLn
       end
  else if showdefault and (defanswer <> '') then { show default-Answer }
         begin
         ans := defanswer;
         nextanswer;
         AnsPoints[answerNr] := 0;
         if rtl 
           then GrfWriteRTL(' (' + ValueToKey(answerNr))
           else GrfWrite(ValueToKey(answerNr) + ') ');
         startpos := GetX;
	 WriteQuizLn(format(ans, MaxLength-3, MaxLength-3));
         while ans <> '' do
           begin
           MoveTo(startpos, GetY);
	   WriteQuizLn(format(ans, MaxLength-3, MaxLength-3))
           end { while }
         end; { if defanswer }

{ all answers shown already? }
showAnswers := (s = '')
end;

procedure Tgrquiz.processAnswer;
var
  maxKey: char;
  a : integer;
  complete: boolean;
 
  procedure handleResult;
  var ap : pointsType;
  begin
  ap := AnsPoints[a];
  inc(Points, ap);
  
  if not neutral then
    if ap > 0
      then begin GrfWrite(msg_right); Symbol(true);  RightSignal end
      else begin GrfWrite(msg_wrong); Symbol(false); FalseSignal end;

  if neutral then NeutralSignal;
  
  GrLn;
  GrfWrite(msg_points + IntToStr(Points));
  
  if not complete then { skip other answers }
    begin
    repeat until readline = '';
    complete := true
    end;
  
  wait
  end;

begin { Tgrquiz.processAnswer }
repeat
  complete := showanswers(true);

  answerEnds(answerNr);

  GrLn;

  GrfWrite('> ');

  maxKey := ValueToKey(answerNr);
  if answerNr < 10 
     then a := getAnswer([ '1'..maxKey, ExitKey ])
     else a := getAnswer([ '1'..'9', 'A'..maxKey, ExitKey ]);

  GrLn;

  if not complete and (a=answerNr) { answer is "..."? }
     then ClearTextArea 
     else if (a <> -1) and not quit 
             then handleResult

until complete or quit;
end;

procedure Tgrquiz.processMultiAnswer;
var a: integer;
    keys: keyset;
    maxKey: char;
    myPoints, myMax : pointsType;
    complete : boolean;
begin
myPoints := 0;
myMax := 0;

repeat
  complete := showanswers(false);
  answerEnds(answerNr);

  GrLn;
  GrfWrite('>> ');

  { max points for this one question }
  for a := 1 to answerNr do
    if AnsPoints[a] > 0 then inc(myMax, AnsPoints[a]);

  maxKey := ValueToKey(answerNr);
  if answerNr<10 
     then keys := [ '1'..maxKey, ExitKey, Enter ]
     else keys := [ '1'..'9', 'A'..maxKey, ExitKey, Enter ];

  repeat {@@@}
    a := getAnswer(keys);
    if not complete and (a=answerNr) { answer is "..."? }
       then a:=-1
       else if a<>-1 then
              begin
              inc(Points, AnsPoints[a]);   { absolute points }
              inc(myPoints, AnsPoints[a]); { points for this question }
              { remove that key from valid keys }
              keys := keys - [ ValueToKey(a) ]
              end;
  until a = -1;
  if not complete and not quit then ClearTextArea
until complete or quit;

if not quit then NeutralSignal;
GrLn; GrLn;
GrfWrite(msg_points + IntToStr(Points) +
         '  (' + IntToStr(myPoints) +
	 '/' + IntToStr(myMax) + ')');
wait
end;

procedure Tgrquiz.evaluate;
begin
ClearTextArea;
if not evaluated and not quit and (MaxPoints<>0) then
  begin
  { if checkTimeout then GrfWriteLn(msg_timeout); }
  GrfWriteLn(msg_sol1 + IntToStr(Points) + msg_sol2 +
            IntToStr(MaxPoints) + msg_sol3);
  If Points > 0
    then GrfWriteLn(msg_sol4 + IntToStr(getPercentage) + '%.')
    else if not neutral then GrfWriteLn(msg_sol5);
    
  { show time }
  if not neutral then
    begin
    GrLn;
    GrfWriteLn(msg_time + ShowTime(GetSecs - StartTime))
    end
  end;
inherited evaluate;
GrLn;

InfoSignal;
wait
end;

procedure Tgrquiz.EndQuiz;
begin
if not evaluated and not quit
  then evaluate
end;

procedure Tgrquiz.error;
begin
readerror := true;
quit := true
end;


{---------------------------------------------------------------------}

procedure version;
begin
setmsgconv(checkDisplay);
WriteLn(PrgVersion);
WriteLn;
WriteLn('Copyright (C) ', AKFQuizCopyright);
WriteLn('Copyright (C) 1999-2001 Free Software Foundation, Inc.');
WriteLn('Copyright (C) 2004 by the contributors to kbd');
WriteLn;
WriteLn(msg_License, msg_GPL);
{$IfDef Advertisement}
  WriteLn;
  WriteLn(msg_advertisement);
{$EndIf}
WriteLn;
WriteLn(msg_noWarranty);
WriteLn;
WriteLn('Written by Andreas K. Foerster');
WriteLn;
WriteLn(msg_contributions);
WriteLn(Contributors);
Halt
end;

procedure help;
begin
setmsgconv(checkDisplay);
WriteLn('graphical quiz-program');
WriteLn;
WriteLn('Usage: grquiz [options] [file.akfquiz]');
WriteLn(' or:   grquiz -h | --help | /?');
WriteLn(' or:   grquiz --version');
WriteLn;
WriteLn('Options:');
WriteLn(' -f, --fullscreen   fullscreen mode (if supported)');
WriteLn(' -w, --window       window mode (if supported)');
WriteLn(' -s                 no sound');
WriteLn(' -m, --nomouse      no mouse');
WriteLn(' -d <dir>           path to quizfiles');
WriteLn(' -1                 run just once');
WriteLn(' -p                 unstopable (use with care!)');
WriteLn(' -t, --title        show just the title screen');
{$IfDef FPC} {$IfDef Go32v2}
WriteLn(' -LFN               use long filenames (DOS only)');
{$EndIf} {$EndIf}

WriteLn;
WriteLn('The environment-variable QUIZPATH can be used for setting');
WriteLn('a searchpath to the quizfiles');
WriteLn;
WriteLn('QUIZPATH="', getQuizPath, '"');

if BugMail <> ''
  then begin WriteLn; WriteLn('Report bugs to <' + BugMail + '>.') end;

Halt
end;

procedure ErrorFileNotFound;
begin
endGraphics;
WriteLn(stderr, msg_filenotfound);
Halt(2)
end;

procedure parameters;
var
  i: integer;
  p: mystring;
begin
i := 0;
while i<ParamCount do
  begin
  inc(i);
  p := makeUpcase(ParamStr(i));
  if p='-LFN' then
      begin setLFNsupport; continue end;
  if p='-S' then
      begin DisableSignals; useSound := false; continue end;
  if (p='-D') then begin inc(i); { handled in qsys } continue end;
  if (p='-1') then begin if not unstopable then loop:=false; continue end;
  if (p='-P') or (p='/P') then
     begin loop := true; unstopable := true; nobreak; continue end;
  if (p='-F') or (p='--FULLSCREEN') then
     begin fullscreen := true; continue end;
  if (p='-W') or (p='--WINDOW') then
     begin fullscreen := false; continue end;
  if (p='-M') or (p='--NOMOUSE') then
     begin usemouse := false; continue end;
  if (p='-T') or (p='--title') then
     begin justTitle := true; continue end;
  if (p='-H') or (p='--HELP') or (p='/?') then help;
  if (p='--VERSION') then version;
  if p[1]='-'    { "/" might be used in a path }
     then help { unknown parameter }
     else infile := ParamStr(i) { not Upcase }
  end
end;


var myexitcode : byte;

begin { main }
ident('$Id: grquiz.pas,v 1.22 2007/07/02 10:14:10 akf Exp $');

{$IfDef FPCSVGALIB}
  { space after messages from SVGALib }
  WriteLn;
  WriteLn;
{$EndIf}

myexitcode := 0;
loop       := true;
unstopable := false;
fullscreen := false;
useSound   := true;
justTitle  := false;
usemouse   := true;

useSystemLanguage;
parameters;

{$IfDef Beeps}
  if useSound then useBeepSignals; { no beeps possible in most cases }
  {$M Beeps}
{$EndIf}

InitializeGraphics;

showmouse(usemouse);
MaxLength := (GetMaxX-(2*40)) div GetTextWidth('m'); { largest letter }

{$IfDef SDL}
  if useSound then 
    begin
    GrfWriteLn(msg_InitAudio);
    ShowTextArea;
    InitAudio(true); { initialize as sub-system }
    ClearTextArea
    end;
{$EndIf}

{ CharsetScreen; }
InfoScreen;

if justTitle then
  begin
  endGraphics;
  Halt
  end;

repeat
  if infile<>'' then
     if not getquizfile(infile) then ErrorFileNotFound;

  if infile='' then
    repeat
      infile := askfile;
      if not unstopable and (infile='') 
         then begin endGraphics; Halt end;
    until (infile<>'') and getquizfile(infile);

  quiz.Init(infile);

  quiz.process;
  { exitcode 1 reserved for graphical error! }
  if quiz.readerror then begin myexitcode:=2; loop:=false end;
  if quiz.stopQuiz and not unstopable then loop:=false;
  ClearTextArea;
  quiz.Done;

  infile := '';

  if loop then 
     begin
     parameters; { to get input files again }
     InfoScreen
     end;
until not loop;

endGraphics;
if myexitcode=2 then WriteLn(stderr, msg_error);

Halt(myexitcode)
end.
