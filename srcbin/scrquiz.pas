{
* scrquiz   (was crtquiz)
* screen/terminal oriented quiz program
*
* $Id: scrquiz.pas,v 1.14 2006/11/15 15:37:28 akf Exp $
*
* Copyright (c) 2003-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
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
*
*}

{ Conditional Defines: 
  NoHide : don't hide Cursor
}

{$X+}
{$R+} { Range checking }

{ compatiblity definition }
{$IfDef _WIN32} {$Define Windows} {$EndIf}
{$IfDef Win32} {$Define Windows} {$EndIf}

{$IfDef Windows}
  {$R w32/scrquiz.res}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}

  {$IfDef Windows}
    {$AppType Console}
  {$EndIf}
{$EndIf}

{ FPC code for hiding the cursor causes trouble on BSD systems }
{$IfDef BSD} {$Define NoHide} {$EndIf}

program scrquiz(input, output);
uses qsys, qmsgs, uakfquiz, crt
{$IfDef SdlSoundForAll}
  ,sdlsnd
{$EndIf}
;

{$I-}

type keyset = set of char;

type TscreenPos = {$IfDef FPC} byte; {$Else} cardinal; {$EndIf}
{ Byte is a bad choice, but FPC insists on it }

{ GNU compliant format }
const PrgVersion = 'scrquiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const 
  Esc   = chr(27);
  Enter = chr(13);

{$IfDef scrNoEscKey} { Esc may cause problems }
const MaxAnswers = 25; { Q is reserved }
const 
  ExitKey     = 'Q';
  ExitKeyName = '['+ExitKey+']';
{$Else}
const MaxAnswers = 35;
const 
  ExitKey     = Esc;
  ExitKeyName = '[Esc]';
{$EndIf}

const
  HeadTxtColor     = yellow;
  HeadTxtBack      = blue;
  HeadColor        = blue;
  HeadBackground   = lightgray;
  TxtColor         = black;
  TxtBackground    = lightgray;
  QuestionColor    = blue;
  waitcolor        = brown;
  falsecolor       = red;
  truecolor        = green;
  errorcolor       = red;
  LogoColor        = lightgray;
  LogoShadow       = black;

const AnswerKeys = [ '1'..'9', 'A'..'Z' ];

var loop, unstopable : boolean;

var ScrWidth, ScrHeight : TscreenPos;

{ maximum Length and Height of textarea }
var MaxLength, MaxHeight: TscreenPos;

{ displaytype }
var display: DisplayType;

{ Line count after last wait }
var LineCnt: cardinal;

type 
  Tscrquiz = 
    object(Takfquiz)
      readerror : boolean;

      { only temorarily used: }
      AnsPoints : array[1..MaxAnswers] of pointsType;

      destructor Done;                            virtual;

      procedure ResetQuiz;                        virtual;
      procedure StartQuiz;                        virtual;
      procedure setcharset(cs: string);           virtual;
      procedure processComment;                   virtual;
      procedure processHint;                      virtual;
      procedure processQuestion;                  virtual; 
      procedure processMulti;                     virtual;
      procedure processAnswer;                    virtual;
      procedure processMultiAnswer;               virtual;
      procedure processAssessment;                virtual;
      procedure processAssessmentPercent;         virtual;
      procedure evaluate;                         virtual;
      procedure EndQuiz;                          virtual; 
      procedure error;                            virtual;
      procedure NewScreen;
      procedure wait;
      procedure paragraph;
      procedure showanswers(showdefault: boolean);
      procedure nextanswer;
      function getanswer(keys: keyset): integer;
    end;

var infile: mystring;
var quiz: Tscrquiz;



procedure normscreen;
begin
window(1,1,ScrWidth,ScrHeight);
NormVideo;
ClrScr;
MaxLength := ScrWidth;
MaxHeight := ScrHeight;
LineCnt   := 0
end;

procedure errorFileNotFound;
begin
normscreen;
WriteLn(msg_filenotfound);
Halt(1)
end;
	  
procedure Ln;
begin
WriteLn;
inc(LineCnt)
end;

function toodeep: boolean;
begin
toodeep := LineCnt >= MaxHeight
end;

procedure clearbuffer;
var c: char;
begin
while KeyPressed do c := ReadKey
end;

function waitkey(const s: string): boolean;
var 
  c : char;
  oldcolor: byte;
begin

{ compatiblity GPC/FPC: }
{$IfDef __GPC__}
  {$Define CursorOff HideCursor}
  {$Define CursorOn  NormalCursor}
{$EndIf}

if WhereX<>1 then Ln;

{ clear buffer }
{ needed for some nasty compiler-bug for BSD ? }
clearbuffer;

oldcolor := TextAttr;
TextColor(waitcolor);
if length(s) <= MaxLength { avoid linebreak here }
  then Write(s)
  else Write(copy(s, 1, MaxLength));
TextAttr := oldcolor;

{$IfNdef NoHide}
  CursorOff;
{$EndIf}

c := ReadKey;
if c=#0 then ReadKey;
  
GotoXY(1, WhereY);
DelLine;
LineCnt := 0;

{$IfNdef NoHide}
  CursorOn;
{$EndIf}

waitkey := (Upcase(c) <> ExitKey)
end;

procedure centered(x: string);
begin
if length(x) > MaxLength then x := copy(x, 1, MaxLength);
GotoXY( TscreenPos((MaxLength div 2) - (Length(x) div 2)), WhereY);
Write(x);
Ln
end;

procedure Logo(s: string);
var 
  x, y: TscreenPos;
  i: integer;
  wide: boolean;
const
  BlockChar = '*';
  LColor  = ((LogoColor  and $7F) shl 4) or (LogoColor  and $7F);
  LShadow = ((LogoShadow and $7F) shl 4) or (LogoShadow and $7F);
  { set color & background to the same value => block }
  { enforce lowvideo, because highvideo cannot be used for background }
begin
wide := ScrWidth >= 80; 

{ draw shadow }
TextAttr := LShadow;
y := TscreenPos(WhereY + 1);
if wide 
  then x := TscreenPos((ScrWidth div 2) - (Length(s)+1))
  else x := TscreenPos((ScrWidth div 2) - (Length(s) div 2)+1);

for i := 1 to Length(s) do
  begin
  if s[i]<>' ' then
    begin
    GotoXY(x, y);
    Write(BlockChar);
    if wide then Write(BlockChar)
    end;
  if wide then inc(x, 2) else inc(x)
  end;

{ draw foreground }
TextAttr := LColor;
y := TscreenPos(WhereY - 1);
if wide 
  then x := TscreenPos((ScrWidth div 2) - (Length(s)+1))
  else x := TscreenPos((ScrWidth div 2) - (Length(s) div 2)+1);

for i := 1 to Length(s) do
  begin
  if s[i]<>' ' then
    begin
    GotoXY(x, y);
    Write(BlockChar);
    if wide then Write(BlockChar)
    end;
  if wide then inc(x, 2) else inc(x)
  end;
WriteLn
end;

procedure titlescreen;
begin
normscreen;

TextColor(HeadTxtColor);
TextBackground(HeadTxtBack);

ClrScr;

{ is the display large enough? }
if (ScrHeight<21) or (ScrWidth<30) 
  then begin
       GotoXY(1, TscreenPos(Max((ScrHeight div 2)-1, 1)));
       centered(AKFQuizName + ' ' + AKFQuizVersion);
       WriteLn
       end
  else begin
       GotoXY(1, TscreenPos(Max((ScrHeight div 2)-8, 1)));
       Logo('   #    #    #  #######');
       Logo('  # #   #   #   #      ');
       Logo(' #   #  #  #    #      ');
       Logo('#     # ###     #####  ');
       Logo('####### #  #    #      ');
       Logo('#     # #   #   #      ');
       Logo('#     # #    #  #      ');
       WriteLn;
       WriteLn;
       Logo(' #####            #        ');
       Logo('#     #                    ');
       Logo('#     #  #    #   #  ######');
       Logo('#     #  #    #   #      # ');
       Logo('#   # #  #    #   #    ##  ');
       Logo('#    #   #    #   #   #    ');
       Logo(' #### #   #### #  #  ######');
       WriteLn;
       WriteLn
       end;

TextColor(HeadTxtColor);
TextBackground(HeadTxtBack);
centered('Copyright (C) '+AKFQuizCopyright);
centered(msg_GPL);
{$IfDef Advertisement}
  centered(msg_advertisement);
{$EndIf}
{$IfDef Windows}
  case lang of
    deutsch: centered('ALT + Enter = Vollbildmodus');
    otherwise centered('Alt + Enter = Fullscreen mode')
    end;
{$EndIf}

IntroSignal;

if not waitKey('') and not unstopable then 
   begin
   normscreen;
   Halt
   end
end;

procedure buildscreen(title: string);
var sideborder, topborder, bottomborder: Tscreenpos;
begin
normscreen;

TextColor(HeadTxtColor);
TextBackground(HeadTxtBack);
ClrScr;

if ScrHeight>17
   then begin topborder := 5; bottomborder := 1 end
   else begin topborder := 2; bottomborder := 0 end;

if ScrWidth<40 
   then begin sideborder:=0; bottomborder:=0 end
   else sideborder := 2;

if topborder=2
  then centered(title)
  else begin
       WriteLn;
       TextColor(HeadColor);
       TextBackground(HeadBackground);
       if Length(title)+6 <= MaxLength
          then centered('   '+title+'   ')
          else centered(title);
       TextColor(HeadTxtColor);
       TextBackground(HeadTxtBack);
       WriteLn;
       if ScrWidth>=70 
          then centered('Program: '+AKFQuizName+', version '+AKFQuizVersion+
                ' by AKFoerster')
          else centered(AKFQuizName)
       end;

{ wider visible border }
window(TscreenPos(sideborder+1), 
       topborder, 
       TscreenPos(ScrWidth-sideborder), 
       TscreenPos(ScrHeight-bottomborder));

TextColor(TxtColor);
TextBackground(TxtBackground);
ClrScr;
{ real window }
if sideborder<>0 then inc(sideborder);
window(TscreenPos(sideborder+1), 
       topborder, 
       TscreenPos(ScrWidth-sideborder), 
       TscreenPos(ScrHeight-bottomborder));
{ one column wider than MaxLength, to avoid automatic linebreaks }

MaxLength := TscreenPos(ScrWidth - (2*sideborder) - 1); { see comment above }
MaxHeight := TscreenPos(ScrHeight - topborder - bottomborder - 1);
LineCnt   := 0
end;

{---------------------------------------------------------------------}
procedure Tscrquiz.resetQuiz;
begin
inherited resetQuiz;

readerror := false
end;

destructor Tscrquiz.Done;
begin
inherited Done;

normscreen;
if readerror then
  begin
  TextColor(errorcolor);
  WriteLn(msg_fileerror);
  NormVideo
  end
end;

{ bug workaround }
procedure Tscrquiz.NewScreen;
begin
{ buildscreen(title); } { redraw everything }

ClrScr; { still problematic for GPC } {@@@}
LineCnt := 0
end;

procedure Tscrquiz.wait;
begin
if not quit then
  if ScrWidth<80 
     then quit := not waitkey(msg_anykey(''))
     else quit := not waitkey(msg_anykey(ExitKeyName))
end;

procedure Tscrquiz.StartQuiz;
begin
inherited StartQuiz;
buildscreen(title);

ClrScr;
Ln; Ln; Ln;

centered(msg_quiz + Title);
if author<>'' then 
  centered(msg_author + author);
if copyright<>'' then
  centered('Copyright: ' + copyright);
if authorURI<>'' then 
  centered(msg_authorURI + authorURI);
if translator<>'' then
  centered(msg_translator + translator);
if edited<>'' then
  centered(msg_edited + edited);
if license<>'' then
  centered(msg_license + license);
if licenseURI<>'' then
  centered(msg_licenseURI + licenseURI);
if timelimit>0 then
  centered(msg_timelimit + showTime(timelimit));

{$IfDef Advertisement}
  Ln;
  centered(msg_advertisement);
{$EndIf}

{$IfDef Windows}
  Ln;
  case lang of
    deutsch: centered('ALT + Enter = Vollbildmodus');
    otherwise centered('Alt + Enter = Fullscreen mode')
    end;
{$EndIf}

Ln;

wait;
StartTime := GetSecs { set StartTime again }
end;

procedure Tscrquiz.setcharset(cs: string);
begin
inherited setcharset(cs);

cs := makeUpcase(cs); { to make checking easier }

if checkCP1252(cs)
   then case display of
          OEMdisplay:  setconverter(ISO1toOEM);
          UTF8display: setconverter(CP1252toUTF8);
          ISOdisplay:  setconverter(noconversion)
          end;

if checkOEM(cs)
   then case display of
          OEMdisplay:  setconverter(noconversion);
          UTF8display: setconverter(OEMtoUTF8);
	  ISOdisplay:  setconverter(OEMtoISO1);
          end;

if checkUTF8(cs) then
   case display of
     OEMdisplay:  setconverter(UTF8toOEM);
     UTF8display: setconverter(noconversion);
     ISOdisplay:  setconverter(UTF8toISO1);
     end;

if checkASCII(cs) then setconverter(forceASCII);

if checkISO(cs)
   then case display of
          OEMdisplay:  setconverter(ISO1toOEM);
          UTF8display: case whichISO(cs) of
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
                        end;
          ISOdisplay:  setconverter(noconversion)
          end
end;

procedure newparagraph;
begin
Ln;
Ln
end;

procedure Tscrquiz.paragraph;
var 
  s, rest: mystring;
begin
s := readLine;
while s<>'' do
  begin
  if s='.'
    then newparagraph
    else begin
         rest := s;
         while rest<>'' do 
	   begin
	   Write(format(rest, MaxLength-WhereX+1, MaxLength), ' ');
	   if rest<>'' then Ln;
	   if toodeep then wait
	   end
	 end;
  if toodeep then wait;
  s := readLine
  end;
Ln
end;

procedure Tscrquiz.processComment;
begin
NewScreen;
Ln;
paragraph;
Ln;
wait
end;

procedure Tscrquiz.processHint;
begin
processComment { handle like a Comment }
end;

procedure Tscrquiz.processQuestion;
begin
inherited processQuestion;

NewScreen;
Ln;
TextColor(QuestionColor);
paragraph;
TextColor(TxtColor);

processAnswer
end;

procedure Tscrquiz.processMulti;
begin
inherited processMulti;
NewScreen;
Ln;
TextColor(QuestionColor);
paragraph;
TextColor(TxtColor);

processMultiAnswer
end;

procedure Tscrquiz.nextanswer;
begin
inc(answerNr);
if answerNr>MaxAnswers then 
   begin 
   error; 
   answerNr := MaxAnswers { to avoid range overruns }
   end
end;

procedure Tscrquiz.showanswers(showdefault: boolean);
var
  startpos: TscreenPos;
  s, ans: mystring;
  value: pointsType;
begin
Ln;
if toodeep then wait;

answerNr := 0;
readAnswer(value, s);
while s<>'' do
  begin
  ans := s;
  nextanswer;
  AnsPoints[answerNr] := value;  { points }
  if ScrWidth>=80 then Write('   ');
  Write(ValueToKey(answerNr), ') ');
  startpos := WhereX;
  while ans<>'' do 
    begin
    Write(format(ans, MaxLength-WhereX+1, MaxLength-startpos)); 
    if ans<>'' then 
       begin
       Ln;
       if toodeep then wait;
       if ScrWidth>=80 then Write('      ')
                       else Write('   ')
       end
    end;
  Ln;
  if toodeep then wait;
  readAnswer(value, s) { Answer line }
  end;

{ show default-Answer }
if showdefault and (defanswer<>'') then
  begin
  ans := defanswer;
  nextanswer;
  AnsPoints[answerNr] := 0;
  if ScrWidth>=80 then write('   ');
  Write(ValueToKey(answerNr), ') ');
  startpos := WhereX;
  while ans<>'' do 
    begin
    Write(format(ans, MaxLength-WhereX+1, MaxLength-startpos)); 
    if ans<>'' then 
       begin
       Ln;
       if toodeep then wait;
       if ScrWidth>=80 then Write('      ')
                       else Write('   ')
       end
    end;
  Ln; 
  if toodeep then wait;
  end
end;

function Tscrquiz.getanswer(keys: keyset): integer;
var 
  c : char;
  okay: boolean;
begin
repeat
  c := Upcase(ReadKey);
  okay := c in keys;
  if not okay then ErrorSignal;
  if c=#0 then ReadKey;
until okay;

if c=ExitKey then quit := true;

if c in AnswerKeys
  then begin write(c); getanswer := KeyToValue(c) end
  else getanswer := -1
end;

procedure Tscrquiz.processAnswer;
var ap : pointsType;
    i : integer;
    keys: keyset;
    maxkey: char;
begin
showanswers(true);

if toodeep then wait;
Ln;
Write('> ');

maxkey := ValueToKey(answerNr);
if answerNr<10 
   then keys := [ '1' .. maxkey, ExitKey ]
   else keys := [ '1' .. '9', 'A' .. maxkey, ExitKey ];
i := getanswer(keys);
if (i<>-1) and not quit then 
  begin
  ap := AnsPoints[i];
  inc(Points, ap);

  if not neutral then
    begin
    Write('  '); { distance }
    if ap > 0 
      then 
        begin { true }
          TextColor(TrueColor);
          Write(msg_right);
	  RightSignal
        end
      else
        begin { false }
            TextColor(FalseColor);
            Write(msg_wrong);
	    FalseSignal
        end
     end; { not neutral }

  if neutral then NeutralSignal;
  
  TextColor(TxtColor);
  Ln; Ln;
  Write(msg_points, Points);
  Ln;
  wait
  end
end;

procedure Tscrquiz.processMultiAnswer;
var i: integer;
    keys: keyset;
    myPoints, myMax : pointsType;
    maxkey: char;
begin
myPoints := 0;
myMax := 0;

showanswers(false);

Ln; 
Write('>> ');

{ max points for this one question }
for i := 1 to answerNr do
  if AnsPoints[i] > 0 then inc(myMax, AnsPoints[i]);

maxkey := ValueToKey(answerNr);
if answerNr<10 
   then keys := [ '1'..maxkey, ExitKey, Enter ]
   else keys := [ '1'..'9', 'A'..maxkey, ExitKey, Enter ];

repeat
  i := getAnswer(keys);
  if i<>-1 then 
    begin
    inc(Points, AnsPoints[i]);   { absolute points }
    inc(myPoints, AnsPoints[i]); { points for this question }
    { remove that key from valid keys }
    keys := keys - [ ValueToKey(i) ]
    end
until i=-1;

Ln; Ln;
Write(msg_points, Points, 
      '  (', myPoints, '/', myMax, ')');
Ln;
NeutralSignal;
wait
end;

procedure Tscrquiz.evaluate;
begin
if MaxPoints=0 then 
  begin
  inherited evaluate;
  exit
  end;
  
NewScreen;
Ln; Ln;
{
if checkTimeout then 
  begin
  if ScrWidth>=80 then Write('   ');
  WriteLn(msg_timeout)
  end; }

if ScrWidth>=80 then Write('   ');
Write(msg_sol1, Points, msg_sol2, MaxPoints);
if length(msg_sol3) > MaxLength-WhereX then Ln;
Write(msg_sol3);
Ln;
If Points > 0
  then begin
       if ScrWidth>=80 then write('   ');
       Write(msg_sol4, getPercentage, '%.');
       Ln
       end
  else if not neutral then 
          begin
	  if ScrWidth>=80 then write('   ');
          Write(msg_sol5);
          Ln
          end;

{ show time needed }
if not neutral then
  begin
  Ln;
  if ScrWidth>=80 then write('   ');
  Write(msg_time + ShowTime(GetSecs - StartTime))
  end;

Ln;
inherited evaluate;

InfoSignal;
wait
end;

procedure Tscrquiz.processAssessment;
begin
processComment { is handled like a comment }
end;

procedure Tscrquiz.processAssessmentPercent;
var rest: mystring;
begin
rest := readAssessmentPercent;
while rest<>'' do 
   begin
   Write(format(rest, MaxLength-WhereX+1, MaxLength), ' ');
   Ln
   end;
wait
end;

procedure Tscrquiz.EndQuiz;
begin
if not evaluated and not quit 
  then evaluate
end;

procedure Tscrquiz.error;
begin
readerror := true;
quit := true
end;

{---------------------------------------------------------------------}

{$IfDef __GPC__}

  procedure FetchScreenSize;
  var x1, y1, x2, y2: integer;
  begin
  GetWindow(x1, y1, x2, y2);
  
  ScrWidth  := TscreenPos(x2-x1+1);
  ScrHeight := TscreenPos(y2-y1+1);

  MaxLength := x2-x1+1;
  MaxHeight := y2-y1+1
  end;

{$Else}

  procedure FetchScreenSize;
  begin
  { like in Borland (Turbo) Pascal }
  ScrWidth  := TscreenPos(Lo(WindMax) - Lo(WindMin) + 1);
  ScrHeight := TscreenPos(Hi(WindMax) - Hi(WindMin) + 1);

  { If nothing else works, use constants }
  { ScrWidth := 80; ScrHeight := 25; }

  MaxLength := ScrWidth;
  MaxHeight := ScrHeight
  end;
{$EndIf}

procedure version;
begin
WriteLn(PrgVersion);
WriteLn;
WriteLn('Copyright (C) ', AKFQuizCopyright);
WriteLn('Copyright (C) 1999-2001 Free Software Foundation, Inc.');
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
WriteLn('Quiz-program for the text-terminal or xterm');
WriteLn;
WriteLn('Usage: scrquiz [options] [inputfile]');
WriteLn(' or:   scrquiz -h | --help | /?');
WriteLn(' or:   scrquiz --version');
WriteLn;
WriteLn('Options:');
WriteLn(' -s          no sound');
WriteLn(' -d <dir>    path to quizfiles');
WriteLn(' -1          run just once');
WriteLn(' -p          unstopable (use with care!)');
WriteLn(' -OEM        display has OEM (IBM850/IBM437) charset');
WriteLn(' -latin1     display has Latin1 charset');
WriteLn(' -UTF8       display has UTF-8 charset');
{$IfDef FPC} {$IfDef Go32v2}
WriteLn(' -lfn        use long filenames (DOS only)');
{$EndIf} {$EndIf}

WriteLn;
WriteLn('QUIZPATH: ', getQuizPath);

if BugMail <> ''
  then begin WriteLn; WriteLn('Report bugs to <' + BugMail + '>.') end;

Halt
end;

function askfile: mystring;
var
  cnv : Tconverter;
  Nr, offset : integer;
  c, maxkey : char;
  okay: boolean;
  keys : keyset;
  fileMaxY: TscreenPos;
  entry : PquizfileList;
begin
buildscreen(AKFQuizName);
Ln;
okay := true;
offset := 0;
Nr := 1;

case display of
  ISOdisplay:  cnv := UTF8toISO1;
  OEMdisplay:  cnv := UTF8toOEM;
  UTF8display: cnv := noconversion;
  otherwise    cnv := noconversion;  
  end;

if quizfileList=NIL then fillQuizfileList;
entry := quizfileList;

{ leave enough space at the bottom 
  also for "more..." }
fileMaxY := TscreenPos(MaxHeight - 3);

{ no entries }
if entry=NIL then 
  begin
  Write(msg_noquizfound);
  Ln; Ln;
  waitkey(msg_anykey(''));
  normscreen;
  Halt
  end;

{ just one entry -> don't ask stupid questions }
if entry^.next=NIL then
  begin
  askfile := entry^.filename;
  exit
  end;

okay := false;
while (entry<>NIL) and (not okay) do
  begin
  while (entry<>NIL) and (Nr<=MaxAnswers-1) and (LineCnt<fileMaxY) do
    begin
    with entry^ do
      if language='' 
        then Write(ValueToKey(Nr)+') ' + copy(cnv(title),1,MaxLength-3))
        else Write(ValueToKey(Nr)+') ' + language + ': '
	           + copy(cnv(title),1,MaxLength-5-length(language)));
    Ln;
    entry := entry^.next;
    inc(Nr)
    end;

  if (entry=NIL) and (offset=0)
    then dec(Nr) { all entries shown }
    else begin Write(ValueToKey(Nr)+') '+msg_more); Ln end;

  maxKey := ValueToKey(Nr);
  if Nr<10 
    then keys := [ '1'..maxKey, ExitKey ]
    else keys := [ '1'..'9', 'A'..maxKey, ExitKey ];

  Ln;
  QuestionSignal;
  Write('> ');
  
  repeat
    c := Upcase(ReadKey);
    okay := c in keys;
    if not okay then ErrorSignal;
  until okay;
  
  if (c=maxkey) and ((entry<>NIL) or (offset<>0)) then { more...}
    begin
    okay := false;
    offset := offset + Nr - 1;
    Nr := 1;
    ClrScr;
    LineCnt:=0;
    Ln;
    if entry=NIL then { start from the beginning }
      begin
      entry := quizfileList;
      offset := 0
      end
    end
  end;

if (c=ExitKey) and not unstopable then
   begin
   normscreen;
   Halt
   end;

{ get the result }
askfile := searchQuizfileList(KeyToValue(c) + offset)
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
      begin DisableSignals; continue end;
  if p='-OEM' then
      begin display := OEMdisplay; setmsgconv(display); continue end;
  if (p='-LATIN1') or (p='-NOOEM') then
      begin display := ISOdisplay; setmsgconv(display); continue end;
  if (p='-UTF8') or (p='-UTF-8') then
      begin display := UTF8display; setmsgconv(display); continue end;
  if (p='-1') then 
      begin if not unstopable then loop:=false; continue end;
  if (p='-P') or (p='/P') then 
     begin loop:=true; unstopable:=true; 
           nobreak; checkbreak:=false; continue end;
  if (p='-D') then
     begin inc(i); { handled in qsys } continue end;
  if (p='-H') or (p='--HELP') or (p='/?') then help;
  if (p='--VERSION') then version;
  if p[1]='-'    { "/" might be used in a path }
     then help { unknown parameter }
     else infile := ParamStr(i) { not Upcase }
  end
end;

var myexitcode : byte;

begin { main }
ident('$Id: scrquiz.pas,v 1.14 2006/11/15 15:37:28 akf Exp $');

myexitcode := 0;
loop := true;
unstopable := false;
useSystemLanguage;

{$IfDef SdlSoundForAll}
  InitAudio(false);
{$Else}
  useBeepSignals;
{$EndIf}

display := checkDisplay;
setmsgconv(display);

{$IfDef __GPC__}
  CRTInit;
  {$IfDef __OS_DOS__}
     SetPCCharset(true);
     display := OEMdisplay;
  {$Else}
     SetPCCharset(false);
  {$EndIf}
{$EndIf}

FetchScreenSize;

repeat
  parameters;
  titlescreen;

  if infile<>'' then
     if not getquizfile(infile) then ErrorFileNotFound;

  if infile='' then 
    repeat
      infile := askfile;
      if not unstopable and (infile='') 
           then begin normscreen; Halt end;
    until (infile<>'') and getquizfile(infile);
  
  quiz.Init(infile);
  if IOResult<>0 then ErrorFileNotFound;

  { assume Latin1 as default charset }
  { may be changed by the "charset:" keyword }
  case display of
    ISOdisplay:  quiz.setconverter(noconversion);
    OEMdisplay:  quiz.setconverter(ISO1toOEM);
    UTF8display: quiz.setconverter(ISO1toUTF8)
    end;

  quiz.process;
  if quiz.readerror then begin myexitcode:=2; loop:=false end;
  if quiz.stopQuiz and not unstopable then loop := false;

  quiz.Done;

  infile := ''
until not loop;

Halt(myexitcode)
end.
