{
* linequiz
* line oriented program for AKFQuiz
* usable for blind users (braile line or speech synthesizer)
* usable as backend for other applications
*
* Copyright (c) 2005-2006,2007,2010,2014
* Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: FreePascal or GNU-Pascal 
*
* This file is part of AKFQuiz
*
* AKFQuiz is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version.
*
* AKFQuiz is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
*}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Windows} {$EndIf}
{$IfDef Win32} {$Define Windows} {$EndIf}

{$IfDef Windows}
  {$R w32/linequiz.res}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
  
  {$IfDef Windows}
    {$AppType Console}
  {$EndIf}
{$EndIf}


{$I-}
{$R+} { Range checking }

program linequiz(input, output, stderr);
uses uakfquiz, qsys, qmsgs
{$IfDef SdlSoundForAll}
  , sdlsnd
{$EndIf}
;

{ GNU compliant format }
const PrgVersion = 'linequiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const MaxAnswers = 35;

type 
  Tlinequiz = 
    object(Takfquiz)
      readerror : boolean;

      { only temporarily used: }
      AnsPoints : array[1..MaxAnswers] of pointsType;
      
      destructor Done;                            virtual;
      procedure resetQuiz;                        virtual;
      procedure StartQuiz;                        virtual;
      procedure setcharset(cs: string);           virtual;
      procedure processParagraph;
      procedure processComment;                   virtual;
      procedure processHint;                      virtual;
      procedure processAssessment;                virtual;
      procedure processAssessmentPercent;         virtual;
      procedure processQuestion;                  virtual; 
      procedure processMulti;                     virtual;
      procedure nextanswer;
      procedure processAnswer;                    virtual;
      procedure processMultiAnswer;               virtual;
      procedure showAnswers(showdefault: boolean);
      procedure evaluate;                         virtual;
      procedure EndQuiz;                          virtual; 
      procedure error;                            virtual;
    end;

var MaxLength: integer = 72;

var infile: mystring;
var quiz: Tlinequiz;

var display: DisplayType;

var moreLineBreaks: boolean = false;

procedure Enter;
begin
Write('ENTER> ');
if moreLineBreaks then WriteLn;
ReadLn
end;

{---------------------------------------------------------------------}
procedure Tlinequiz.resetQuiz;
begin
inherited resetQuiz;
readerror := false
end;

destructor Tlinequiz.Done;
begin
inherited Done;

if readerror then
  WriteLn(stderr, msg_error)
end;

procedure Tlinequiz.setcharset(cs: string);
begin
inherited setcharset(cs);

cs := makeUpcase(cs); { to make checkung easier }

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
          end;

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
	  ISOdisplay:  setconverter(OEMtoISO1)
          end;

if checkUTF8(cs) then
   case display of
     OEMdisplay:  setconverter(UTF8toOEM);
     UTF8display: setconverter(noconversion);
     ISOdisplay:  setconverter(UTF8toISO1)
     end;

if checkASCII(cs) then setconverter(forceASCII)
end;

procedure Tlinequiz.StartQuiz;
begin
inherited StartQuiz;
WriteLn;
WriteLn('INFO:');
WriteLn;
WriteLn(msg_quiz + title);

if author<>'' then 
  WriteLn(msg_author, author);
if copyright<>'' then
  WriteLn('Copyright: ', copyright);
if authorURI<>'' then 
  WriteLn(msg_authorURI, authorURI);
if translator<>'' then
  WriteLn(msg_translator, translator);
if edited<>'' then
  WriteLn(msg_edited, edited);
if license<>'' then
  WriteLn(msg_license, license);
if licenseURI<>'' then
  WriteLn(msg_licenseURI, licenseURI);
if timelimit>0 then
  WriteLn(msg_timelimit + showTime(timelimit));
WriteLn;
{$IfDef Advertisement}
  WriteLn(msg_advertisement);
  WriteLn;
{$EndIf}
Enter;
StartTime := GetSecs
end;

procedure Tlinequiz.processParagraph;
var s, rest, outs : mystring;
begin
outs := '';
s := readLine;
while s<>'' do
   begin
   if s='.'
     then begin
          WriteLn(outs);
	  outs := '';
	  WriteLn
          end 
     else begin
          rest := s;
          while rest<>'' do
	    begin
	    if outs=''
	      then outs := format(rest, MaxLength, MaxLength)
	      else outs := outs + ' ' +
                           format(rest, MaxLength-Length(outs)-1, MaxLength);
            if rest<>'' then 
	      begin
	      WriteLn(outs);
	      outs := ''
	      end
	    end
          end;
   s := readLine
   end;
WriteLn(outs)
end;

procedure Tlinequiz.processComment;
begin
WriteLn;
WriteLn('COMMENT:');
processParagraph;
WriteLn;
Enter
end;

procedure Tlinequiz.processHint;
begin
WriteLn;
WriteLn('HINT:');
processParagraph;
WriteLn;
Enter
end;

procedure Tlinequiz.processAssessment;
begin 
WriteLn;
WriteLn('ASSESSMENT:');
processParagraph;
WriteLn;
Enter
end;

procedure Tlinequiz.processAssessmentPercent;
var rest: mystring;
begin 
WriteLn;
WriteLn('ASSESSMENT:');

rest := readAssessmentPercent;
while rest<>'' do
    WriteLn(format(rest, MaxLength, MaxLength));
WriteLn;
Enter
end;

procedure Tlinequiz.processQuestion;
begin
inherited processQuestion;

WriteLn;
WriteLn('QUESTION:');
processParagraph;

processAnswer
end;

procedure Tlinequiz.processMulti;
begin
inherited processMulti;

WriteLn;
WriteLn('MULTI-QUESTION:');
processParagraph;

processMultiAnswer
end;

procedure Tlinequiz.nextanswer;
begin
inc(answerNr);
if answerNr>MaxAnswers then 
   begin 
   error; 
   answerNr := MaxAnswers { to avoid range overruns }
   end
end;

procedure Tlinequiz.showanswers(showdefault: boolean);
var 
  s, ans: mystring;
  value: pointsType;
begin
WriteLn;
WRITELN('ANSWERS:');

answerNr := 0;
readAnswer(value, s);
while s<>'' do
  begin
  ans := s;
  nextanswer;
  AnsPoints[answerNr] := value;
  Write(ValueToKey(answerNr), ') ');
  if moreLineBreaks 
     then WriteLn
     else WriteLn(format(ans, MaxLength-3, MaxLength));
  while ans<>'' do
     WriteLn(format(ans, MaxLength, MaxLength));
  readAnswer(value, s)
  end;
  
{ show default-Answer }
if showdefault and (defanswer<>'') then
  begin
  ans := defanswer;
  nextanswer;
  AnsPoints[answerNr] := 0;
  Write(ValueToKey(answerNr), ') ');
  if moreLineBreaks 
     then WriteLn
     else WriteLn(format(ans, MaxLength-3, MaxLength));
  while ans<>'' do
     WriteLn(format(ans, MaxLength, MaxLength));
  end
end;

procedure Tlinequiz.processAnswer;
var 
  ap : pointsType;
  i : integer;
  c : char;
begin
showanswers(true);

WriteLn;

repeat
  Write('#> ');
  if moreLineBreaks then WriteLn;
  ReadLn(c);
  i := KeyToValue(c);
until i<>-1;

if (i<>-1) and not quit then 
  begin
  ap := AnsPoints[i];
  inc(Points, ap);

  if not neutral then
    if ap > 0 
      then begin 
           WriteLn(msg_right); 
	   RightSignal
	   end
      else begin
           WriteLn(msg_wrong);
	   FalseSignal
	   end;
	   
  if neutral then NeutralSignal;
  
  WriteLn;
  WriteLn(msg_points, Points);
  WriteLn;
  Enter
  end
end;

procedure Tlinequiz.processMultiAnswer;
var 
  i : integer;
  ans: integer;
  Answers : mystring;
  ap, myPoints, myMax: PointsType;
begin
myPoints := 0;
myMax    := 0;

showanswers(false);

{ calculate maximum points for this question }
for i := 1 to answerNr do
  if AnsPoints[i] > 0 then inc(myMax, AnsPoints[i]);
  
WriteLn;
Write('##> ');
if moreLineBreaks then WriteLn;
ReadLn(Answers);

for i := 1 to Length(Answers) do
    begin
    ans := KeyToValue(Answers[i]);
    if ans>0 then 
       begin
       ap := AnsPoints[ans];
       AnsPoints[ans] := 0; { to use it not more than once }
       inc(myPoints, ap);
       inc(Points, ap)
       end
    end;
WriteLn;

WriteLn(msg_points, Points,
        ' (', myPoints, '/', myMax, ')');
WriteLn;
NeutralSignal;
Enter
end;

procedure Tlinequiz.evaluate;
begin
WriteLn;
if not evaluated and not quit and (MaxPoints<>0) then
  begin
  { if checkTimeout then WriteLn(msg_timeout); }
  WriteLn(msg_sol1, Points, msg_sol2,
          MaxPoints, msg_sol3);
  If Points > 0 
    then WriteLn(msg_sol4, getPercentage, '%.')
    else if not neutral then WriteLn(msg_sol5);
  if not neutral then WriteLn(msg_time, ShowTime(GetSecs - StartTime))
  end;

inherited evaluate;
InfoSignal;
WriteLn;
Enter
end;

procedure Tlinequiz.EndQuiz;
begin
if not evaluated and not quit 
  then evaluate
end;

procedure Tlinequiz.error;
begin
readerror := true;
quit := true
end;



{---------------------------------------------------------------------}

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
WriteLn('Command-line quiz-program');
WriteLn;
WriteLn('Usage: linequiz [options] [file.akfquiz]');
WriteLn(' or:   linequiz -h | --help | /?');
WriteLn(' or:   linequiz --version');
WriteLn;
WriteLn('Options:');
WriteLn(' -l          more line-breaks');
WriteLn(' -s          no sound');
WriteLn(' -d <dir>    path to quizfiles');
WriteLn(' -w <width>  set maximum width of lines');
WriteLn(' -OEM        display has OEM (IBM850/IBM437) charset');
WriteLn(' -latin1     display has Latin1 charset');
WriteLn(' -UTF8       display has UTF-8 charset');
{$IfDef FPC} {$IfDef Go32v2}
WriteLn(' -LFN        use long filenames (DOS only)');
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

procedure setwidth(const s: string);
var error: word;
begin
val(s, MaxLength, error);
if error<>0 then
   begin
   WriteLn(stderr, 'Error: after -w must be a number');
   Halt(1)
   end
end;

procedure parameters;
var 
  i: integer;
  count: integer;
  p: mystring;
begin
count := ParamCount;
i := 0;
while i<count do
    begin
    inc(i);
    p := makeUpcase(ParamStr(i));
    if p='-L' then 
        begin moreLineBreaks := true; continue end;
    if p='-S' then 
        begin DisableSignals; continue end;
    if p='-D' then 
        begin inc(i); { handled in qsys } continue end;
    if p='-LFN' then
        begin setLFNsupport; continue end;
    if p='-OEM' then
        begin display := OEMdisplay; setmsgconv(display); continue end;
    if (p='-LATIN1') or (p='-NOOEM') then
        begin display := ISOdisplay; setmsgconv(display); continue end;
    if (p='-UTF8') or (p='-UTF-8') then
        begin display := UTF8display; setmsgconv(display); continue end;
    if p='-W' then
        begin inc(i); setwidth(ParamStr(i)); continue end;
    if (p='-H') or (p='--HELP') or (p='/?') then help;
    if (p='--VERSION') then version;
    if p[1]='-'    { "/" might be used in a path }
       then help { unknown parameter }
       else infile := ParamStr(i); { not Upcase }
    end
end;

procedure myshowentry(const dir, s: string);
begin
WriteLn(stripext(s))
end;

function askfile: mystring;
var 
  found: boolean;
  path, s: mystring;
begin
{$IfDef Advertisement}
  WriteLn(msg_advertisement);
  WriteLn;
{$EndIf}

WriteLn('QUIZFILES:');
WriteLn;

found := false;
path := getquizpath;
while path<>'' do
  begin
  s := getnextdir(path);
  if ListEntries(s, quizext, myshowentry)  then found := true;
  if ListEntries(s, quizext2, myshowentry) then found := true;
  end;

if not found then begin Write(msg_noquizfound); WriteLn end;

WriteLn;
QuestionSignal;
Write('$> ');
if moreLineBreaks then WriteLn;
ReadLn(s);
askfile := s
end;

var myexitcode : byte;

begin { main }
myexitcode := 0;
display := checkdisplay; { set a default }
setmsgconv(display);
useSystemLanguage;

{$IfDef SdlSoundForAll}
  Write(msg_InitAudio);
  InitAudio(false);
  WriteLn;
{$Else}
  useBeepSignals;
{$EndIf}

parameters;

IntroSignal;

if infile='' then infile := askfile;
if infile='' then Halt;

if not getquizfile(infile) then
  begin
  WriteLn(stderr, msg_filenotfound);
  Halt(1)
  end;

quiz.Init(infile);

quiz.process;
if quiz.readerror then myexitcode := 2;
quiz.Done;

Halt(myexitcode)
end.
