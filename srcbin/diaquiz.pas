{
* diaquiz
* quiz program based on "Xdialog"
*
* $Id: diaquiz.pas,v 1.7 2006/10/02 12:49:32 akf Exp $
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
*}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
{$EndIf}

{$R+} { Range checking }
{$I-}

program diaquiz(input, output, stderr);

{$IfDef FPC}
  uses uakfquiz, qmsgs, qsys, dialog, SysUtils
  {$IfDef SdlSoundForAll}
    ,sdlsnd
  {$EndIf}
  ;
{$EndIf}

{$IfDef __GPC__}
  import uakfquiz; qmsgs; qsys; dialog;
    {$IfDef SdlSoundForAll}
    sdlsnd;
    {$EndIf}
{$EndIf}

{ GNU compliant format }
const PrgVersion = 'diaquiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const MaxAnswers = 35;
const AnswerKeys = [ '1'..'9', 'A'..'Z' ];

type 
  Tdiaquiz = 
    object(Takfquiz)
      readerror : boolean;

      { only temporarily used: }
      AnsPoints : array[1..MaxAnswers] of pointsType;
      
      constructor Init(infile: string);
      destructor Done;                            virtual;

      procedure ResetQuiz;                        virtual;
      procedure StartQuiz;                        virtual;
      procedure setcharset(cs: string);           virtual;
      procedure processComment;                   virtual;
      procedure processHint;                      virtual;
      procedure processAssessment;                virtual;
      procedure processAssessmentPercent;         virtual;
      procedure nextanswer;
      procedure processQuestion;                  virtual; 
      procedure processMulti;                     virtual;
      procedure evaluate;                         virtual;
      procedure EndQuiz;                          virtual; 
      procedure error;                            virtual;
      function fetchAnswers(radio: boolean): DialogString;
    end;

var infile: mystring;
var quiz: Tdiaquiz;

{---------------------------------------------------------------------}
constructor Tdiaquiz.Init(infile: string);
begin
if infile='' then fselect_a(getquizdir, infile);
if infile='' then Halt;
if not getquizfile(infile) then 
   begin
   WriteLn(stderr, msg_filenotfound);
   Halt(2)
   end;

inherited Init(infile)
end;

procedure Tdiaquiz.resetQuiz;
begin
inherited resetQuiz;

readerror := false;

DialogTitle := AKFQuizName + ', ' + AKFQuizVersion;
DialogBackTitle := ''
end;

destructor Tdiaquiz.Done;
begin
inherited Done;

if readerror then
  WriteLn(stderr, msg_fileerror)
end;

procedure Tdiaquiz.setcharset(cs: string);
begin
inherited setcharset(cs);

cs := makeUpcase(cs);

{ Latin1 is default }
setconverter(noconversion);
if checkOEM(cs)   then setconverter(OEMtoISO1);
if checkUTF8(cs)  then setconverter(UTF8toISO1);
if checkASCII(cs) then setconverter(forceASCII)
end;

procedure Tdiaquiz.StartQuiz;
var txt: DialogString;
begin
inherited StartQuiz;

DialogBackTitle := title;

txt := msg_quiz + title + '\n';

if author<>'' then 
  txt := txt + msg_author + author + '\n';
if copyright<>'' then
  txt := txt + 'Copyright: ' + copyright + '\n';
if authorURI<>'' then 
  txt := txt + msg_authorURI + authorURI + '\n';
if translator<>'' then
  txt := txt + msg_translator + translator + '\n';
if edited<>'' then
  txt := txt + msg_edited + edited + '\n';
if license<>'' then
  txt := txt + msg_license + license;
if licenseURI<>'' then
  txt := txt + msg_licenseURI + licenseURI;

msgbox_a(txt);
quit := DialogCode <> 0;
StartTime := GetSecs
end;

procedure Tdiaquiz.processComment;
var 
  s : mystring;
  txt : DialogString;
begin
txt := '';
s := readLine;
while s<>'' do
   begin
   if s='.' { new paragraph }
     then txt := txt + '\n\n'
     else txt := txt + s + ' ';
   s := readLine
   end;
msgbox_a(txt);
quit := DialogCode <> 0
end;

procedure Tdiaquiz.processHint;
begin
processComment { handle like a Comment }
end;

procedure Tdiaquiz.processAssessment;
begin
processComment
end;

procedure Tdiaquiz.processAssessmentPercent;
var s: mystring;
begin
s := readAssessmentPercent;
if s<>'' then
   begin
   msgbox_a(readAssessmentPercent);
   quit := DialogCode <> 0
   end
end;

procedure Tdiaquiz.nextanswer;
begin
inc(answerNr);
if answerNr>MaxAnswers then 
   begin 
   error; 
   answerNr := MaxAnswers { to avoid range overruns }
   end
end;


procedure Tdiaquiz.processQuestion;
var 
  s           : mystring;
  txt, answer,
  answers     : DialogString;
  myPoints    : pointsType; 
  value       : integer;
begin
inherited processQuestion;

txt := '';
s := readLine;
while s<>'' do
   begin
   if s='.' { new paragraph }
     then txt := txt + '\n\n'
     else txt := txt + s + ' ';
   s := readLine
   end;

answers := fetchAnswers(true);
if defanswer<>'' then
  begin
  nextanswer;
  AnsPoints[answerNr] := 0;
  addlistitem(answers, ValueToKey(answerNr), defanswer, true)
  end;

radiolist(txt, autosize, autosize, 9, answers, answer);
quit := DialogCode <> 0;

if answer<>'' 
   then value := KeyToValue(answer[1])
   else value := 0;

if (value>0) and not quit then
  begin
  myPoints := AnsPoints[value];
  inc(Points, myPoints);
  
  if neutral then NeutralSignal;
  
  if not neutral then
    begin
    if myPoints > 0 
      then begin
           RightSignal;
           msgbox_a(msg_right + '\n\n' +
                    '('+msg_points + IntToStr(myPoints)+')')
           end
      else begin
           FalseSignal;
           msgbox_a(msg_wrong + '\n\n' +
                    '('+msg_points + IntToStr(myPoints)+')')
	   end
    end
  end
end;

function fetchpoints(var x: string): pointsType; {@@@}
var 
  s: mystring;
  i: integer;
  v: pointsType;
begin
s := '';
i := 1;
while (i<=length(x)) and (x[i] in AnswerKeys) do 
 begin
 s := s + x[i];
 inc(i)
 end;

if s<>'' 
  then v := KeyToValue(s[1])
  else v := 0;

{ strip away to next key }
i := 1;
while (i<length(x)) and not (x[i] in AnswerKeys) do inc(i);
delete(x, 1, i+1);

fetchpoints := v
end;

procedure Tdiaquiz.processMulti;
var 
  s           : mystring;
  txt, answer : DialogString;
  myPoints,
  thisPoints  : pointsType;
begin
inherited processMulti;

txt := '';
s := readLine;
while s<>'' do
   begin
   if s='.' { new paragraph }
     then txt := txt + '\n\n'
     else txt := txt + s + ' ';
   s := readLine
   end;

checklist(txt, autosize, autosize, 9, fetchAnswers(false), answer);
quit := DialogCode <> 0;
if quit then exit;

myPoints := 0;
while answer<>'' do
  begin
  thisPoints := AnsPoints[fetchpoints(answer)];
  inc(Points, thisPoints);
  inc(myPoints, thisPoints)
  end;

if neutral then NeutralSignal;

{ right or wrong }
if not neutral then
  begin
  if myPoints > 0 
     then begin
          RightSignal;
          msgbox_a(msg_right + '\n\n' +
                   '('+msg_points + IntToStr(myPoints)+')')
          end
     else begin
          FalseSignal;
          msgbox_a(msg_wrong + '\n\n' +
                   '('+msg_points + IntToStr(myPoints)+')')
          end
  end
end;

function Tdiaquiz.fetchAnswers(radio: boolean): DialogString;
var
  s : mystring; 
  ans: DialogString;
  value: pointsType;
begin
ans := '';
answerNr := 0;
readAnswer(value, s);
while (s<>'') do
  begin
  nextanswer;
  addlistitem(ans, ValueToKey(answerNr), s, false);
  AnsPoints[answerNr] := value;
  readAnswer(value, s)
  end;

fetchAnswers := ans
end;

procedure Tdiaquiz.evaluate;
var txt: DialogString;
begin
txt := '';
if not evaluated and not quit and (MaxPoints<>0) then
  begin
  {if checkTimeout then txt := txt + msg_timeout + '\n';}
  txt := txt + msg_sol1 + IntToStr(Points) + msg_sol2 +
              IntToStr(MaxPoints) + msg_sol3 + '\n';
  If Points > 0 
    then txt := txt + msg_sol4 +
                IntToStr(getPercentage) + '%.\n'
    else if not neutral then txt := txt + msg_sol5 + '\n';
  txt := txt + msg_time + ShowTime(GetSecs - StartTime);
  InfoSignal;
  msgbox_a(txt);
  quit := DialogCode<>0
  end;
inherited evaluate
end;

procedure Tdiaquiz.EndQuiz;
begin
evaluate;
DialogBacktitle := ''
end;

procedure Tdiaquiz.error;
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
Halt
end;


procedure help;
begin
WriteLn('Quiz program using Xdialog');
WriteLn;
WriteLn('Usage: diaquiz [file.akfquiz]');
WriteLn(' or:   diaquiz -h | --help | /?');
WriteLn(' or:   diaquiz --version');
WriteLn;
WriteLn('QUIZPATH: ', getQuizPath);

if BugMail <> ''
  then begin WriteLn; WriteLn('Report bugs to <' + BugMail + '>.') end;

Halt
end;

procedure parameters;
var p: mystring;
begin
p := ParamStr(1);
if (p='-h') or (p='--help') or (p='/?') then help;
if (p='--version') then version;

infile := p
{ if no parameter (infile='') it's handled later }
end;

procedure InitDialog;
begin
DialogCmd    := 'Xdialog';
DialogParams := '--no-tags --wrap'
end;


var myexitcode : byte;

begin { main }
myexitcode := 0;
useSystemLanguage;

{$IfDef SdlSoundForAll}
  InitAudio(false);
{$EndIf}

parameters;

InitDialog;
IntroSignal;

quiz.Init(infile);
quiz.process;
if quiz.readerror then myexitcode := 2;
quiz.Done;

Halt(myexitcode)
end.
