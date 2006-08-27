{
* uakfquiz (unit)
*
* $Id: uakfquiz.pas,v 1.4 2006/08/27 06:47:35 akf Exp $
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

{ 
  Conditional Defines:

  NoBuffer
     don't Buffer the input
  Buffer
     enforce Buffering
}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
  {$Smartlink on}
{$EndIf}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Win32} {$EndIf}

{$IfNDef NoBuffer}
  {$IfNDef __GPC__} { GPC doesn't need no additional buffer }
    {$Define Buffer}
  {$EndIf}
{$EndIf}

{$I-} { no automatic I/O-Check }
{$X+} { function call as procedure allowed }


unit uakfquiz;    { "All unit is love" (the Beatles) ;-) }

interface
uses qsys, qmsgs;

{$I config.inc}

{ please add your name, when you make changes }
{ You may not delete my name }
const AKFQuizCopyright = '2006 AKFoerster';

{ when you make changes, change the contact address to your one }
const AKFQuizEmail = 'akfquiz@akfoerster.de';

{ Address, where to get the source code }
{ When you make changes, please provide the address where your version
  of source-code is published }
const Homepage = 'http://akfoerster.de/akfquiz/';


{ some default values }
const
   def_title          = 'Quiz';
   def_author         = '';
   def_authorURI      = '';
   def_translator     = '';
   def_edited         = '';
   def_copyright      = '';
   def_license        = '';
   def_licenseURI     = '';
   def_language       = 'en';   { see also Takfquiz.resetQuiz }
   def_charset        = 'US-ASCII';
   def_baseURI        = '';
   def_javascript     = 'akfquiz5.js';
   def_CSS            = '';
   def_defanswer      = '';
   def_neutral        = false;
   def_HTMLcode       = false;  { keep that false as default! }
   def_noindex        = false;
   def_rtl            = false;
   def_bidi           = false;
   def_assessmentURI  = '';

type pointsType = LongInt;

{ MC = Multiple-Choice, MCMA = Multiple-Choice Multiple Answers }
type TquestionType = (MC, MCMA, textfield);

type 
  Pakfquiz = ^Takfquiz;
  Takfquiz = 
    { abstract } object
      private
	inp : text;
	htmltagskip: boolean; { skip over html Tags? }
	convert : Tconverter;
	
        {$IfDef Buffer}
          Buffer : array[ 1..10240 ] of char;
        {$EndIf}

      protected
        { anything that can be defined in the input file belongs here! }
        title, author, authorURI, copyright, translator, edited,
        license, licenseURI, language, charset, defanswer, keywords, 
        baseURI, javascript, CSS: string255;
	assessmentURI: string255;
	timelimit: LongInt;
	
	{ the "language" here may be an unsupported language,
	  "lang" in qmsgs.pas is always supported }

        HTMLcode, noindex, neutral, rtl, bidi: boolean;

        { internal stuff: }
	qType: TquestionType;        { type of actual question }
        questionNr: integer;         { number of actual question }
	answerNr: integer;           { number of answer for actual question }
        started: boolean;            { already started? }
	evaluated: boolean;          { already ealuated? }
        quit: boolean;               { quit from interactive apps }
	startTime: LongInt;          { set in StartQuiz }

        Points,
	MaxPoints,                   { maximum points for quiz }
	thisMaxPoints : pointsType;  { maximum points for this question }

      public
        constructor Init(infile: string);
	constructor InitCfg(cfgfile, infile: string); { experimental }
        destructor Done;			    virtual;
	
        procedure resetQuiz;                        virtual;
        procedure process;
        procedure StartQuiz;                        virtual;
	procedure ignoreBlock;
        procedure processComment;                   virtual;
	procedure processHint;                      virtual;
        procedure processQuestion;                  virtual;
        procedure processMulti;                     virtual;
	procedure processFitBest;                   virtual;
        procedure processAssessment;                virtual;
	function  readAssessmentPercent: mystring;  
        procedure processAssessmentPercent;         virtual;
	procedure setcharset(cs: string);           virtual;
	procedure setconverter(p: Tconverter);
	{ called from StartQuiz: }
	procedure convertsettings;                  virtual;
        procedure EndQuiz;                          virtual; { empty }
	function  checkTimeout: boolean;            virtual;
        procedure evaluate;                         virtual;
	procedure seekAssessment;
        function readLine: mystring;                virtual;
        procedure readAnswer(var p: pointsType; 
	                     var s: mystring);      virtual;
        procedure error;  			    virtual;
	function htmlconvert(const x: mystring): mystring; virtual;

        procedure readRawLine(var s: mystring);
        function checkEOF: boolean;
        function nextLineEmpty: boolean;
        procedure gotoEOF;
	function getTitle: mystring;
	function getAuthor: mystring;
        function getAuthorURI: mystring;
	function getCopyright: mystring;
	function getTranslator: mystring;
	function getLanguage: mystring;
	function getCharset: mystring;
	function getDefAnswer: mystring;
	function getbaseURI: mystring;
	function getJavascript: mystring;
	function getCSS: mystring;
	function getneutral: boolean;
	function getHTMLcode: boolean;
	function getNoindex: boolean;
	function getPoints: pointsType;
	function getMaxPoints: pointsType;
	function getPercentage: integer;
        function stopQuiz: boolean;
	function quizstarted: boolean;
      private
      	procedure useQuizLanguage;
        function testStarted: boolean;
    end;

function getPointsFromLine(var p: pointsType; var s: mystring):boolean;

{ for the interactive AKFQuiz interpreters }
function KeyToValue(c: char): integer;
function ValueToKey(v: integer): char;

{ some functions, not directly related to AKFQuiz }
function replaceall(s: mystring; const r1, r2: string): mystring;
function format(var s: mystring; len, MaxLen: integer): mystring;

{ --------------------------------------------------------------------- }

implementation
{$IfDef FPC} 
  uses math; { for "min" }
{$EndIf}

{ for the interactive AKFQuiz interpreters }
function KeyToValue(c: char): integer;
begin
c := Upcase(c);
if (c>='0') and (c<='9') 
   then KeyToValue := ord(c)-48
   else if (c>='A') and (c<='Z')
           then KeyToValue := ord(c)-65+10
	   else KeyToValue := -1
end;

function ValueToKey(v: integer): char;
begin
if v < 10 
   then ValueToKey := chr(v+48) { number character }
   else ValueToKey := chr(v-10+65) { uppercase letter }
end;

{ "format" returns as much as fits and leaves the rest in the parameter }
{ len is the rest of the current line, MaxLen the whole length }
function format(var s: mystring; len, maxlen: integer): mystring;
const wseparators = [' ', TAB, '-', '/'];
var cut: integer;
begin
if length(s) < len
  then begin format := s; s := '' end
  else
    begin
    cut := len;
    while (cut>0) and not (s[cut] in wseparators) do dec(cut);
    if cut<>0
       then begin { line can be splitted }
            if (s[cut]=' ') or (s[cut]=TAB)
	      then format := copy(s, 1, cut-1)
	      else format := copy(s, 1, cut);
            delete(s, 1, cut)
            end
       else begin { first word of s doesn't fit }
            cut := 1;
	    while (cut<length(s)) and 
	          not (s[cut] in wseparators) do inc(cut);
            if cut < MaxLen
               then format := '' { word fits in a new line }
               else begin { single word must be splitted :-( }
                    format := copy(s, 1, len);
                    delete(s, 1, len)
                    end
            end
    end
end;

function replaceall(s: mystring; const r1, r2: string): mystring;
var
  rest: mystring;
  p : LongInt;
begin

if pos(r1, r2)<>0 then RunError; { to avoid enless loops }

p := pos(r1, s);
while p<>0 do
  begin
  rest := s;
  delete(rest, 1, p+length(r1)-1);
  s := copy(s, 1, p-1) + r2 + rest;
  p := pos(r1, s)
  end;
replaceall := s
end;

{ gets string after colon, leading spaces stripped }
function getvalue(x: string): mystring;
var i: integer;
begin
i := succ(pos(':', x));
while (x[i]=' ') or (x[i]=TAB) do inc(i);
getvalue := copy(x, i, length(x)-i+1);
end;

{ getvalue with fallback }
function getvaluefb(x, fallback: string): mystring;
begin
x := getvalue(x);
if x='' then x := fallback;
getvaluefb := x
end;

{ get boolean value }
function getboolvalue(x: string): boolean;
begin
x := getvalue(x);
getboolvalue := 
  (x='1') or ((length(x)>=1) and (UpCase(x[1]) in ['T','Y','J','O']))
end;

function gettimevalue(x: string): LongInt;
var 
  e: LongInt;
  code, multiplyer : word;
begin
code := 0; { no error }

x := getvalue(x);
case x[length(x)] of
  's' : multiplyer := 1;
  'm' : multiplyer := 60;
  'h' : multiplyer := 60*60;
  otherwise multiplyer := 0; { wrong suffix }
  end;

x := copy(x, 1, length(x)-1);
val(x, e, code);
if (code<>0) or (multiplyer=0)
  then gettimevalue := -1
  else gettimevalue := e * multiplyer
end;

{ --------------------------------------------------------------------- }

constructor Takfquiz.Init(infile: string);
begin
assign(inp, infile);
{$IfDef Buffer}
  SetTextBuf(inp, Buffer);
{$EndIf}

resetQuiz;
if (IOResult<>0) or checkEOF then fail
end;

{ Init with configuration file }
constructor Takfquiz.InitCfg(cfgfile, infile: string);
begin
assign(inp, cfgfile);
{$IfDef Buffer}
  SetTextBuf(inp, Buffer);
{$EndIf}

resetQuiz;
process;
close(inp);
if IOResult<>0 then fail;
if started then fail; { mustn't be started in config file! }

assign(inp, infile);
{$IfDef Buffer}
  SetTextBuf(inp, Buffer);
{$EndIf}
reset(inp);
if (IOResult<>0) or CheckEOF then fail
end;

destructor Takfquiz.Done;
begin close(inp) end;

procedure Takfquiz.resetQuiz;
begin
title         := def_title;
author        := def_author;
authorURI     := def_authorURI;
translator    := def_translator;
edited        := def_edited;
copyright     := def_copyright;
license       := def_license;
licenseURI    := def_licenseURI;
language      := def_language;
charset       := def_charset;
baseURI       := def_baseURI;
javascript    := def_javascript;
neutral       := def_neutral;
HTMLcode      := def_HTMLcode;
CSS           := def_CSS;
noindex       := def_noindex;
rtl           := def_rtl;
bidi          := def_bidi;
defanswer     := def_defanswer;
assessmentURI := def_assessmentURI;
timelimit     := 0;

quit          := false;
started       := false;
evaluated     := false;
qType         := MC;   { just to have something meaningful }

Points        := 0;
MaxPoints     := 0;
thisMaxPoints := 0;
htmltagskip   := false;
convert       := noconversion;
startTime     := 0;

setconverter(convert);
reset(inp)
end;

procedure Takfquiz.useQuizLanguage;
var l: string[2];
begin
l := makeUpcase(copy(Language,1,2));
if l='EN' then lang := english;
if l='DE' then lang := deutsch;
if l='DA' then lang := dansk;
if l='IT' then lang := italiano
{ else unchanged }
end;

function Takfquiz.readLine: mystring;
var 
  res, s: mystring;
  okay : boolean;
begin
res := '';

if not eof(inp) then
  repeat
    okay := true; { assume line ends here }
    ReadLn(inp, s);
    if IOResult<>0 then error;
    s := stripWhitespace(htmlconvert(s));
    if htmltagskip and (s='') then okay := false;
    if s='' then continue;
    if pos('#',s)=1 then
       begin
       okay := false;
       continue
       end;
    if s[length(s)]<>'\'
       then res := res + s
       else begin
            res := res + stripWhitespace(copy(s, 1, length(s)-1)) + ' ';
  	    okay := false
	    end
  until okay;
if started then res := convert(res);
res := replaceall(res, '\-', '-');
readLine := res
end;

{ strips points from line }
function getPointsFromLine(var p: pointsType; var s: mystring):boolean;
var 
  i: integer;
  e: integer;
begin
getPointsFromLine := false; { assumption }

if s='' then exit;

{ correction for GPG clearsigned quiz-files with dash-escape }
if (s[1]='-') and (s[2]=' ') then Delete(s, 1, 2);

{ search for whitespace: }
i:=1;
while (i<length(s)) and (s[i]<>' ') and (s[i]<>TAB) do inc(i);

{ no whitespace? }
if i=length(s) then exit;

val(copy(s, 1, i-1), p, e);
if e<>0 then exit;

Delete(s, 1, i);
s := stripWhitespace(s);
getPointsFromLine := true
end;

procedure Takfquiz.readAnswer(var p: pointsType; var s: mystring);
var e : integer;
begin
p := 0;
s := readline;

if s<>'' then
   begin
   if not getPointsFromLine(p, s) then
      begin { try old format }
      val(readline, p, e);
      if e<>0 then error
      end;
   
   case qType of
     { only the best answer counts: }
     MC, textfield : thisMaxPoints := Max(thisMaxPoints, p);
     { count all positive points: }
     MCMA : if p > 0 then inc(thisMaxPoints, p);
     end
   end
end;

procedure Takfquiz.evaluate;
begin 
evaluated := true
end;

procedure Takfquiz.StartQuiz;
begin
{ check whether value for timelimit is correct }
if timelimit<0 then error;

startTime := GetSecs; { may be set again in child processes }

started := true;
convertsettings
end;

function Takfquiz.htmlconvert(const x: mystring): mystring;
var
 i: integer;
 e: mystring;
begin
if not HTMLcode
   then htmlconvert := x
   else begin
        e := '';
        i := 1;
        while i <= length(x) do
	  begin
	  if x[i]='<' then htmltagskip := true;
	  if not htmltagskip then 
	    if x[i]='&' 
	       then begin
	            if copy(x, i, 4)='&lt;' then 
		       begin e := e + '<'; inc(i, 3) end;
	            if copy(x, i, 4)='&gt;' then 
		       begin e := e + '>'; inc(i, 3) end;
	            if copy(x, i, 5)='&amp;' then 
		       begin e := e + '&'; inc(i, 4) end;
	            if copy(x, i, 6)='&quot;' then 
		       begin e := e + '"'; inc(i, 5) end;
	            if copy(x, i, 6)='&euro;' then 
		       begin e := e + 'Euro'; inc(i, 5) end;
	            end
	       else e := e + x[i];
	  if x[i]='>' then htmltagskip := false;
          inc(i)
	  end; { while }
	htmlconvert := e
        end { if not HTMLcode }
end;

function Takfquiz.testStarted: boolean; {@@@}
begin
if not started then StartQuiz;
testStarted := started and not quit
end;

procedure Takfquiz.setconverter(p: Tconverter);
begin
convert := p
end;

procedure Takfquiz.convertsettings;
begin
{ authorURI, licenseURI: don't convert URIs }
title      := convert(title); 
author     := convert(author);
translator := convert(translator);
license    := convert(license);
copyright  := convert(copyright);
defanswer  := convert(defanswer);
keywords   := convert(keywords)
end;

procedure Takfquiz.setcharset(cs: string);
begin
charset := cs
end;

procedure Takfquiz.ignoreblock;
var s: mystring;
begin
s := readLine;
while (s<>'') and not checkEOF do s := readLine
end;

{ simply ignore as default }
procedure Takfquiz.processComment;
begin ignoreblock end;

procedure Takfquiz.processHint;
begin ignoreblock end;

{ simply ignore as default }
procedure Takfquiz.processAssessment;
begin ignoreblock end;

function Takfquiz.readAssessmentPercent: mystring;
var 
  s: mystring;
  value, oldvalue: pointsType;
  reached: integer;
  found: boolean;
begin
found := false;
oldvalue := 101; { more than 100 }
reached  := getPercentage;
readAssessmentPercent := '';

s := readLine;
while s<>'' do
  begin
  if not getPointsFromLine(value, s) then error;
  if value >= oldvalue then error; { enforce descending order }
  if (not found) and (reached >= value) and (MaxPoints<>0) then 
     begin
     readAssessmentPercent := s;
     found := true
     end;
  s := readLine;
  oldvalue := value
  end;

{ last value must be 0! }
if value <> 0 then error
end;


{ ignore as default, but check syntax }
procedure Takfquiz.processAssessmentPercent;
begin 
readAssessmentPercent
end;

procedure Takfquiz.processQuestion;
begin 
qType := MC;
inc(questionNr);
answerNr := 0;
thisMaxPoints := 0
end;

procedure Takfquiz.processMulti;
begin 
qType := MCMA;
inc(questionNr);
answerNr := 0;
thisMaxPoints := 0
end;

procedure Takfquiz.processFitBest;
begin 
qType := textfield;
inc(questionNr);
answerNr := 0;
thisMaxPoints := 0
end;

procedure Takfquiz.process;
var s, e: mystring;
begin
started       := false;
evaluated     := false;
quit          := false;
questionNr    := 0;
answerNr      := 0;
Points        := 0;
MaxPoints     := 0;
thisMaxPoints := 0;

repeat
  s := makeUpcase(readLine)
until (pos('AKFQUIZ', s)=1) or (pos('QUIZ', s)=1) or checkEOF;

if checkEOF then error; { no AKFQuiz header found }

while not checkEOF and not quit do
   begin
   s := readLine;
   e := makeUpcase(s);

   if (s<>'') and 
      (e<>'END') and 
      (e<>'ENDE') and 
      (pos(':',s)=0) then
     begin { if no keyword, no comment }
     error;
     { there is normally more than one line with this error }
     while not checkEOF and (s<>'') and (pos(':',s)=0) do s := readLine
     end;

   if (pos('TITLE:',e)=1) or
      (pos('TITEL:',e)=1)
          then title := getvaluefb(s, def_title);
   if (pos('AUTHOR:',e)=1) or
      (pos('AUTOR:',e)=1)
          then author:=getvalue(s);
   if (pos('AUTHORURI:',e)=1) or
      (pos('AUTHORURL:',e)=1) or
      (pos('AUTHORLINK:',e)=1) or
      (pos('AUTORURI:',e)=1) or
      (pos('AUTORURL:',e)=1) or
      (pos('AUTORLINK:',e)=1)
          then authorURI:=getvalue(s);
   if (pos('EDITED:',e)=1) or
      (pos('BEARBEITET:',e)=1)
          then edited:=getvalue(s);
   if (pos('LICENSE:',e)=1) or
      (pos('LIZENZ:',e)=1)
          then license:=getvalue(s);
   if (pos('LICENSEURI:',e)=1) or
      (pos('LIZENZURI:',e)=1) or
      (pos('LICENSELINK:',e)=1) or
      (pos('LIZENZLINK:',e)=1)
          then licenseURI:=getvalue(s);
   if (pos('COPYRIGHT:',e)=1)   
          then copyright:=getvalue(s);
   if (pos('TRANSLATOR:',e)=1) or
      (pos('UEBERSETZER:',e)=1) 
          then translator:=getvalue(s);
   if (pos('CHARSET:',e)=1) or
      (pos('ZEICHENSATZ:',e)=1) 
          then setcharset(getvaluefb(s, def_charset));
   if (pos('RTL:',e)=1) 
          then rtl := getboolvalue(s);
   if (pos('BIDI:',e)=1) 
          then bidi := getboolvalue(s);
   if (pos('LANGUAGE:',e)=1) or
      (pos('SPRACHE:',e)=1)
          then begin language := getvalue(s); useQuizLanguage end;
   if pos('NEUTRAL:',e)=1
          then neutral := getboolvalue(s);
   if (pos('TIMEOUT',e)=1) or
      (pos('TIMELIMIT',e)=1) or
      (pos('ZEITLIMIT',e)=1)
          then timelimit := gettimevalue(s);
   if pos('HTMLCODE:',e)=1
          then HTMLcode:=getboolvalue(s);
   if (pos('BASEURI:',e)=1) or
      (pos('BASEURL:',e)=1) or
      (pos('HAUPTURI:',e)=1) or
      (pos('HAUPTURL:',e)=1) 
          then begin baseURI := getvalue(s);
                     { make sure, that it ends with a / }
	             if baseURI<>'' then 
                        if baseURI[length(baseURI)]<>'/'
			   then baseURI := baseURI+'/'
               end;
   if (pos('ASSESSMENTURI:',e)=1) or
      (pos('ASSESSMENTURL:',e)=1) or
      (pos('AUSWERTUNGSURI:',e)=1) or
      (pos('AUSWERTUNGSURL:',e)=1) or
      (pos('AUSWERTUNGURI:',e)=1) or
      (pos('AUSWERTUNGURL:',e)=1) or
      (pos('ASSESSMENTLINK:',e)=1) or
      (pos('AUSWERTUNGSLINK:',e)=1) or
      (pos('AUSWERTUNGLINK:',e)=1) then assessmentURI:=getvalue(s);
   if (pos('CSS:',e)=1) or
      (pos('LAYOUT:',e)=1)
          then CSS:=getvalue(s);
   if (pos('NOINDEX:',e)=1)  
          then noindex := getboolvalue(s);
   if (pos('KEYWORDS:',e)=1)  or
      (pos('STICHWORTE:',e)=1)
          then keywords := getvalue(s);
   if (pos('JAVASCRIPT:',e)=1) 
          then javascript := getvaluefb(s, def_javascript);
   if (pos('DEFAULT:',e)=1) or
      (pos('STANDARDANTWORT:',e)=1) 
          then defanswer:=getvalue(s);
   if (e='INTRO:') or
      (e='COMMENT:') or
      (e='KOMMENTAR:')
          then if testStarted then processComment;
   if (e='HINT:') or
      (e='REMARK:') or
      (e='HINWEIS:') or
      (e='ANMERKUNG:')
          then if testStarted then processHint;
   if (e='QUESTION:') or
      (e='FRAGE:') or
      (e='MC:')
          then begin
               if testStarted then processQuestion;
	       inc(MaxPoints, thisMaxPoints)
	       end;
   if (e='MULTI:') or
      (e='QUERY:') or
      (e='ANFRAGE:') or
      (e='MCMA:')
          then begin
               if testStarted then processMulti;
	       inc(MaxPoints, thisMaxPoints)
	       end;
   if (e='FITBEST:') or
      (e='FREEFORM:') or
      (e='FREIFORM:') 
          then begin
               if testStarted then processFitBest;
	       inc(MaxPoints, thisMaxPoints)
	       end;
   if (e='ASSESSMENT:') or
      (e='AUSWERTUNG:')
          then if teststarted then 
	          begin
		  if not evaluated then evaluate;
		  processAssessment
		  end;
   if (e='ASSESSMENT%:') or
      (e='AUSWERTUNG%:')
          then if teststarted then
	          begin
		  if not evaluated then evaluate;
		  processAssessmentPercent
		  end;
   if (e='END') or
      (e='ENDE') then gotoEOF;
   
   { doesn't work!
     problem: MaxPoints aren't counted }
   {
   if quizstarted and not evaluated then
     if checkTimeout then
       begin
       evaluate;
       seekAssessment
       end
   }
   
   end;
if started then EndQuiz
end;

function Takfquiz.getPoints: pointsType;
begin
getPoints := Points
end;

function Takfquiz.getMaxPoints: pointsType;
begin
getMaxPoints := MaxPoints
end;

function Takfquiz.getPercentage: integer;
begin
getpercentage := round(max(Points,0)*100/MaxPoints)
end;

function Takfquiz.getTitle: mystring;
begin getTitle := title end;

function Takfquiz.getAuthor: mystring;
begin getAuthor := Author end;

function Takfquiz.getAuthorURI: mystring;
begin getAuthorURI := AuthorURI end;

function Takfquiz.getCopyright: mystring;
begin getCopyright := Copyright end;

function Takfquiz.getTranslator: mystring;
begin getTranslator := translator end;

function Takfquiz.getLanguage: mystring;
begin getLanguage := Language end;

function Takfquiz.getCharset: mystring;
begin getCharset := charset end;

function Takfquiz.getDefAnswer: mystring;
begin getDefAnswer := defAnswer end;

function Takfquiz.getJavascript: mystring;
begin getJavascript := Javascript end;

function Takfquiz.getbaseURI: mystring;
begin getbaseURI := baseURI end;

function Takfquiz.getCSS: mystring;
begin getCSS := CSS end;

function Takfquiz.getneutral: boolean;
begin getneutral := neutral end;

function Takfquiz.getHTMLcode: boolean;
begin getHTMLcode := HTMLcode end;

function Takfquiz.getNoIndex: boolean;
begin getNoIndex := noindex end;

function Takfquiz.stopQuiz: boolean;
begin stopQuiz := quit end;

function Takfquiz.quizstarted: boolean;
begin quizstarted := started end;

function Takfquiz.checkEOF: boolean;
begin
checkEOF := EOF(inp)
end;

procedure Takfquiz.readRawLine(var s: mystring);
begin
if eof(inp) 
  then s:=''
  else ReadLn(inp, s)
end;

function Takfquiz.nextLineEmpty: boolean;
begin
nextLineEmpty := seekeoln(inp)
end;

procedure Takfquiz.gotoEOF;
begin
while not EOF(inp) do ReadLn(inp)
end;

procedure Takfquiz.seekAssessment;
var s: mystring;
begin
s := '';
while not EOF(inp) and 
     (pos('ASSESSMENT',s)<>1) and
     (pos('AUSWERTUNG',s)<>1) and
     (s<>'END') and (s<>'ENDE')
  do s := makeUpcase(readline);
  
if (s='ASSESSMENT:') or (s='AUSWERTUNG:') then processAssessment;
if (s='ASSESSMENT%:') or (s='AUSWERTUNG%:') then processAssessmentPercent;
if (s='END') or (s='ENDE') then gotoEOF
end;

procedure Takfquiz.error;
begin quit := true end;

procedure Takfquiz.EndQuiz;
begin end;

function Takfquiz.checkTimeout: boolean;
begin
if TimeLimit>0
  then checkTimeout := (GetSecs >= TimeLimit+StartTime)
  else checkTimeout := false
end;

end.
