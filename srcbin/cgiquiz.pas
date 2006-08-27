{
* cgiquiz (aka "akfquiz.cgi")
* CGI/1.1 runtime for a quiz from a simplified input script
*
* Needs a CGI/1.1 compatible web-server (boa, apache, ...)
* Needs the files "falsch.png", "richtig.png",
* and optionally a given CSS file in the same directory with 
* the input file or in a directory set by "baseURI:"
*
* $Id: cgiquiz.pas,v 1.9 2006/08/27 06:46:16 akf Exp $
*
* Copyright (c) 2003-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: GNU-Pascal or FreePascal
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
}

{$IfDef _WIN32} {$Define Win32} {$EndIf}

{$IfDef Win32}
  {$R w32/cgiquiz}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}

  {$IfDef Win32}
    {$AppType Console}
  {$EndIf}
{$EndIf}

{$I-}
{$R+}
{$X+}


program cgiquiz(input, output); { aka "akfquiz.cgi" }

{$IfDef __GPC__}
  import uakfquiz; htmlquiz; qmsgs; qsys;
         GPC (GetEnv => GetEnvironmentVariable); 
{$Else}
    uses SysUtils, qsys, qmsgs, uakfquiz, htmlquiz;
{$EndIf} { __GPC__ }


{ GNU compliant format }
const PrgVersion = 'cgiquiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const
  grRight = 'richtig.png';
  grFalse = 'falsch.png';

type
  Pcgiquiz = ^Tcgiquiz;
  Tcgiquiz =
    object(Thtmlquiz)
      protected
        { where all needed files are 
	  either BaseURI or derived from PATH_INFO }
	DocumentURI : mystring;
	
      public
        constructor init;
	destructor Done;                         virtual;
	procedure HTTPdata;
	procedure error;                         virtual;
	procedure startForm;                     virtual;
        procedure headdata;                      virtual;
        procedure StartQuiz;                     virtual;
        procedure EndQuiz;                       virtual;
	procedure bottom;                        virtual;
	procedure processDefaultAnswer;          virtual;
	procedure processAssessment;             virtual;
	procedure processHint;                   virtual;
	procedure showanswer(value: pointsType;
                             const ans: string); 
		                                 virtual;
    end;

type 
  Pcgianswer = ^Tcgianswer;
  Tcgianswer =    
    object(Tcgiquiz)
      private
	CGIElement : string255; { just one Element }
	Home       : mystring;
        Name       : string255;
	AnsPoints  : pointsType;

      public
        constructor init;
	procedure startForm;                     virtual;
        procedure startQuiz;                     virtual;
	procedure putgraphic;                    virtual;
	procedure processDefaultAnswer;          virtual; { empty }
	procedure evaluate;                      virtual;
	procedure bottom;                        virtual;
	procedure processAnswer;                 virtual;
	procedure processAssessment;             virtual;
	procedure processAssessmentPercent;      virtual;
	procedure processHint;                   virtual;
	function calculateAssessmentURI: mystring;
	procedure showanswer(value: pointsType;
                             const ans: string); virtual;
    end;

var ExamMode: boolean = false;

var 
  CGI_PATH_INFO,
  CGI_PATH_TRANSLATED: myString;

{$IfDef __GPC__}
  var 
    CGI_QUERY_STRING: PString;
    QUERY_STRING_POS: Integer;
{$Else}
  var 
    CGI_QUERY_STRING: PAnsiString;
    QUERY_STRING_POS: LongInt;
{$EndIf}

function isLastElement: boolean;
begin
if CGI_QUERY_STRING=NIL
  then isLastElement := true
  else 
   if QUERY_STRING_POS < Length(CGI_QUERY_STRING^)
     then isLastElement := false
     else isLastElement := true
end;

procedure GetCGIElement(var s: String255);
var 
  i,err: integer;
  c, c1: char;
  nextElement: boolean;

  function getNextChar: char;
  begin
  inc(QUERY_STRING_POS);
  getNextChar := CGI_QUERY_STRING^[QUERY_STRING_POS]
  end;

begin { GetCGIElement }
s := '';
if isLastElement then exit;

repeat
  c := getNextChar;
  nextElement := (c='&'); { an encoded '&' doesn't hurt here yet }
  if c='+' then c := ' ';
  if c='%' then { hexadecimal encoded }
    begin
    { read 2 hex-chars }
    c  := getNextChar;
    c1 := getNextChar;
    val('$'+c+c1, i, err); { convert them to an integer }
    c := chr(i) { and that integer to a char }
    end;
  if not nextElement then s := s + c
until nextElement or isLastElement
end;

procedure version;
var Browser: boolean;
begin
{ is this called from a Browser? }
Browser := (GetEnvironmentVariable('REQUEST_METHOD')<>'');

if not Browser then 
  begin
  useSystemLanguage;
  setmsgconv(checkDisplay)
  end;

if Browser then { send CGI Header }
  begin
  WriteLn('Content-Type: text/html; charset=UTF-8');
  WriteLn;
  WriteLn(HTMLDocType);
  WriteLn;
  WriteLn('<html>');
  WriteLn('<head>');
  WriteLn('<title>' + AKFQuizName + ': version</title>');
  WriteLn('<meta name="robots" content="noindex">');
  WriteLn('</head>');
  WriteLn;
  WriteLn('<body>');
  WriteLn('<h1>', AKFQuizName, '</h1>');
  WriteLn
  end;

{ the text }
WriteLn(PrgVersion);
WriteLn;
If Browser then WriteLn('<br><br>');
Write('Copyright');
If Browser then Write(' &copy; ') else Write(' (C) '); 
WriteLn(AKFQuizCopyright);
If Browser then WriteLn('<br>');
Write('Copyright');
If Browser then Write(' &copy; ') else Write(' (C) '); 
WriteLn('1999-2001 Free Software Foundation, Inc.');
If Browser then WriteLn('<br><br>');
WriteLn;
WriteLn(msg_License, msg_GPL);
WriteLn;
If Browser then WriteLn('<pre>');
WriteLn(msg_noWarranty);
WriteLn;
WriteLn('Written by Andreas K. Foerster');
If Browser then WriteLn('</pre></body></html>');
Halt
end;


procedure help;
var 
  Browser: boolean;
  CGIBase: mystring;
begin
{ is this called from a Browser? }
Browser := (GetEnvironmentVariable('REQUEST_METHOD')<>'');

if Browser
  then CGIBase := '<strong>http://'
                  + GetEnvironmentVariable('HTTP_HOST')
                  + GetEnvironmentVariable('SCRIPT_NAME')
	          + '</strong>'
  else CGIBase := 'http://example.org/cgi-bin/cgiquiz';

if Browser then { send CGI Header }
  begin
  WriteLn('Content-Type: text/html; charset=UTF-8');
  WriteLn;
  WriteLn(HTMLDocType);
  WriteLn;
  WriteLn('<html>');
  WriteLn('<head>');
  WriteLn('<title>' + AKFQuizName + ': help</title>');
  WriteLn('<meta name="robots" content="noindex">');
  WriteLn('</head>');
  WriteLn;
  WriteLn('<body>');
  WriteLn('<h1>', AKFQuizName, '</h1>');
  WriteLn
  end;

{ the text }

If Browser 
  then begin
       WriteLn(PrgVersion);
       WriteLn('<br><br>');
       WriteLn('Quiz-program for the CGI interface of a webserver');
       WriteLn('<br><br>');
       WriteLn('<dl><dt>Usage:</dt>');
       WriteLn('<dd>');
       WriteLn('cgiquiz [ --help | -h | /? ]<br>');
       WriteLn('cgiquiz --version');
       WriteLn('<br><br></dd>');
       WriteLn('<dt>Examples:</dt>');
       WriteLn('<dd>');  
       WriteLn(CGIBase, '/--help<br>');
       WriteLn(CGIBase, '/--version<br>');
       WriteLn(CGIBase, '/quizpath/<br>');
       WriteLn(CGIBase, '/quizpath/myquiz.akfquiz');
       WriteLn(CGIBase, '/quizpath/myquiz.akfquiz?q1=2&amp;q2=1&amp;q5=2');
       WriteLn('</dd></dl>');
       WriteLn;
       if BugMail<>'' then
         WriteLn('Report bugs to <a href="mailto:' + BugMail + '">'
                 + BugMail+'</a>.');
       WriteLn('</body></html>')
       end
  else begin { not Browser }
       WriteLn('Quiz-program for the CGI interface of a webserver');
       WriteLn;
       WriteLn('Usage: cgiquiz [ --help | -h | /? ]');
       WriteLn(' or:   cgiquiz --version');
       WriteLn;
       WriteLn('Examples:');
       WriteLn(' > ', CGIBase, '/--help');
       WriteLn(' > ', CGIBase, '/--version');
       WriteLn(' > ', CGIBase, '/quizpath/');
       WriteLn(' > ', CGIBase, '/quizpath/myquiz.akfquiz');
       WriteLn(' > ', CGIBase, '/quizpath/myquiz.akfquiz?q1=2&q2=1&q5=2');
       
       if BugMail <> '' then
         begin WriteLn; WriteLn('Report bugs to <' + BugMail + '>.') end
	 
       end;
Halt
end;

{ sets "lang" according the browser settings }
procedure useBrowserLanguage;
var s: myString;
begin
{ first just analyze the first language }
s := copy(GetEnvironmentVariable('HTTP_ACCEPT_LANGUAGE'), 1, 2);

if s='de' then lang := deutsch
  else if s='it' then lang := italiano
    else if s='da' then lang := dansk
      else if s='en' then lang := english
        else begin 
             { first language unknown - check for others: }
             s := GetEnvironmentVariable('HTTP_ACCEPT_LANGUAGE');
  	     if pos('de', s)<>0 then lang := deutsch
	       else if pos('it', s)<>0 then lang := italiano
	         else if pos('da', s)<>0 then lang := dansk
	  	   else lang := english
             end
end;

{ prepare CGI_PATH_TRANSLATED when -exam or --exam is used }
procedure prepareExam(l: integer);
var s: mystring;
begin
ExamMode := true;

s := CGI_PATH_INFO; 
delete(s, 1, l);
CGI_PATH_TRANSLATED := ExamDir + s
end;

{ --------------------------------------------------------------------- }
{ Error handling procedures }

procedure errorHTMLhead(status: integer; s: string);
begin
WriteLn('Status: ', status, ' "AKFQuiz: ', s, '"');
WriteLn('Content-Type: text/html; charset=UTF-8');
WriteLn;
WriteLn(HTMLDocType);
WriteLn;
WriteLn('<html>');
WriteLn('<head>');
WriteLn('<title>AKFQuiz: ', msg_error, ' ', s, '</title>');
WriteLn('</head>');
WriteLn;
WriteLn('<body style="background-color:red; color:white">');
WriteLn('<h1>', msg_error, '</h1>');
WriteLn;
Write('<p>')
end;

procedure errorHTMLfoot;
begin
WriteLn('</p>');
WriteLn;
WriteLn('<hr>', PrgVersion);
WriteLn('</body>');
WriteLn('</html>')
end;

procedure NotFound;
begin
errorHTMLhead(404, 'file not found');
WriteLn(msg_filenotfound);
errorHTMLfoot;
Halt(1)
end;

procedure CannotOpen;
begin
errorHTMLhead(404, 'cannot open file');
case lang of 
  deutsch : WriteLn('Fehler: kann "',CGI_PATH_INFO,'" nicht öffnen');
  otherwise WriteLn('Error: cannot open "', CGI_PATH_INFO, '"')
  end;
errorHTMLfoot;
Halt(1)
end;

procedure Forbidden;
begin
errorHTMLhead(403, 'request forbidden');
case lang of
  deutsch : WriteLn('Fehler: keine Berechtigung diesen Pfad zu benutzen');
  otherwise WriteLn('Error: you don''t have permission to enter this path')
  end;
errorHTMLfoot;
Halt(1)
end;

procedure FileError;
begin
errorHTMLhead(403, 'Error in file');
case lang of 
  deutsch : WriteLn('Fehler in Datei "',CGI_PATH_INFO, '"');
  otherwise WriteLn('Error in file "', CGI_PATH_INFO, '"')
  end;
errorHTMLfoot;
Halt(1)
end;

{ --------------------------------------------------------------------- }

constructor Tcgiquiz.Init;
var i : integer;
begin
{ get the DocumentURI - where all needed files are }
{ may be overwritten later by the keyword "baseURI:" }
DocumentURI := CGI_PATH_INFO;
i := Length(DocumentURI);
if i=0 then fail;
while (DocumentURI[i]<>'/') and (i>1) do dec(i);
DocumentURI := copy(DocumentURI, 1, i);

{ empty string '' stands for stdout }
inherited Init(CGI_PATH_TRANSLATED, '');
if checkEOF then fail
end;

destructor Tcgiquiz.Done;
begin
inherited Done;
if (IOResult<>0) or not started then CannotOpen
end;

procedure Tcgiquiz.HTTPdata;
begin
WriteLn(outp, 'Content-Type: text/html; charset=', charset);
if language<>'' then
  WriteLn(outp, 'Content-Language: ', language);
WriteLn(outp)
end;

procedure Tcgiquiz.error;
begin
if started 
  then inherited error
  else FileError
end;

procedure Tcgiquiz.headdata;
begin
WriteLn(outp);
if pos('http', DocumentURI)=1
  then WriteLn(outp, '<base href="', DocumentURI, '">')
  else WriteLn(outp, '<base href="http://', 
                       GetEnvironmentVariable('HTTP_HOST'),
		       DocumentURI, '">');
WriteLn(outp);

inherited headdata
end;

procedure Tcgiquiz.StartForm;
begin
WriteLn(outp);
WriteLn(outp, '<form name="akfquiz" id="akfquiz" method="POST" action="',
              GetEnvironmentVariable('SCRIPT_NAME'), CGI_PATH_INFO, '">');

{WriteLn(outp, '<form name="akfquiz" id="akfquiz" method="POST" action="',
              '/cgi-bin/cgi-test.cgi', CGI_PATH_INFO, '">');}

{ remember the origin: }
if GetEnvironmentVariable('HTTP_REFERER')<>'' then
  WriteLn(outp, '<input type="hidden" name="home" id="home" value="', 
  	        GetEnvironmentVariable('HTTP_REFERER'), '"', cet);

{ in ExamMode ask for Name }
if ExamMode then
  WriteLn(outp, '<div class="name"><label for="name">Name:</label> '
    + '<input type="text" name="name" id="name" size="50" maxlength="100">'
    + '</div>')
end;

procedure Tcgiquiz.StartQuiz;
begin
if BaseURI<>'' then DocumentURI := BaseURI;
if (CSS<>'') and (pos('/',CSS)=0) then CSS := DocumentURI + CSS;

HTTPdata;
inherited StartQuiz;

StartForm
end;

procedure Tcgiquiz.processDefaultAnswer;
begin
inc(answerNr);
WriteLn(outp, '<div class="defanswer">');
WriteLn(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
	      ' name="q', questionNr, '"',
              ' type="radio" value="',
              answerNr, '" checked', cet); { change-xhtml }

Write(outp, '<label for="q', questionNr, 'a', answerNr, '">');
Write(outp, defAnswer);
WriteLn(outp, '</label>');
WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Tcgiquiz.showanswer(value: pointsType;
                              const ans: string);
begin
inc(answerNr);

{ it must be included in block containers to make RTL work correctly }
WriteLn(outp, '<div>');
WriteLn(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
	      ' name="q', questionNr, '"',
              ' type="', qTypeStr(qType), '" value="', answerNr, '"', cet);

Write(outp, '<label for="q', questionNr, 'a', answerNr, '">');
Write(outp, ans);
WriteLn(outp, '</label>');
WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Tcgiquiz.processAssessment;
begin
ignoreblock
end;

procedure Tcgiquiz.processHint;
begin
ignoreblock
end;

procedure Tcgiquiz.bottom;
begin
WriteLn(outp, '<div align="center" class="buttons">');
WriteLn(outp, '<input type="submit" accesskey="r" value="',
              msg_result,'"', cet);
WriteLn(outp, '<input type="reset" accesskey="n" value="',
              msg_new, '"', cet);
WriteLn(outp, '</div>');

WriteLn(outp);
WriteLn(outp, '</form>')
end;

procedure Tcgiquiz.EndQuiz;
begin
bottom;
inherited EndQuiz
end;

{ --------------------------------------------------------------------- }

constructor Tcgianswer.init;
begin
inherited init;
if checkEOF then fail;

AnsPoints := 0;
Home := '';
Name := '';

repeat
  GetCGIelement(CGIElement);

  if Pos('home=', CGIElement)=1 then
    begin
    Home := CGIElement;
    Delete(Home, 1, length('home='));
    GetCGIElement(CGIElement)
    end;
  
  if Pos('name=', CGIElement)=1 then
    begin
    Name := CGIElement;
    Delete(Name, 1, length('name='));
    GetCGIElement(CGIElement)
    end
until (CGIElement[1]='q') or isLastElement
end;

procedure Tcgianswer.StartForm;
begin
WriteLn(outp);
{ "action" must always be given, even if it's none }
WriteLn(outp, '<form name="akfquiz" id="akfquiz" action="">');
WriteLn(outp)
end;


procedure Tcgianswer.StartQuiz;
begin
noindex := true; { don't index - no matter what the input file says }

inherited StartQuiz;

if Home<>'' then
  begin
  WriteLn(outp, '<div align="center" class="home"><small>');
  WriteLn(outp, '<a href="', Home, '">', msg_back, '</a>');
  WriteLn(outp, '</small></div>')
  end;

if Name<>'' then
  WriteLn(outp, '<div class="name">Name: ', Name, '</div>')
end;

procedure Tcgianswer.processAnswer;
begin
inherited processAnswer;
AnsPoints := 0
end;


procedure Tcgianswer.showanswer(value: pointsType;
                                const ans: string);
begin
inc(answerNr);
WriteLn(outp, '<div>');
Write(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
	    ' name="q', questionNr, '"',
            ' type="', qTypeStr(qType), 
            '" value="', answerNr, '" disabled'); { change-xhtml }
if CGIElement = 'q'+IntToStr(questionNr)+'='+IntToStr(answerNr) then 
   begin
   inc(Points, value);
   inc(AnsPoints, value);
   Write(outp, ' checked'); { change-xhtml }
   GetCGIElement(CGIElement)
   end;
WriteLn(outp, cet);

Write(outp, '<label for="q', questionNr, 'a', answerNr, '"');
if not neutral then
  if value > 0 
    then Write(outp, ' class="correct"')
    else Write(outp, ' class="wrong"');
Write(outp, '>');

if (value > 0) and not neutral then Write(outp, '<strong>');
Write(outp, ans);
if (value > 0) and not neutral then Write(outp, '</strong>');

WriteLn(outp, '</label>');
WriteLn(outp, '</div>');
WriteLn(outp)
end;


procedure Tcgianswer.processDefaultAnswer;
begin
inc(answerNr);
WriteLn(outp, '<div class="defanswer">');
Write(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
	    ' name="q', questionNr, '"',
            ' type="radio" value="',
            answerNr, '" disabled');
if CGIElement = 'q'+IntToStr(questionNr)+'='+IntToStr(answerNr) then 
   begin
   Write(outp, ' checked'); { change-xhtml }
   GetCGIElement(CGIElement)
   end;
WriteLn(outp, cet);
Write(outp, '<label for="q', questionNr, 'a', answerNr, '"');
if not neutral then Write(outp, ' class="wrong"');
Write(outp, '>');
Write(outp, defAnswer);
WriteLn(outp, '</label>');
WriteLn(outp, '</div>');
WriteLn(outp)
end;


procedure Tcgianswer.putgraphic;
begin
WriteLn(outp, '<strong>'); { for non-graphical browsers }
Write(outp, '<img width="18" height="18"');
if rtl 
  then Write(outp, ' style="float:left; vertical-align:text-bottom;"')
  else Write(outp, ' style="float:right; vertical-align:text-bottom;"');

if AnsPoints = thisMaxPoints
   then WriteLn(outp, ' alt="- ', msg_right, 
                      ' -" src="', DocumentURI+grRight+'"', cet)
   else WriteLn(outp, ' alt="- ', msg_wrong, 
                      ' -" src="', DocumentURI+grFalse+'"', cet);
WriteLn(outp, '</strong>'); { for non-graphical browsers }

Write(outp, '<div class="points">');
Write(outp, msg_points, AnsPoints);
if qType=MCMA then Write(outp, '/', thisMaxPoints);
WriteLn(outp, '</div>')
end;

function tcgianswer.calculateAssessmentURI: mystring;
begin
if assessmentURI='' then begin calculateAssessmentURI := ''; exit end;
if assessmentURI[Length(assessmentURI)]<>'?'
  then calculateAssessmentURI := assessmentURI { unchanged }
  else calculateAssessmentURI := assessmentURI + 
       'points=' + IntToStr(Points) +
       '&maxpoints=' + IntToStr(MaxPoints) +
       '&percent=' + IntToStr(getPercentage)
end;

procedure Tcgianswer.evaluate;
var s: mystring;
begin
WriteLn(outp);
WriteLn(outp, '<hr', cet);
WriteLn(outp);
WriteLn(outp, '<div align="center" class="result"><strong>');
WriteLn(outp, '<a name="result" id="result"></a>');

WriteLn(outp, msg_sol1, Points, msg_sol2,
              MaxPoints, msg_sol3, BR);
if Points > 0
  then WriteLn(outp, msg_sol4, getPercentage, '%.')
  else if not neutral then WriteLn(outp, msg_sol5);

WriteLn(outp, '</strong>');

WriteLn(outp, '</div>');

s := calculateAssessmentURI;
if s<>'' then
  begin
  WriteLn(outp, '<div align="center">');
  Write(outp, BR, '<a href="');
  if pos('/',s)=0 then s := DocumentURI + s;
  WriteLn(outp, s, '">', msg_assessment, '</a>');
  WriteLn(outp, '</div>')
  end;

inherited evaluate
end;

procedure Tcgianswer.processAssessment;
begin
Thtmlquiz.processAssessment
end;

procedure Tcgianswer.processAssessmentPercent;
begin
{ don't use the assessment-block, when assessmentURI is given }
if assessmentURI<>'' 
   then readAssessmentPercent
   else begin
        WriteLn(outp);
        WriteLn(outp, '<div class="assessment">');
        WriteLn(outp, readAssessmentPercent);
        WriteLn(outp, '</div>');
        WriteLn(outp)
	end
end;


procedure Tcgianswer.processHint;
begin
WriteLn(outp);
WriteLn(outp, '<div class="hint">');
processBlock;
WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Tcgianswer.bottom;
begin
if not evaluated then evaluate;

WriteLn(outp, '</form>');

if Home<>'' then
  begin
  WriteLn(outp, '<div align="center" class="home"><small>');
  WriteLn(outp, '<a href="', Home, '">', msg_back, '</a>');
  WriteLn(outp, '</small></div>')
  end
end;

{ --------------------------------------------------------------------- }

function isPath: boolean;
begin
{ Directory-separator not system-specific here }
isPath := (CGI_PATH_INFO[length(CGI_PATH_INFO)]='/')
end;

procedure NoEntriesFound;
begin
WriteLn('<p class="error">');
WriteLn(msg_noquizfound);
WriteLn('</p>')
end;

procedure myshowentry(const dir, s: string);
begin
WriteLn('<a href="', s, '">');
WriteLn(getQuizTitle(CGI_PATH_TRANSLATED + s), '</a><br>') { change-xhtml }
end;

procedure showList;
var found: boolean;
begin
{ the Quiz-titles are recoded to UTF-8 }
WriteLn('Content-Type: text/html; charset=UTF-8');
WriteLn('Cache-control: no-cache');
WriteLn;
WriteLn(HTMLDocType);
WriteLn;
WriteLn('<html>');
WriteLn('<head>');
WriteLn('<title>AKFQuiz</title>');
WriteLn('<meta name="generator" content="'+AKFQuizName + ' ' +
          AKFQuizVersion+'">'); { change-xhtml }
{ the next instruction is also in the HTTP header }
WriteLn('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
{ the next instruction is also in the HTTP header }
{ WriteLn('<meta http-equiv="Cache-control" content="no-cache">'); }
WriteLn;
WriteLn('<style type="text/css">');
WriteLn('<!--');
WriteLn('body { color:black; background-color:#d8d0c8; margin:1ex 8%; }');
WriteLn('h1 { color:#ffffdd; background-color:#605030; padding:12px;');
WriteLn('     border:12px ridge; border-color:#605030; margin:1em 15%;');
WriteLn('     text-align:center; font-weight:bold; }');
WriteLn('.error { color:red; background-color:transparent;');
WriteLn('         font-weight:bold; font-style:italic;}');
WriteLn('-->');
WriteLn('</style>');
WriteLn('</head>');
WriteLn;
WriteLn('<body>');
WriteLn;
WriteLn('<div align="center">');
WriteLn('<h1>AKFQuiz</h1>');
WriteLn;
found := ListEntries(CGI_PATH_TRANSLATED, quizext, myshowentry);
if ListEntries(CGI_PATH_TRANSLATED, quizext2, myshowentry) 
   then found := true;
if not found then NoEntriesFound;
WriteLn('</div>');
WriteLn('</body>');
WriteLn('</html>')
end;

procedure getQueryString;
var i, len, code: word;
begin
CGI_QUERY_STRING := NIL;
QUERY_STRING_POS := 0;

if GetEnvironmentVariable('REQUEST_METHOD')='GET'
  then begin
       if GetEnvironmentVariable('QUERY_STRING')<>'' 
         then begin
              {$IfDef __GPC__}
                { CString can be longer than TString }
                new(CGI_QUERY_STRING, 
                       CStringLength(CStringGetEnv('QUERY_STRING')));
                CGI_QUERY_STRING^ := 
                   CString2String(CStringGetEnv('QUERY_STRING'))
              {$Else}
                new(CGI_QUERY_STRING);
                CGI_QUERY_STRING^ := GetEnvironmentVariable('QUERY_STRING')
              {$EndIf}
              end
       end
  else begin
       if GetEnvironmentVariable('REQUEST_METHOD')='POST'
         then begin
              val(GetEnvironmentVariable('CONTENT_LENGTH'), len, code);
	      if (len>0) and (code=0) then
	        begin
                new(CGI_QUERY_STRING {$IfDef __GPC__} ,len {$EndIf});
                SetLength(CGI_QUERY_STRING^, len);
                for i := 1 to len do
                  Read(CGI_QUERY_STRING^[i])
		end
              end
       end
end;

procedure runQuiz;
var MyQuiz: Pakfquiz;
begin
getQueryString;

if CGI_QUERY_STRING=NIL
   then MyQuiz := new(Pcgiquiz, init)    { query }
   else MyQuiz := new(Pcgianswer, init); { show answer }

if (IOResult<>0) or (MyQuiz=NIL) then NotFound;
MyQuiz^.process;
dispose(MyQuiz, Done)
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
    if (p='-H') or (p='--HELP') or (p='/?') then help;
    if (p='--VERSION') then version;
    end
end;


begin
parameters;
useBrowserLanguage;

CGI_PATH_INFO       := GetEnvironmentVariable('PATH_INFO');
CGI_PATH_TRANSLATED := GetEnvironmentVariable('PATH_TRANSLATED');

if (CGI_PATH_INFO='') or (CGI_PATH_TRANSLATED='') then help;

if (pos('/--help', CGI_PATH_INFO)<>0) 
   or (pos('/-h', CGI_PATH_INFO)<>0) 
   or (pos('/?', CGI_PATH_INFO)<>0)  
    then help;

if pos('/--version', CGI_PATH_INFO)<>0 then version;

{ Don't allow to use /.. for security reasons }
{ else someone could scan through the whole machine }
if pos('/..', CGI_PATH_INFO)<>0 then Forbidden;

if (pos('/--exam/', CGI_PATH_INFO)=1) then prepareExam(length('/--exam'));
if (pos('/-exam/', CGI_PATH_INFO)=1)  then prepareExam(length('/-exam'));

if isPath 
  then showList
  else runQuiz
end.
