{
* cgiquiz (aka "akfquiz.cgi")
* CGI/1.1 runtime for a quiz from a simplified input script
*
* Needs a CGI/1.1 compatible web-server (boa, apache, ...)
* Needs the files "falsch.png", "richtig.png",
* and optionally a given CSS file in the same directory with 
* the input file or in a directory set by "baseURI:"
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

  {$IfDef Win32}
    {$AppType Console}
  {$EndIf}
{$EndIf}

{$I-}
{$X+}


program cgiquiz(input, output); { aka "akfquiz.cgi" }

{$IfDef __GPC__}
  import uakfquiz; htmlquiz; qmsgs; qsys;
         GPC (GetEnv => GetEnvironmentVariable); 
{$Else}
    uses SysUtils, qsys, qmsgs, uakfquiz, htmlquiz;
{$EndIf} { __GPC__ }


const PrgLine = AKFQuizName + ' CGI-Version: ' + AKFQuizVersion;

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
	CGIelement : mystring;
	Home       : mystring;
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


var 
  CGI_PATH_INFO,
  CGI_PATH_TRANSLATED: myString;

{ gets data from CGI method POST }
procedure GetCGIelement(var s: myString); {@@}
var 
  i,err: integer;
  c, c1: char;
  next: boolean;
begin
s := '';
if eof then exit;

repeat
  Read(c);
  next := (c='&'); { an encoded '&' doesn't hurt here yet }
  if c='+' then c := ' ';
  if c='%' then { hexadecimal encoded }
    begin
    Read(c, c1); { read 2 hex-chars }
    val('$'+c+c1, i, err); { convert them to an integer }
    c := chr(i) { and that integer to a char }
    end;
  if not next then s := s + c
until next or eof
end;

procedure showHelp;
var Browser: boolean;
begin
{ is this called from a Browser? }
Browser := (GetEnvironmentVariable('REQUEST_METHOD')<>'');

if Browser then { send CGI Header }
  begin
  WriteLn('Content-Type: text/html; charset=UTF-8');
  WriteLn;
  WriteLn(HTMLDocType);
  WriteLn;
  WriteLn('<html>');
  WriteLn('<head>');
  WriteLn('<title>' + AKFQuizName + ': Status</title>');
  WriteLn('</head>');
  WriteLn;
  WriteLn('<body>');
  WriteLn('<h1>', AKFQuizName, '</h1>')
  end;

{ the text }
WriteLn;
WriteLn(PrgLine);
If Browser then WriteLn('<br>');
Write('Copyright');
If Browser then Write(' &copy; ') else Write(' (C) '); 
WriteLn(AKFQuizCopyright);
If Browser then WriteLn('<br>');
WriteLn(msg_License, msg_GPL);
WriteLn;
If Browser then WriteLn('<br><br>');

WriteLn('Syntax:');
If Browser 
  then begin
       WriteLn('<br>');
       Write  ('<strong>');
       Write  ('http://',
               GetEnvironmentVariable('HTTP_HOST'),
               GetEnvironmentVariable('SCRIPT_NAME'),
	       '</strong>');
       WriteLn('/quizpath/');
       WriteLn('<br>');
       Write  ('<strong>');
       Write  ('http://',
               GetEnvironmentVariable('HTTP_HOST'),
               GetEnvironmentVariable('SCRIPT_NAME'),
	       '</strong>');
       WriteLn('/quizpath/myquiz.akfquiz');
       WriteLn('</body></html>')
       end
  else begin
       WriteLn('   http://example.org/cgi-bin/cgiquiz/quizpath/');
       WriteLn('   http://example.org/cgi-bin/cgiquiz/quizpath/myquiz.akfquiz')
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
WriteLn('<hr>', PrgLine);
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
  	        GetEnvironmentVariable('HTTP_REFERER'), '"', cet)
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

GetCGIelement(CGIelement);

if Pos('home=', CGIelement)=1 then
  begin
  Home := CGIelement;
  Delete(Home, 1, length('home='));
  GetCGIelement(CGIelement)
  end
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

{ conflicts with base }
{
WriteLn(outp, '<small>');
WriteLn(outp, '<div align="center" class="resultlink">');
WriteLn(outp, '<a href="#result">', msg_result, '</a>');
WriteLn(outp, '</div>');
}

if Home<>'' then
  begin
  WriteLn(outp, '<div align="center" class="home">');
  WriteLn(outp, '<a href="', Home, '">', msg_back, '</a>');
  WriteLn(outp, '</div>')
  end;
WriteLn(outp, '</small>')
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
if CGIelement = 'q'+IntToStr(questionNr)+'='+IntToStr(answerNr) then 
   begin
   inc(Points, value);
   inc(AnsPoints, value);
   Write(outp, ' checked'); { change-xhtml }
   GetCGIelement(CGIelement)
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
if CGIelement = 'q'+IntToStr(questionNr)+'='+IntToStr(answerNr) then 
   begin
   Write(outp, ' checked'); { change-xhtml }
   GetCGIelement(CGIelement)
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
  WriteLn(outp, '<div align="center" class="home">');
  WriteLn(outp, '<a href="', Home, '">', msg_back, '</a>');
  WriteLn(outp, '</div>')
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
{ WriteLn('<meta http-equiv="Cache-control" content="no-cache">'); }
WriteLn;
WriteLn('<style type="text/css">');
WriteLn('<!--');
WriteLn('body { color:black; background:#d8d0c8; margin:1ex 8%; }');
WriteLn('h1 { color:#ffffdd; background:#605030; padding:12px;');
WriteLn('     border:12px ridge; border-color:#605030; margin:1em 15%;');
WriteLn('     text-align:center; font-weight:bold; }');
WriteLn('.error { color:red; background:transparent;');
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

procedure runQuiz;
var MyQuiz: Pakfquiz;
begin

{ Method POST must be used 
  (else CONTENT_LENGTH is undefined anyway) }
if GetEnvironmentVariable('CONTENT_LENGTH')='' 
   then MyQuiz := new(Pcgiquiz, init)    { query }
   else MyQuiz := new(Pcgianswer, init); { show answer }

if (IOResult<>0) or (MyQuiz=NIL) then NotFound;
MyQuiz^.process;
dispose(MyQuiz, Done)
end;

begin
useBrowserLanguage;

CGI_PATH_INFO       := GetEnvironmentVariable('PATH_INFO');
CGI_PATH_TRANSLATED := GetEnvironmentVariable('PATH_TRANSLATED');

if (CGI_PATH_INFO='') or (CGI_PATH_TRANSLATED='')
  then showHelp;

{ Don't allow to use /.. for security reasons }
{ else someone could scan through the whole machine }
if Pos('/..', CGI_PATH_INFO)<>0 then Forbidden;

if isPath 
  then showList
  else runQuiz
end.
