{
* cgiquiz
* CGI/1.1 runtime for a quiz from a simplified input script
*
* Needs a CGI/1.1 compatible web-server (boa, apache, ...)
*
* $Id: cgiquiz.pas,v 1.46 2006/10/19 05:33:39 akf Exp $
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
  {$R w32/cgiquiz.res}
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


program cgiquiz(input, output);

{$IfDef __GPC__}
  import uakfquiz; htmlquiz; qmsgs; qsys; styles; GPC; 
{$Else}
  uses SysUtils, qsys, qmsgs, uakfquiz, htmlquiz, styles;
{$EndIf} { __GPC__ }

{ GNU compliant format }
const PrgVersion = 'cgiquiz ('+ AKFQuizName + ') ' + AKFQuizVersion;

const ExamModeName = 'exam'; { abstract name for the URI }

const AKFQuizMime = 'application/x-akfquiz';

{ for self-referring URIs }
const protocol = 'http://';

{ for the exam-mode }
const ResultExt = '.result';
const examConfigFileName = '.config';

{ just used internally, so you might change the names }
{ for standard-compliant browsers the extension needn't be given, 
  but for IE }
const
  grRight = '/richtig.png';
  grFalse = '/falsch.png';
  grIcon  = '/akfquiz-icon.png';

{$I pngdata.inc}

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
        procedure headBaseURI;
        procedure headdata;                      virtual;
	function  GeneratorName: mystring;       virtual;
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
	CGIElement : ShortString; { just one Element }
	Home       : mystring;
	save       : boolean;
        Name       : string255;
	AnsPoints  : pointsType;
	oldPercent : pointsType; { for sanity-check }

      public
        constructor init;
        procedure headdata;                      virtual;
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
	procedure saveResult;
        procedure EndQuiz;                       virtual;
    end;

{$IfDef FPC}
  type FormDataString = AnsiString;
{$Else}
  type FormDataString = String(10240);
{$EndIf}

type TRequestMethod = (METHOD_UNKNOWN, HEAD, GET, POST);

var 
  ExamMode: boolean = false;
  RequestMethod : TRequestMethod = METHOD_UNKNOWN;

var 
  CGI_PATH_INFO,
  CGI_PATH_TRANSLATED: myString;
  
{ HTTP_HOST or SERVER_NAME or SERVER_ADDR }
var ServerName: ShortString;
var ScriptName: myString;

{ for exam mode }
var Cookie : ShortString = '';
var passwd : ShortString = '';

var 
  CGI_QUERY_STRING: FormDataString;
  QUERY_STRING_POS: LongInt;

procedure HTTPStatus(status: integer; message: string);
begin
{ CGI-Style: }
if status <> 200 then
  WriteLn('Status: ', status, ' "', message, '"');

{ HTTP-Style: (nph-mode, server-mode) }
{ WriteLn('HTTP/1.0 ', status, ' ', message); }

{ additional header data for a standalone server: }
{ WriteLn('Server: ', PrgVersion);
  WriteLn('Connection: close');
}
end;

procedure closeHttpHead;
begin
WriteLn;

{ some webservers don't like CGI programs to do that }
{ if RequestMethod = HEAD then Halt }
end;

function isLastElement: boolean;
begin
if CGI_QUERY_STRING=''
  then isLastElement := true
  else 
   if QUERY_STRING_POS < Length(CGI_QUERY_STRING)
     then isLastElement := false
     else isLastElement := true
end;

procedure GetCGIElement(var s: ShortString);
var 
  i,ignore: integer;
  c, c1: char;
  nextElement: boolean;

  function getNextChar: char;
  begin
  inc(QUERY_STRING_POS);
  getNextChar := CGI_QUERY_STRING[QUERY_STRING_POS]
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
    val('$'+c+c1, i, ignore); { convert them to an integer }
    c := chr(i) { and that integer to a char }
    end;
  if not nextElement then s := s + c
until nextElement or isLastElement
end;

function CGIfield(const CGIElement: ShortString): ShortString;
var p : integer;
begin
p := pos('=', CGIElement);
if p = 0 
  then CGIfield := ''
  else CGIfield := copy(CGIElement, 1, p-1)
end;

function CGIvalue(const CGIElement: ShortString): ShortString;
var p : integer;
begin
p := pos('=', CGIElement);
if p = 0 
  then CGIvalue := ''
  else CGIvalue := copy(CGIElement, p+1, length(CGIElement))
end;

function QueryLookup(item: ShortString): ShortString;
var 
  p: integer;
  s: FormDataString;
begin
s := '';

p := pos(item + '=', CGI_QUERY_STRING);
if p <> 0 then { found }
  begin
  s := CGI_QUERY_STRING;
  Delete(s, 1, p + length(item));
  
  p := pos('&', s);
  if p <> 0 then { not last element }
    s := copy(s, 1, p-1)
  end;

QueryLookup := s
end;

function CGIInfo(const s: string): mystring;
begin
{$IfDef FPC}
  CGIInfo := GetEnvironmentVariable(s)
{$Else}
  CGIInfo := GetEnv(s)
{$EndIf}
end;

{ get the ServerName with fallbacks }
procedure getServerName;
begin
{ my version of "boa" doesn't set SERVER_NAME }
ServerName := CGIInfo('SERVER_NAME');
if ServerName = '' then ServerName := CGIInfo('HTTP_HOST');
if ServerName = '' then ServerName := CGIInfo('SERVER_ADDR')
end;

function RemoteAddr: shortstring;
var s: shortstring;
begin
s := CGIInfo('REMOTE_HOST'); { often not available }
if s = '' then s := CGIInfo('REMOTE_ADDR');
RemoteAddr := s
end;

procedure version;
var Browser: boolean;
begin
{ is this called from a Browser? }
Browser := (CGIInfo('REQUEST_METHOD')<>'');

if not Browser then 
  begin
  useSystemLanguage;
  setmsgconv(checkDisplay)
  end;

if Browser then { send HTTP Header }
  begin
  HTTPStatus(200, 'OK');
  WriteLn('Content-Type: text/html; charset=UTF-8');
  closeHttpHead;
  
  WriteLn(HTMLDocType);
  WriteLn;
  WriteLn('<html>');
  WriteLn('<head>');
  WriteLn('<title>' + AKFQuizName + ': version</title>');
  WriteLn('<meta name="robots" content="noindex">');
  WriteLn('<meta name="generator" content="'
           + PrgVersion + '">'); { change-xhtml }
  WriteLn('<link rel="bookmark" title="Software Homepage" href="', 
              msg_homepage, '">');
  WriteLn('<link rel="icon" type="image/png" href="', 
              ScriptName, grIcon + '">');
  WriteLn('<link rel="stylesheet" type="text/css" href="', 
           ScriptName, '/q-brown.css">');
  WriteLn('</head>');
  WriteLn;
  WriteLn('<body>');
  WriteLn('<h1>', AKFQuizName, '</h1>');
  WriteLn;
  WriteLn('<img alt="[Icon]" width="32" height="32" src="',
    ScriptName, grIcon + '">');
  end;

{ the text }
if Browser 
  then WriteLn('<a href="', ScriptName, '?--help">', PrgVersion, '</a>')
  else WriteLn(PrgVersion);
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
WriteLn;
WriteLn(msg_contributions);
WriteLn(Contributors);
If Browser then WriteLn('</pre></body></html>');
Halt
end;


procedure help;
var 
  Browser: boolean;
  CGIBase: mystring;
begin
{ is this called from a Browser? }
Browser := (CGIInfo('REQUEST_METHOD')<>'');

if Browser
  then begin
       if ServerName='' then getServerName;
       CGIBase := '<strong>'
                  + protocol
                  + ServerName
                  + CGIInfo('SCRIPT_NAME')
	          + '</strong>'
       end
  else CGIBase := 'http://example.org/cgi-bin/cgiquiz';

if Browser then { send HTTP Header }
  begin
  HTTPStatus(200, 'OK');
  WriteLn('Content-Type: text/html; charset=UTF-8');
  closeHttpHead;

  
  WriteLn(HTMLDocType);
  WriteLn;
  WriteLn('<html>');
  WriteLn('<head>');
  WriteLn('<title>' + AKFQuizName + ': help</title>');
  WriteLn('<meta name="robots" content="noindex">');
  WriteLn('<meta name="generator" content="'
            + PrgVersion + '">'); { change-xhtml }
  WriteLn('<link rel="bookmark" title="Software Homepage" href="', 
             msg_homepage, '">');
  WriteLn('<link rel="icon" type="image/png" href="', 
              ScriptName, grIcon + '">');
  WriteLn('<link rel="stylesheet" type="text/css" href="', 
           ScriptName, '/q-brown.css">');
  WriteLn('</head>');
  WriteLn;
  WriteLn('<body>');
  WriteLn('<h1>', AKFQuizName, '</h1>');
  WriteLn
  end;

{ the text }

if Browser 
  then begin
       WriteLn('<img alt="[Icon]" width="32" height="32" src="',
             ScriptName, grIcon + '">');
       WriteLn('<a href="', ScriptName, '?--version">', 
               PrgVersion, '</a>');
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
       WriteLn(CGIBase, '?--help<br>');
       WriteLn(CGIBase, '?--version<br>');
       WriteLn(CGIBase, '/quizdir/<br>');
       WriteLn(CGIBase, '/quizdir/myquiz.akfquiz');
       WriteLn(CGIBase, '/quizdir/myquiz.akfquiz?format=akfquiz');
       WriteLn(CGIBase, '/quizdir/myquiz.akfquiz?format=text');
       WriteLn(CGIBase, '/quizdir/myquiz.akfquiz?q1=2&amp;q2=1&amp;q5=2');
       WriteLn('</dd></dl>');
       WriteLn;
       if ExamDir<>'' 
         then WriteLn('<p>The special quizdir "', ExamModeName, '" for exams'
                      + ' is mapped to "', ExamDir, '".</p>')
	 else WriteLn('<p>Exam mode disabled.</p>');
       WriteLn;
       WriteLn('<p>The following layouts are included in the binary:<br>');
       WriteLn('"q-school.css", "q-brown.css", "q-blue.css"</p>');
       if BugMail<>'' then
         WriteLn('<p>Report bugs to <a href="mailto:' + BugMail + '">'
                 + BugMail+'</a>.</p>');
       WriteLn('</body></html>')
       end
  else begin { not Browser }
       WriteLn('Quiz-program for the CGI interface of a webserver');
       WriteLn;
       WriteLn('Usage: cgiquiz [ --help | -h | /? ]');
       WriteLn(' or:   cgiquiz --version');
       WriteLn;
       WriteLn('Examples:');
       WriteLn(' > ', CGIBase, '?--help');
       WriteLn(' > ', CGIBase, '?--version');
       WriteLn(' > ', CGIBase, '/quizdir/');
       WriteLn(' > ', CGIBase, '/quizdir/myquiz.akfquiz');
       WriteLn(' > ', CGIBase, '/quizdir/myquiz.akfquiz?format=akfquiz');
       WriteLn(' > ', CGIBase, '/quizdir/myquiz.akfquiz?format=text');
       WriteLn(' > ', CGIBase, '/quizdir/myquiz.akfquiz?q1=2&q2=1&q5=2');
       WriteLn;
       if ExamDir<>'' 
         then WriteLn('The special quizdir "', ExamModeName, '" for exams'
                      + ' is mapped to "', ExamDir, '".')
	 else WriteLn('Exam mode disabled.');
       WriteLn;
       WriteLn('The following layouts are included in the binary:');
       WriteLn('"q-school.css", "q-brown.css", "q-blue.css"');
       
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
s := copy(CGIInfo('HTTP_ACCEPT_LANGUAGE'), 1, 2);

if s='de' then lang := deutsch
  else if s='it' then lang := italiano
    else if s='da' then lang := dansk
      else if s='en' then lang := english
        else begin 
             { first language unknown - check for others: }
             s := CGIInfo('HTTP_ACCEPT_LANGUAGE');
  	     if pos('de', s)<>0 then lang := deutsch
	       else if pos('it', s)<>0 then lang := italiano
	         else if pos('da', s)<>0 then lang := dansk
	  	   else lang := english
             end
end;

{ --------------------------------------------------------------------- }
{ Error handling procedures }

procedure errorHTMLhead(status: integer; message: string);
begin
HTTPStatus(status, message);
WriteLn('Content-Type: text/html; charset=UTF-8');
closeHttpHead;

WriteLn(HTMLDocType);
WriteLn;
WriteLn('<html>');
WriteLn('<head>');
WriteLn('<title>', AKFQuizName, ': ', msg_error, ' ', message, '</title>');
WriteLn('<meta name="generator" content="'
         + PrgVersion + '">'); { change-xhtml }
WriteLn('<link rel="bookmark" title="Software Homepage" href="', 
              msg_homepage, '">');
WriteLn('<link rel="icon" type="image/png" href="', 
            ScriptName, grIcon + '">');
WriteLn('<meta name="robots" content="noindex">');
WriteLn('</head>');
WriteLn;
WriteLn('<body style="background-color:#a00; color:white">');
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
Halt
end;

procedure CannotOpen;
begin
errorHTMLhead(404, 'cannot open file');
case lang of 
  deutsch : WriteLn('Fehler: kann "',CGI_PATH_INFO,'" nicht &ouml;ffnen');
  otherwise WriteLn('Error: cannot open "', CGI_PATH_INFO, '"')
  end;
errorHTMLfoot;
Halt
end;

procedure SetupError;
begin
errorHTMLhead(409, 'setup error');
case lang of 
  deutsch : WriteLn('Fehler: ', AKFQuizName, ' nicht richtig eingerichtet');
  otherwise WriteLn('Error: ', AKFQuizName, ' not set up correctly')
  end;
errorHTMLfoot;
Halt
end;

procedure Forbidden;
begin
errorHTMLhead(403, 'request forbidden');
case lang of
  deutsch : WriteLn('Fehler: keine Berechtigung');
  otherwise WriteLn('Error: no permission')
  end;
errorHTMLfoot;
Halt
end;

procedure FileError;
begin
errorHTMLhead(403, 'Error in file');
case lang of 
  deutsch : WriteLn('Fehler in Datei "',CGI_PATH_INFO, '"');
  otherwise WriteLn('Error in file "', CGI_PATH_INFO, '"')
  end;
errorHTMLfoot;
Halt
end;

{ used when the user ommits a trailing slash, when it is needed }
procedure MovedPermanently(const Location: myString);
begin
HTTPStatus(301, 'Moved Permanently');
WriteLn('Location: ', Location);
WriteLn('Content-Type: text/html; charset=UTF-8');
closeHttpHead;

WriteLn(HTMLDocType);
WriteLn;
WriteLn('<html>');
WriteLn('<head>');
WriteLn('<title>', AKFQuizName, ': Moved Permanently</title>');
WriteLn('<meta name="generator" content="'
         + PrgVersion + '">'); { change-xhtml }
WriteLn('<meta name="robots" content="noindex">');
WriteLn('<link rel="bookmark" title="Sofware Homepage" href="', 
                msg_homepage, '">');
WriteLn('</head>');
WriteLn;
WriteLn('<body>');
WriteLn('<h1>Moved Permanently</h1>');
WriteLn;
WriteLn('<p>Please use the address <a href="', Location, '">',
        Location, '</a> in the future.');
ErrorHTMLfoot;
Halt
end;

procedure RejectAnswer(s: string);
begin
HTTPStatus(204, s);
{WriteLn('Content-Type: text/plain');} { needed? }
closeHttpHead;
Halt
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
HTTPStatus(200, 'OK');
WriteLn(outp, 'Content-Type: text/html; charset=', charset);
if language<>'' then
  WriteLn(outp, 'Content-Language: ', language);
WriteLn(outp);

{ some webservers don't like CGI programs to do that }
{ if RequestMethod = HEAD then Halt }
end;

procedure Tcgiquiz.error;
begin
if started 
  then inherited error
  else FileError
end;

function Tcgiquiz.GeneratorName: mystring;
begin
GeneratorName := PrgVersion
end;

procedure Tcgiquiz.headBaseURI;
begin
WriteLn(outp);
if pos(protocol, DocumentURI)=1
  then WriteLn(outp, '<base href="', DocumentURI, '">')
  else WriteLn(outp, '<base href="', protocol, ServerName, DocumentURI, '">');
WriteLn(outp);
end;

{ Tcgianswer doesn't automatically inherit this! }
procedure Tcgiquiz.headdata;
begin
headBaseURI;

inherited headdata;

WriteLn(outp, '<link rel="icon" type="image/png" href="', 
                ScriptName, grIcon + '"', cet);

{ prefetch is a Mozilla specific feature, but it doesn't interfere with 
  the official HTML-standards }
WriteLn(outp, '<link rel="prefetch" href="', ScriptName, grRight + '"', cet);
WriteLn(outp, '<link rel="prefetch" href="', ScriptName, grFalse + '"', cet)
end;

procedure Tcgiquiz.StartForm;
begin
WriteLn(outp);

if not ExamMode 
  then WriteLn(outp,
          '<form name="akfquiz" id="akfquiz" method="GET" action="',
          ScriptName, CGI_PATH_INFO, '">')
  else begin { ExamMode }
       { check if name-field is filled out }
       WriteLn(outp, 
         '<!-- JavaScript is not necessarily needed for this program -->');
       WriteLn(outp, '<script type="text/javascript" language="JavaScript">');
       WriteLn(outp, '<!--');
       WriteLn(outp, 'function checkName() {');
       WriteLn(outp, '  if (document.akfquiz.name.value == "") {');
       WriteLn(outp, '    document.akfquiz.name.focus();');
       WriteLn(outp, '    return false; }');
       WriteLn(outp, '  else return true; }');
       WriteLn(outp, '// -->');
       WriteLn(outp, '</script>');
       WriteLn(outp);
       WriteLn(outp,
          '<form name="akfquiz" id="akfquiz" method="POST" action="',
          ScriptName, CGI_PATH_INFO, 
	  '" onSubmit="return checkName()">')
       end;

WriteLn(outp);

{ remember the origin: }
if CGIInfo('HTTP_REFERER')<>'' then
  WriteLn(outp, '<input type="hidden" name="home" value="', 
  	        CGIInfo('HTTP_REFERER'), '"', cet);

if ExamMode then
  begin
  WriteLn(outp, '<div class="name"><label for="name">', msg_name, 
    '</label>'
    + '<input type="text" name="name" id="name" size="50" maxlength="100">'
    + '</div>')
  end
end;

procedure Tcgiquiz.StartQuiz;
begin
if BaseURI <> '' then DocumentURI := BaseURI;
if (CSS <> '') and (pos('/', CSS) = 0) then 
  CSS := ScriptName + '/' + CSS;

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

if not ExamMode 
  then WriteLn(outp, '<input type="reset" accesskey="n" value="',
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
oldPercent := -1;

{ if ExamMode and RequestMethod=POST then save else don't }
save := (ExamMode and (RequestMethod=POST));

repeat
  GetCGIelement(CGIElement);

  if CGIfield(CGIElement) = 'home' then Home := CGIvalue(CGIElement);
  if CGIfield(CGIElement) = 'percent' then 
    oldPercent := StrToInt(CGIvalue(CGIElement), -1);
  if CGIfield(CGIElement) = 'name' then Name := CGIvalue(CGIElement);
until (CGIElement[1]='q') or isLastElement
end;

procedure Tcgianswer.headdata;
begin
headBaseURI;

{ avoid inheriting the special data introduced in Tcgiquiz }
Thtmlquiz.headdata;

WriteLn(outp, '<link rel="icon" type="image/png" href="', 
                ScriptName, grIcon + '"', cet)
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
  WriteLn(outp, '<div class="name">', msg_name, Name, '</div>')
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
   then WriteLn(outp, '  alt="- ', msg_right, 
                      ' -" src="', ScriptName + grRight+'"', cet)
   else WriteLn(outp, ' alt="- ', msg_wrong, 
                      ' -" src="', ScriptName + grFalse+'"', cet);
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
if MaxPoints > 0 then
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

  { sanity-check with oldPercent }
  if (oldPercent >= 0) and (getPercentage <> oldPercent) then
    WriteLn(outp, '<p class="error">', msg_inconsistent, '</p>');

  WriteLn(outp, '</strong></div>');
  end;

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

procedure Tcgianswer.saveResult;
var 
  f: text;
  i: integer;
  myname,
  FileName : mystring;
  ResultStr: FormDataString;
begin

{ prepare output line to make write access as atomic as possible }
if Name<>'' 
  then myname := csToUTF8(charset, Name)
  else myname := 'anonymous'; { shouldn't happen }

myname := myname + ' (' + RemoteAddr + ')';

FileName := CGI_PATH_INFO;
Delete(FileName, 1, length('/' + ExamModeName + '/'));

{ ResultStr := CGI_QUERY_STRING, starting from name= }
ResultStr := CGI_QUERY_STRING;
i := pos('name=', ResultStr);
if i <> 0 then Delete(ResultStr, 1, pred(i));
{ add percent= for sanity-check }
ResultStr := 'percent=' + IntToStr(getPercentage) + '&' + ResultStr;

{ collect the all data }
ResultStr := showDateTime + nl + myname + nl + IntToStr(getPercentage) 
                 + nl + FileName + '?' + ResultStr + nl + nl;

Assign(f, stripext(CGI_PATH_TRANSLATED) + ResultExt);

{$IfDef FPC}
  Append(f);
  if IOResult<>0 then Rewrite(f);
{$Else}
  Extend(f); { ISO-10206 }
{$EndIf}

{ keep write-process as atomic as possible }
Write(f, ResultStr);
Close(f);

{ we are still in the body - at the end of the body }
if IOResult<>0 then 
  WriteLn(outp, '<p class="error">', msg_error, '</p>')
end;

procedure Tcgianswer.EndQuiz;
begin
if save then saveResult;
inherited EndQuiz
end;

{ --------------------------------------------------------------------- }

function isDirectory: boolean;
begin
{ It is a Directory, when there is a trailing slash. }
{ When there is no trailing slash, but it is nethertheless a Directory,
  then the browser must be redirected. The trailing slash is really 
  needed for relative addresses to function. }

{ Directory-separator not system-specific here }
if CGI_PATH_INFO[length(CGI_PATH_INFO)] = '/'
  then isDirectory := true
  else begin
       isDirectory := false;
       if DirectoryExists(CGI_PATH_TRANSLATED) 
         then MovedPermanently(protocol + ServerName + ScriptName
	                       + CGI_PATH_INFO + '/')
       end
end;

{ is a Result file requested? }
function isResultFile: boolean;
begin
isResultFile := (pos(ResultExt, CGI_PATH_INFO)<>0)
end;

procedure CommonHtmlStart(const title: string);
begin
WriteLn('Content-Type: text/html; charset=UTF-8');

{ Don't cache these pages
  different program react on different of these settings }
WriteLn('Cache-control: no-cache');
WriteLn('Pragma: no-cache');
WriteLn('Expires: 0');
closeHttpHead;

WriteLn(HTMLDocType);
WriteLn;
WriteLn('<html>');
WriteLn('<head>');
WriteLn('<title>', title, '</title>');
WriteLn('<meta name="generator" content="'
         + PrgVersion + '">'); { change-xhtml }
{ the next instruction is also in the HTTP header }
WriteLn('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
WriteLn('<meta name="robots" content="noindex">');
WriteLn;
WriteLn('<link rel="bookmark" title="Software Homepage" href="', 
             msg_homepage, '">');
WriteLn('<link rel="icon" type="image/png" href="', 
              ScriptName, grIcon + '">');
WriteLn('<link rel="stylesheet" type="text/css" href="', 
           ScriptName, '/q-brown.css">');
WriteLn('</head>');
WriteLn;
WriteLn('<body>');
WriteLn;
WriteLn('<h1 align="center">', title, '</h1>');
WriteLn
end;

procedure CommonHtmlEnd;
begin
WriteLn;
WriteLn('<hr><div align="right" class="made"><a href="', msg_homepage, 
        '" target="_top">' + AKFQuizName + '</a></div>');
WriteLn('</body>');
WriteLn('</html>')
end;

function loggedIn: boolean;
begin
loggedIn := 
  (passwd <> '') and 
  (pos('passwd="' + passwd + '"', Cookie) <> 0)
end;

procedure RequireAuthorization;
begin
if not loggedIn then Forbidden
end;

procedure NoEntriesFound;
begin
WriteLn('<p align="center" class="error">');
WriteLn(msg_noquizfound);
WriteLn('</p>')
end;

procedure ListShowEntry(const dir, s: string);
begin
WriteLn('<li>');
WriteLn('<a href="', s, '">',
        getQuizTitle(CGI_PATH_TRANSLATED + s), '</a>');

if not ExamMode then
  begin
  Write('<small>(<a href="', s, 
        '?format=akfquiz" type="' + AKFQuizMime + '">AKFQuiz</a>');
  Write('&nbsp;|&nbsp;');
  WriteLn('<a href="', s, '?format=text" type="text/plain">',
           msg_view, '</a>)</small>')
  end;

WriteLn('</li>');
WriteLn
end;

procedure showList;
var found: boolean;
begin
HTTPStatus(200, 'OK');
CommonHtmlStart(AKFQuizName);
WriteLn('<ul>');
found := ListEntries(CGI_PATH_TRANSLATED, quizext, ListShowEntry);
if ListEntries(CGI_PATH_TRANSLATED, quizext2, ListShowEntry) 
   then found := true;
WriteLn('</ul>');
if not found then NoEntriesFound;
if ExamMode then
  if loggedIn 
    then begin
         WriteLn('<hr><ul>');
	 WriteLn('<li><a href="results">', msg_showResults, '</a></li>');
	 WriteLn('<li><a href="logout">', msg_logout, '</a></li>');
	 WriteLn('</ul>')
	 end
    else WriteLn('<hr><ul><li><a href="login">', msg_login, '</a></li></ul>');
CommonHtmlEnd;
Halt
end;

procedure unacceptableNewPasswd;
begin
HTTPStatus(200, 'OK');
CommonHtmlStart('unacceptable new Password');
WriteLn('<p>Sorry, but you may only use letters of the latin alphabet or '
        + 'numbers in a password.</p>');
	
{ when a password is set: reconfigure
  when no password is set, the target of the link is ignored anyway }
WriteLn('<p><a href="reconfigure">', msg_back, '</a></p>');
CommonHtmlEnd;
Halt
end;

procedure checkNewPasswd;
const passwdChars = ['A' .. 'Z', 'a' .. 'z', '0', '1' .. '9'];
var i: integer;
begin
for i := 1 to length(passwd) do
  if not (passwd[i] in passwdChars)
    then unacceptableNewPasswd
end;

procedure saveExamConfig;
var 
  f: text;
  CGIElement: ShortString;
begin

{ for security reasons only the POST method is allowed here }
if RequestMethod <> POST then Forbidden;

{ a new passwd is only acceptable, when there is none yet
  or the user is correctly authorized with the old passwd }
if passwd <> '' then if not loggedIn then Forbidden;

{ always get the new passwd from the query }
repeat
  GetCGIelement(CGIElement);
  if CGIfield(CGIElement) = 'passwd' then passwd := CGIvalue(CGIElement)
until isLastElement;

checkNewPasswd;

Assign(f, useDirSeparator(ExamDir) + examConfigFileName);
Rewrite(f);
WriteLn(f, passwd);
close(f);
if IOResult<>0 then SetupError;

HTTPStatus(200, 'OK');
{ Session-Cookie, deleted when browser is closed }
WriteLn('Set-Cookie: passwd="' + passwd + '"; Discard; Version="1";');
CommonHtmlStart(AKFQuizName + ': Configuration saved');
WriteLn('<p>Configuration saved</p>');
WriteLn('<p><a href="results">', msg_showResults, '</a></p>');
CommonHtmlEnd;
Halt
end;

procedure configureExamMode;
begin
HTTPStatus(200, 'OK');
CommonHtmlStart(AKFQuizName + ': Configuration');
WriteLn('<form method="POST" action="saveconfig">');
WriteLn('<div>');
Write(msg_newpasswd, ': ');
WriteLn('<input type="password" name="passwd" value="', passwd,
        '" size="12" maxlength="12">');
WriteLn('<br>');

WriteLn('<input type="submit"><input type="reset">');
WriteLn('</div>');
WriteLn('</form>');
CommonHtmlEnd;
Halt
end;

procedure reconfigureExamMode;
begin
RequireAuthorization;
configureExamMode
end;

procedure readExamConfig;
var f: text;
begin
if not ExamMode then exit;

Assign(f, useDirSeparator(ExamDir) + examConfigFileName);
Reset(f);
ReadLn(f, passwd);
close(f);

if IOResult<>0 then passwd := ''
end;

procedure Login;
begin
HTTPStatus(200, 'OK');
CommonHtmlStart(AKFQuizName + ': ' + msg_passwd);
WriteLn('<form method="POST" action="login2">');
WriteLn('<div>');
WriteLn(msg_passwd, ': ');
WriteLn('<input type="password" name="passwd" '
        + 'size="12" maxlength="12">');
WriteLn('<input type="submit">');
WriteLn('</div>');
WriteLn('</form>');
CommonHtmlEnd;
Halt
end;

procedure Login2;
var qpasswd : ShortString;
begin
{ security: never accept a password via GET! }
if RequestMethod <> POST then Forbidden;

qpasswd := QueryLookup('passwd');
if qpasswd <> passwd then Forbidden;

HTTPStatus(200, 'OK');
WriteLn('Set-Cookie: passwd="' + qpasswd + '"; Discard; Version="1";');
CommonHtmlStart(AKFQuizName + ': ' + msg_loggedin);
WriteLn(msg_loggedin);
WriteLn('<p><a href="results">', msg_showResults, '</a></p>');
WriteLn;
CommonHtmlEnd;
Halt
end;

procedure Logout;
begin
{ not security critical }
HTTPStatus(200, 'OK');
{ Expiration-date in the past deletes a cookie }
WriteLn('Set-Cookie: passwd=""; expires=Thu, 1-Jan-1970 00:00:00 GMT; '
        + 'Discard; Version="1";');
CommonHtmlStart(AKFQuizName + ': ' + msg_loggedout);
WriteLn('<h2>', msg_loggedout, '</h2>');
WriteLn('<p><a href="', ScriptName, '/' + ExamModeName + '/">', msg_back, 
        '</a></p>');
CommonHtmlEnd;
Halt
end;

procedure ResultListShowEntry(const dir, s: string);
begin
WriteLn('<li><a href="', s, '">', stripext(s), '</a></li>')
end;

procedure showResultList;
var found: boolean;
begin
RequireAuthorization;
HTTPStatus(200, 'OK');
CommonHtmlStart(AKFQuizName + ': ' + msg_Results);

WriteLn('<ul>');
found := ListEntries(dirname(CGI_PATH_TRANSLATED), 
                     ResultExt, 
		     ResultListShowEntry);
WriteLn('</ul>');
if not found then NoEntriesFound;

WriteLn('<hr><ul>');
WriteLn('<li><a href="reconfigure">', msg_reconfigure, '</a></li>');
WriteLn('<li><a href="logout">', msg_logout, '</a></li>');
WriteLn('<li><a href="', ScriptName, '/' + ExamModeName + '/">', 
        msg_back, '</a></li>');
WriteLn('</ul>');
CommonHtmlEnd;
Halt
end;

procedure WriteWithAmpersandReplaced(const s: FormDataString);
var i: integer;
begin
for i := 1 to length(s) do
  if s[i]<>'&'
    then Write(s[i])
    else Write('&amp;')
end;

procedure showResults; { for given file }
var 
  f: text;
  percent: integer;
  date, name: mystring;
  FormData: FormDataString;
begin
RequireAuthorization;
HTTPStatus(200, 'OK');
CommonHtmlStart(AKFQuizName + ': ' + msg_Results);

WriteLn;
WriteLn('<p><a href="results">', 
        msg_back, '</a></p>');
WriteLn('<hr>');

Assign(f, CGI_PATH_TRANSLATED);
reset(f);

repeat
  ReadLn(f, date);
  ReadLn(f, name);
  ReadLn(f, percent);
  ReadLn(f, FormData);
  ReadLn(f);
  
  WriteLn('<div>');
  WriteLn(date, ', ');
  Write('<a href="');
  WriteWithAmpersandReplaced(FormData);
  WriteLn('"');
  WriteLn('>', name, '</a>, ', percent, '%');
  WriteLn('</div>');
  WriteLn; 
until EOF(f);

close(f);

if IOResult<>0 then 
  WriteLn('<p class="error">', msg_error, '</p>');

WriteLn('<hr>');
WriteLn('<p><a href="results">', 
        msg_back, '</a></p>');
WriteLn;

CommonHtmlEnd;
Halt
end;

procedure getQueryString;

  procedure getRequestMethod; { subprocess to getQueryString }
  var method: ShortString; 
  begin
  RequestMethod := METHOD_UNKNOWN;

  method := CGIInfo('REQUEST_METHOD');
  if method = 'HEAD' then RequestMethod := HEAD;
  if method = 'GET'  then RequestMethod := GET;
  if method = 'POST' then RequestMethod := POST;
  { @@@ if method = METHOD_UNKNOWN then ... }
  end;

  procedure useGetMethod; { subprocess to getQueryString }
  begin
  CGI_QUERY_STRING := '';
  if CGIInfo('QUERY_STRING')<>'' 
     then begin
          {$IfDef __GPC__}
             { normal GetEnv is limited in size }
             CGI_QUERY_STRING := 
                CString2String(CStringGetEnv('QUERY_STRING'))
           {$Else}
              CGI_QUERY_STRING := GetEnvironmentVariable('QUERY_STRING')
           {$EndIf}
           end
  end;

  procedure usePostMethod; { subprocess to getQueryString }
  var i, len, code: integer;
  begin
  CGI_QUERY_STRING := '';
  val(CGIInfo('CONTENT_LENGTH'), len, code);
  if (len>0) and (code=0) then
     begin
     {$IfNDef FPC}
        if len > CGI_QUERY_STRING.Capacity { should never happen, but... }
          then len := CGI_QUERY_STRING.Capacity;
     {$EndIf}
     SetLength(CGI_QUERY_STRING, len);
     for i := 1 to len do Read(CGI_QUERY_STRING[i])
     end
  end;

begin { getQueryString }
getRequestMethod;

case RequestMethod of
  GET  : useGetMethod;
  POST : usePostMethod;
  otherwise CGI_QUERY_STRING := ''
  end;

QUERY_STRING_POS := 0;

if (pos('--help', CGI_QUERY_STRING)<>0) 
    or (pos('-h', CGI_QUERY_STRING)<>0) 
     then help;
if pos('--version', CGI_QUERY_STRING)<>0 then version
end; { getQueryString }

{ run quiz with a web browser }
procedure runQuiz;
var MyQuiz: Pakfquiz;
begin

{ additional tests }
if ExamMode and (CGI_QUERY_STRING<>'') then
  begin
  { GET method requires authorization }
  if (RequestMethod=GET) and (not loggedIn) then Forbidden;

  { name-field is empty }
  if (pos('name=&', CGI_QUERY_STRING)<>0)
    then rejectAnswer('No name given')
  end;


if CGI_QUERY_STRING=''
   then MyQuiz := new(Pcgiquiz, init)    { query }
   else MyQuiz := new(Pcgianswer, init); { show answer }

if (IOResult<>0) or (MyQuiz=NIL) then NotFound;
MyQuiz^.process;
dispose(MyQuiz, Done)
end;

procedure showQuizFile(ContentType: shortstring);
var 
  t: text;
  line: mystring;
begin
Assign(t, CGI_PATH_TRANSLATED);
reset(t);
if IOResult <> 0 then begin CannotOpen; exit end;

HTTPStatus(200, 'OK');
WriteLn('Content-Type: ', ContentType);
closeHttpHead;

while not EOF(t) do
  begin
  ReadLn(t, line);
  WriteLn(line)
  end;
Close(t);

if IOResult <> 0 then ;
end;

{ Quizfile requested - decide in which way to handle it }
procedure handleQuizFile;
begin
if ExamMode 
  then runQuiz { never show a quizfile as such in exam-mode! }
  else
    { send it as "application/x-akfquiz" if
      either the URI contains "?format=akfquiz" 
      or the client especially supports akfquiz files (HTTP_ACCEPT), 
      or the client has "AKFQuiz" in it's name }
    { some webservers don't support HTTP_ACCEPT }
    if (CGI_QUERY_STRING = 'format=akfquiz') 
       or (pos(AKFQuizMime, CGIInfo('HTTP_ACCEPT'))<>0) 
       or (pos('AKFQuiz', CGIInfo('HTTP_USER_AGENT'))<>0)
      then showQuizFile(AKFQuizMime)
      else 
        if CGI_QUERY_STRING = 'format=text' 
	  then showQuizFile('text/plain')
	  else runQuiz
end;

procedure checkActions;
begin
{ be careful not to open up exploitable loopholes here! }

if ExamMode then
  begin
  if CGI_PATH_INFO='/'+ExamModeName+'/results' then showResultList;
  if CGI_PATH_INFO='/'+ExamModeName+'/reconfigure' then reconfigureExamMode;
  if CGI_PATH_INFO='/'+ExamModeName+'/login' then Login;
  if CGI_PATH_INFO='/'+ExamModeName+'/login2' then Login2;
  if CGI_PATH_INFO='/'+ExamModeName+'/logout' then Logout;
  if CGI_PATH_INFO='/'+ExamModeName+'/saveconfig' then saveExamConfig
  end
end;

{ prepare CGI_PATH_TRANSLATED when ExamModeName is used in the URI }
procedure prepareExam;
var s: mystring;
begin
if ExamDir<>'' then { if Exam mode isn't disabled }
  begin
  ExamMode := true;

  s := CGI_PATH_INFO; 
  delete(s, 1, length('/' + ExamModeName));
  CGI_PATH_TRANSLATED := ExamDir + s;
  readExamConfig;
  
  if CGI_PATH_INFO = '/' + ExamModeName + '/saveconfig' then 
    saveExamConfig;

  if passwd = '' then configureExamMode
  end
end;

procedure pleaseCacheHttpHeader;
begin
{ both cache-control headers mean: please chache me! }
WriteLn('Cache-Control: public');

{ max-age must be in seconds }
{ calculation is done at compile-time, not at run-time }
WriteLn('Cache-Control: max-age=', 
  7 {days} * 24 {hours} * 60 {minutes} * 60 {seconds} ); { 7 days }
end;

procedure showImage(const img; size: integer);
type TcharArray = array[1..MaxInt] of char;
var i: integer;
begin
{ should be kept in the chache }
{ these images are prefetched (if the browser supports it) }

HTTPStatus(200, 'OK');
WriteLn('Content-Type: image/png');
WriteLn('Content-Length: ', size);
pleaseCacheHttpHeader;
closeHttpHead;

for i := 1 to size do Write(TcharArray(img)[i]);
Halt
end;

procedure getSchoolLayout;
begin
HTTPStatus(200, 'OK');
WriteLn('Content-Type: text/css');
pleaseCacheHttpHeader;
closeHttpHead;

StyleSchool;
StylePrint;
Halt
end;

procedure getBlueLayout;
begin
HTTPStatus(200, 'OK');
WriteLn('Content-Type: text/css');
pleaseCacheHttpHeader;
closeHttpHead;

StyleColor(1);
StylePrint;
Halt
end;

procedure getBrownLayout;
begin
HTTPStatus(200, 'OK');
WriteLn('Content-Type: text/css');
pleaseCacheHttpHeader;
closeHttpHead;

StyleColor(2);
StylePrint;
Halt
end;

procedure lookForStaticPages;
begin
{ all these resources can and should be kept in the cache
  (browser or proxy cache) }

{ deprecated method, but defined in GNU Coding Standards }
if (pos('/--help', CGI_PATH_INFO)<>0) 
   or (pos('/-h', CGI_PATH_INFO)<>0) 
    then help;
if pos('/--version', CGI_PATH_INFO)<>0 then version;

{ static data links }
if CGI_PATH_INFO = grRight then 
  showImage(rightImageData, sizeof(rightImageData));

if CGI_PATH_INFO = grFalse then 
  showImage(falseImageData, sizeof(falseImageData));

if CGI_PATH_INFO = grIcon then 
  showImage(AKFQuizIcon, sizeof(AKFQuizIcon));

if CGI_PATH_INFO = '/school.png' then 
  showImage(schoolImageData, sizeof(schoolImageData));

if CGI_PATH_INFO = '/q-school.css' then getSchoolLayout;

if CGI_PATH_INFO = '/q-brown.css' then getBrownLayout;

if CGI_PATH_INFO = '/q-blue.css' then getBlueLayout
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
    end;

{ if not called from browser and unknown parameters are there,
  then print help }
if CGIInfo('REQUEST_METHOD')='' then help
end;

begin
ident('$Id: cgiquiz.pas,v 1.46 2006/10/19 05:33:39 akf Exp $');

useBrowserLanguage;
ScriptName := CGIInfo('SCRIPT_NAME');
getServerName;

parameters;

CGI_PATH_INFO       := CGIInfo('PATH_INFO');
CGI_PATH_TRANSLATED := CGIInfo('PATH_TRANSLATED');
Cookie              := CGIInfo('HTTP_COOKIE');

if (CGI_PATH_INFO='') or (CGI_PATH_TRANSLATED='') then
  MovedPermanently(protocol + Servername + ScriptName + '?--help');

lookForStaticPages;

{ Don't allow to use /.. for security reasons }
{ else someone could scan through the whole machine }
if pos('/..', CGI_PATH_INFO)<>0 then Forbidden;

getQueryString;

if ExamDir<>'' then { if exam-mode is enabled }
  begin
  { Redirect /exam to /exam/ }
  if CGI_PATH_INFO = '/' + ExamModeName then 
    MovedPermanently(protocol + ServerName + ScriptName
                     + '/' + ExamModeName + '/');

  if (pos('/'+ExamModeName+'/', CGI_PATH_INFO)=1) then prepareExam;
  end;

{ check for abstract names in URI, which may need additional data }
checkActions;

if isDirectory 
  then showList
  else { concrete file }
    if ExamMode and isResultFile
      then showResults { for file }
      else handleQuizFile
end.
