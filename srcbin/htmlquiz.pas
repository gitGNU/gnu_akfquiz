{
* htmlquiz (unit)
*
* $Id: htmlquiz.pas,v 1.3 2006/08/27 06:47:35 akf Exp $
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

  NoProjectLink
    disable Link to the Projects Homepage
}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}  
  {$Smartlink on}
{$EndIf}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Win32} {$EndIf}

{$I-} { no automatic I/O-Check }
{$X+} { function call as procedure allowed }

unit htmlquiz;

interface
uses uakfquiz, qsys, qmsgs;

type 
  Phtmlquiz = ^Thtmlquiz;
  Thtmlquiz = 
    object(Takfquiz)
      protected
        outp: text;
        errorcode : integer;
	cet : string[3];              { close sequence for empty tags }
	BR  : string[6];              { <br> or <br /> }

      public
        constructor Init(infile, outfile: string);
        destructor Done;                            virtual;
        procedure newoutput;                        virtual;
	function  handleURIs(const x: string): mystring;
        function  htmlconvert(const x: mystring): mystring; virtual;
        procedure headdata;                         virtual;
        procedure StartQuiz;                        virtual;
        procedure putgraphic;                       virtual;
	procedure showanswer(value: pointsType;
	                     const ans: string);    virtual;
	procedure processBlock;
        procedure processComment;                   virtual;
        procedure processQuestion;                  virtual;
        procedure processAnswer;                    virtual;
        procedure processMulti;                     virtual;
	procedure processDefaultAnswer;		    virtual;
        procedure processAssessment;                virtual;
        procedure setcharset(cs: string);           virtual;
        procedure handleSettingsURIs;
        procedure EndQuiz;                          virtual;
	function  checkTimeout: boolean;            virtual;
        procedure error;			    virtual;
        function  geterror: integer;

    protected
        function getAssessmentURI: mystring;
    private
        procedure newparagraph;
    end;

function qTypeStr(qType: TquestionType): mystring;


implementation

{ ATTENTION: never use non-ascii characters for the output here!
  The charset-encoding might be set to a different one in the 
  input-script. Instead use HTML-entities, like &copy; or &euro; }

function qTypeStr(qType: TquestionType): mystring;
begin
case qType of
  MC   : qTypeStr := 'radio';
  MCMA : qTypeStr := 'checkbox';
  otherwise RunError
  end
end;

constructor Thtmlquiz.Init(infile, outfile: string);
begin
inherited Init(infile);
errorcode := 0;
cet := '>'; { for xhtml: ' />' }
BR  := '<br' + cet;

{ use HTML-entities in messages, so they are independent of the 
  charset set in the quizfile }
setmsgconverter(UTF8toHTML);

assign(outp, outfile);
newoutput;
if IOResult<>0 then fail
end;

destructor Thtmlquiz.Done;
begin
inherited Done;
close(outp)
end;

procedure Thtmlquiz.newoutput;
begin
Rewrite(outp)
end;

function Thtmlquiz.handleURIs(const x: string): mystring;
var
  s, URI, rest: mystring;
  img, URIprefix, f, t, p : LongInt; { image, from, to, position }
  blank: boolean; { on a blank page? }
begin
s := '';
rest := x;

repeat
  f := 0;

  { prefixes: }
  img :=  pos('image:', rest);
  
  URIprefix := pos('URI:', rest);
  if URIprefix=0 then URIprefix := pos('URL:', rest);
  { URN: is handled later }
  if URIprefix<>0 then f := URIprefix + length('UR?:');

  { browser internal URIs }
  if f=0 then f := pos('http://', rest);   { RFC2616 }
  if f=0 then f := pos('https://', rest);  { RFC2818 }
  if f=0 then f := pos('ftp://', rest);    { RFC1738 }
  if f=0 then f := pos('tftp://', rest);   { RFC3617 }
  if f=0 then f := pos('gopher://', rest); { RFC-hoffman-gopher-uri-03.txt }
  if f=0 then f := pos('wais://', rest);   { RFC4156 }
  if f=0 then f := pos('file://', rest);   { RFC1738 }

  blank := (f<>0); { if one of the above is found, use a blank page }
  
  { only the above URIs can be handles as images }
  if (img<>0) and (f=0) then img := 0;
  
  { if the URI isn't directly after "image:", then it's not an image }
  if (img<>0) and (f<>img+length('image:')) 
                  and (URIprefix<>img+length('image:'))
	                 then img := 0;

  { URIs for external programs - no _blank target }
  if f=0 then f := pos('javascript:', rest);   { ? }
  if f=0 then f := pos('mailto:', rest);   { RFC2368 }
  if f=0 then f := pos('telnet:', rest);   { RFC4248 } { officially with // }
  if f=0 then f := pos('tn3270:', rest);   { ??? }
  if f=0 then f := pos('news:', rest);     { RFC1738 }
  if f=0 then f := pos('nntp://', rest);   { RFC1738 }
  if f=0 then f := pos('tel:', rest);      { RFC2806 }
  if f=0 then f := pos('fax:', rest);      { RFC2806 }
  if f=0 then f := pos('modem:', rest);    { RFC2806 }
  if f=0 then f := pos('nfs://', rest);    { RFC2224 }
  if f=0 then f := pos('ldap://', rest);   { RFC-ietf-ldapbis-url-09.txt }
  if f=0 then f := pos('prospero://', rest); { RFC4157 }
  if f=0 then f := pos('urn:', rest);      { RFC2141 }
  if f=0 then f := pos('URN:', rest);      { RFC2141 }

  { not official }
{ if f=0 then f := pos('fish:', rest); }

  if f<>0 then
    begin
    { start up to the found URI }
    if img=0 
      then s := s + copy(rest, 1, f-1)
      else s := s + copy(rest, 1, img-1);

    { extrapolate URI }
    URI := rest;
    Delete(URI, 1, f-1);
    
    { search end of URI: }
    t := length(URI);
    p := pos('&gt;', URI);
    if p<>0 then t := min(t, p);
    p := pos(' ', URI);
    if p<>0 then t := min(t, p);
    p := pos(TAB, URI);
    if p<>0 then t := min(t, p);
    p := pos('&quot;', URI);
    if p<>0 then t := min(t, p);
    if t<>length(URI) 
      then begin dec(t); URI := copy(URI, 1, t) end;
    
    delete(rest, 1, f+t-1);

    if img<>0 
      then s := s+'<img src="'+URI+'" alt="['+URI+
                ']" style="vertical-align:middle; float:right">'
      else if blank
             then s := s+'<a href="'+URI+'" target="_blank">'+URI+'</a>'
             else s := s+'<a href="'+URI+'">'+URI+'</a>'
    end
until (rest='') or (f=0);

handleURIs := s + rest
end;

{ quotes characters, that have special meanings in HTML }
function Thtmlquiz.htmlconvert(const x: mystring): mystring;
var 
   i: integer;
   e: mystring;
begin
if HTMLcode 
   then htmlconvert := x
   else begin
        e:='';
        for i:=1 to length(x) do
            case x[i] of
                 '<': e := e + '&lt;';
                 '>': e := e + '&gt;';
                 '&': e := e + '&amp;';
                 '"': e := e + '&quot;';
                 otherwise 
		   if (x[i] > chr(31)) or (x[i] = TAB) then e := e + x[i]
                 end;
	if started 
	   then htmlconvert := handleURIs(e)
	   else htmlconvert := e
        end
end;

procedure Thtmlquiz.error; {@@@}
begin
errorcode := 1;

if not started then {@@@}
  begin
  StartQuiz;
  quit := true
  end;

{ don't use no block-elements! It would break an existing block }
case lang of
  deutsch:
       begin
       Write(outp, '<strong class="error"');
       if rtl then Write(outp, ' dir="ltr"'); { switch back to ltr }
       WriteLn(outp, '>');
       WriteLn(outp, 'Fehler in den Eingabedaten</strong>', BR);
       WriteLn(stderr,'Fehler in den Eingabedaten - '+
                      'Einzeilheiten im HTML-Code')
      end
  otherwise
       begin
       Write(outp, '<strong class="error" lang="en"');
       if rtl then Write(outp, ' dir="ltr"'); { switch back to ltr }
       WriteLn(outp, '>');
       WriteLn(outp, 'fault in input data</strong>', BR);
       WriteLn(stderr, 'fault in input data - look at the HTML code')
       end
  end { case }
end;

procedure Thtmlquiz.headdata;
begin
if noindex then
   WriteLn(outp, '<meta name="robots" content="noindex"', cet);
WriteLn(outp, '<meta http-equiv="Content-Type" content="text/html; charset=',
        charset,'"', cet);

{$IfDef Win32}
WriteLn(outp, '<!-- AKFoerster: I don''t really like Windows, '
            + 'but I have to support it -->');
{$EndIf}

if language<>'' then
   WriteLn(outp, '<meta http-equiv="Content-Language" content="',language,
                 '"', cet);
if author<>'' then
   WriteLn(outp, '<meta name="author" content="', author, '"', cet);
if copyright<>'' then
   WriteLn(outp, '<meta name="copyright" content="', copyright, '"', cet);
if license<>'' then
   WriteLn(outp, '<meta name="license" content="', license, '"', cet);
if keywords<>'' then
   WriteLn(outp, '<meta name="keywords" content="', keywords, '"', cet);

WriteLn(outp, '<meta http-equiv="Content-Style-Type" content="text/css"', cet);
WriteLn(outp, '<style type="text/css">');
WriteLn(outp, '<!-- @media print { .buttons, .defanswer, .resultlink, noscript {display:none} } -->');
WriteLn(outp, '</style>');

if CSS<>'' then
   WriteLn(outp, '<link rel="stylesheet" type="text/css" href="',CSS,'"', cet)
end;

procedure Thtmlquiz.StartQuiz;
begin
inherited StartQuiz;

WriteLn(outp, HTMLDocType);
WriteLn(outp);

Write(outp, '<html');
if language<>''
   then Write(outp, ' lang="',language,'"');
if RTL
   then Write(outp, ' dir="rtl"');
WriteLn(outp, '>');
WriteLn(outp);
WriteLn(outp, '<head>');
WriteLn(outp);
WriteLn(outp, '<meta name="generator" content="'+AKFQuizName+' '
              +AKFQuizVersion+'"', cet);

headdata;
handleSettingsURIs;

WriteLn(outp);
WriteLn(outp, '<title>', title, '</title>');
WriteLn(outp, '</head>');
WriteLn(outp);
WriteLn(outp);
WriteLn(outp, '<body>');
WriteLn(outp);
WriteLn(outp, '<h1>', title, '</h1>');
WriteLn(outp);

if (author<>'') or (authorURI<>'') or (copyright<>'') or 
   (translator<>'') or (license<>'') or (licenseURI<>'') then
  begin
  WriteLn(outp, '<dl id="metadata">');
  if author<>'' then
    WriteLn(outp, '<dt>', msg_author, '</dt> <dd>', author, '</dd>');
  if copyright<>'' then 
    WriteLn(outp, '<dt>Copyright &copy;:', '</dt> <dd>', copyright, '</dd>');
  if authorURI<>'' then
    WriteLn(outp, '<dt>', msg_authorURI, '</dt> <dd>', authorURI, '</dd>');
  if translator<>'' then 
    WriteLn(outp, '<dt>', msg_translator, '</dt> <dd>', translator, '</dd>');
  if edited<>'' then 
    WriteLn(outp, '<dt>', msg_edited, '</dt> <dd>', edited, '</dd>');
  if license<>'' then 
    WriteLn(outp, '<dt>', msg_license, '</dt> <dd>', license, '</dd>');
  if licenseURI<>'' then 
    WriteLn(outp, '<dt>', msg_licenseURI, '</dt> <dd>', licenseURI, '</dd>');
  WriteLn(outp, '</dl>');
  WriteLn(outp)
  end
end;

procedure Thtmlquiz.handleSettingsURIs;
begin
title      := handleURIs(title);
author     := handleURIs(author);
authorURI  := handleURIs(authorURI);
copyright  := handleURIs(copyright);
translator := handleURIs(translator);
license    := handleURIs(license); { may have an URI }
licenseURI := handleURIs(licenseURI);
defanswer  := handleURIs(defanswer);
keywords   := handleURIs(keywords)
end;

procedure Thtmlquiz.EndQuiz;
begin
{$IfNDef NoProjectLink}
  WriteLn(outp);
  WriteLn(outp, '<div align="right" dir="ltr" class="made"><small>');
    WriteLn(outp, msg_made, ' <a '+
      'href="', Homepage, '" target="_blank">'+
           AKFQuizName + '</a>');
  WriteLn(outp, '</small></div>');
{$EndIf}
WriteLn(outp);
WriteLn(outp, '</body>');
WriteLn(outp, '</html>')
end;

{ close old paragraph and open a new one }
procedure Thtmlquiz.newparagraph;
begin
WriteLn(outp, '</p>');
WriteLn(outp);
WriteLn(outp, '<p>')
end;

procedure Thtmlquiz.processBlock;
var s: mystring;
begin
WriteLn(outp, '<p>');
s := readLine;
while s<>'' do
  begin
  if s<>'' 
    then begin
         if s='.' { new paragraph? }
	   then newparagraph
           else WriteLn(outp, s) { not s='.' }
        end; { s<>'' }
  s := readLine
  end;
if s<>'' then WriteLn(outp, s);
WriteLn(outp, '</p>')
end;

procedure Thtmlquiz.processComment;
begin
WriteLn(outp);
WriteLn(outp, '<div class="comment">');
processBlock;
WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Thtmlquiz.putgraphic;
begin end;

procedure Thtmlquiz.showanswer(value: pointsType; const ans: string);
begin
inc(answerNr);

{ it must be included in block containers to make RTL work correctly }
WriteLn(outp, '<div>');
WriteLn(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
              ' name="q',questionNr,'"',
              ' value="', value, '"',
              ' type="', qTypeStr(qType), '"', cet);

Write(outp, '<label for="q', questionNr, 'a', answerNr, '">');
Write(outp, ans);
WriteLn(outp, '</label>');
WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Thtmlquiz.processDefaultAnswer;
begin
inc(answerNr);
WriteLn(outp, '<div class="defanswer">');
WriteLn(outp, '<input  id="q', questionNr, 'a', answerNr, '"',
	      ' name="q', questionNr, '"',
              ' type="radio" value="0" checked', cet); { xhtml-change }

Write(outp, '<label for="q', questionNr, 'a', answerNr, '">');
Write(outp, defanswer);
WriteLn(outp, '</label></div>', BR)
end;

procedure Thtmlquiz.processAnswer;
var 
  value: pointsType;
  s: mystring;
begin
WriteLn(outp, '<div class="answer">');

repeat
  readanswer(value, s);
  if s<>'' then showanswer(value, s);
until s='';

if (defanswer<>'') and (qType=MC) then processDefaultAnswer;

if not neutral then putgraphic;

WriteLn(outp, '</div>');
WriteLn(outp)
end;

procedure Thtmlquiz.processQuestion;
begin
inherited processQuestion;
WriteLn(outp);
WriteLn(outp, '<div class="question" id="question', questionNr, '">');
processBlock;
WriteLn(outp, '</div>');
WriteLn(outp);
processAnswer
end;

procedure Thtmlquiz.processMulti;
begin
inherited processMulti;
WriteLn(outp);
WriteLn(outp, '<div class="question" id="question', questionNr, '">');
processBlock;
WriteLn(outp, '</div>');
WriteLn(outp);
processAnswer
end;

procedure Thtmlquiz.processAssessment;
begin
{ don't use the assessment-block, when assessmentURI is given }
if assessmentURI<>'' 
  then ignoreBlock
  else begin
       WriteLn(outp);
       WriteLn(outp, '<div class="assessment">');
       processBlock;
       WriteLn(outp, '</div>');
       WriteLn(outp)
       end
end;

procedure Thtmlquiz.setcharset(cs: string);
begin
inherited setcharset(cs);

cs := makeUpcase(cs);
if checkASCII(cs) 
  then setconverter(forceASCII)
  else setconverter(noconversion)
end;

function Thtmlquiz.getAssessmentURI: mystring;
begin
getAssessmentURI := assessmentURI
end;

function Thtmlquiz.geterror: integer;
begin geterror := errorcode end;

function Thtmlquiz.checkTimeout: boolean;
begin
{ not interactive, so always false }
checkTimeout := false
end;

end.
