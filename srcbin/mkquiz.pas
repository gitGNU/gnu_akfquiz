{
* mkquiz
* creates HTML output for a quiz from a simplified input script
*
* the created HTML code needs the files:
* "akfquiz4.js", "leer.png", "falsch.png", "richtig.png",
* and optionally a given CSS file
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
}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Win32} {$EndIf}

{$IfDef Win32}
  {$R w32/mkquiz}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  
  {$IfDef Win32}
    {$AppType Console}
  {$EndIf}
{$EndIf}

{$I+}

program mkquiz(input, output, stderr);
uses uakfquiz, htmlquiz, qmsgs, qsys;

type TMode = (automode, makeindex);

type
  Tjavascriptquiz =
    object(Thtmlquiz)
      procedure headdata;                  virtual;
      procedure StartQuiz;                 virtual;
      procedure putgraphic;                virtual;
      procedure evaluate;                  virtual; { here: print buttons }
      procedure processHint;               virtual;
      procedure processAssessment;         virtual;
      procedure processAssessmentPercent;  virtual;
      procedure EndQuiz;                   virtual;
      procedure attachQuizfile;
    end;

var 
  MyQuiz: Tjavascriptquiz;
  MyExitCode: integer;

var 
  modes : set of TMode;
  outpath : mystring;
  idxfile : text;
    
procedure help;
begin
WriteLn;
WriteLn(AKFQuizName + ', mkquiz, version ' + AKFQuizVersion);
WriteLn('(' + platform + ')');
WriteLn('creates HTML quiz-file for JavaScript');
WriteLn('Copyright (C) ', AKFQuizCopyright);
WriteLn(msg_License, msg_GPL);
WriteLn;
WriteLn('Syntax:');
WriteLn('  mkquiz [options] [input files]');
WriteLn('  mkquiz -h | --help | /?');
WriteLn;
WriteLn('Options:');
WriteLn('-o <dir> | --out <dir>');
WriteLn('     directory for output files');
WriteLn('-a | --auto');
WriteLn('    process all quizfiles in current directory');
WriteLn('-i | --index');
WriteLn('    write an index.html for all files processed');
{$IfDef FPC} {$IfDef Go32v2}
WriteLn('-LFN           use long filenames (DOS only)');
{$EndIf} {$EndIf}
WriteLn;
WriteLn('Default charset: '+def_charset);
{$IfDef FPC}
  {$IfDef DPMI}
    WriteLn('LFN support: ', LFNsupport);
  {$EndIf}
{$EndIf}
WriteLn;
Halt
end;

function quote(s: string): mystring;
var 
  i : integer;
  e : mystring;
begin
e := '';
for i := 1 to length(s) do
  if s[i]<>'''' 
     then e := e + s[i]
     else e := e + '\''';
quote := e
end;

{ --------------------------------------------------------------------- }

procedure Tjavascriptquiz.headdata;
begin
inherited headdata;

WriteLn(outp, 
  '<meta http-equiv="Content-Script-Type" content="text/javascript"',cet);
{ the charset is always the default charset, since the data-texts are 
just the phrases from qmsgs.pas and nothing from the document }
WriteLn(outp, '<script src="', javascript,
              '" type="text/javascript" language="JavaScript" charset="', 
              def_charset, '">');
WriteLn(outp, '</script>')
end;

procedure Tjavascriptquiz.StartQuiz;
begin
inherited StartQuiz;

WriteLn(outp, '<noscript><p class="error"><strong>');
WriteLn(outp, msg_noJS, '<br', cet);
WriteLn(outp, msg_notusable);
WriteLn(outp, '</strong></p></noscript>');

WriteLn(outp);
WriteLn(outp, '<form name="akfquiz" action="">');
WriteLn(outp);
WriteLn(outp, '<a name="top" id="top"></a>')
end;

procedure Tjavascriptquiz.putgraphic;
begin
if not neutral then
  begin
  Write(outp, '<img name="q', questionNr, '"');
  if rtl 
    then Write(outp, ' style="float:left; vertical-align:text-bottom;"')
    else Write(outp, ' style="float:right; vertical-align:text-bottom;"');
  WriteLn(outp, ' width="18" height="18" alt="" src="leer.png"', cet)
  end
end;

procedure Tjavascriptquiz.processAssessment;
begin

{ comment out when assessmentURI is given }
{ but I want it kept available in the HTML source }
if assessmentURI<>'' then WriteLn(outp, '<!--');

WriteLn(outp);
WriteLn(outp, '<div class="assessment"'+
              ' style="clear:both; display:none">');
processBlock;
WriteLn(outp, '</div>');
WriteLn(outp);

if assessmentURI<>'' then WriteLn(outp, '-->')
end;

procedure Tjavascriptquiz.processAssessmentPercent; {@@@@}
var 
  s: mystring;
  value, oldvalue: pointsType;
begin
if assessmentURI='' then
   begin
   WriteLn(outp);
   WriteLn(outp, '<script type="text/javascript" charset="', 
                 charset, '">');
   
   WriteLn(outp, 'var asmntval = new Array();');
   WriteLn(outp, 'var asmnttxt = new Array();');

   oldvalue := 101; { larger than 100 }
   s := readLine;
   while s<>'' do
     begin
     if not getPointsFromLine(value, s) then error;
     if value >= oldvalue then error; { enforce descending order }
     
     WriteLn(outp);
     WriteLn(outp, 'asmntval.push(', value:3, ' );');
     { Takfquiz.htmlconvert: strips html code from text string }
     WriteLn(outp, 'asmnttxt.push(''', quote(Takfquiz.htmlconvert(s)), ''');');
     s := readLine;
     oldvalue := value
     end;

   { last value must be 0! }
   if value <> 0 then error; 

   WriteLn(outp, '</script>');
   WriteLn(outp)
   end
end;

procedure Tjavascriptquiz.processHint;
begin
WriteLn(outp, '<div class="hint" style="clear:both; display:none">');
processBlock;
WriteLn(outp, '</div>')
end;

procedure Tjavascriptquiz.evaluate;
begin
if AssessmentURI<>'' then
  begin
  writeLn(outp);
  writeLn(outp, '<input type="hidden" name="assessmentURI" id="assessmentURI"'+
                'value="', AssessmentURI, '">');
  writeLn(outp)
  end;

inherited evaluate
end;

procedure Tjavascriptquiz.attachQuizfile;
var 
  s, e: mystring;
  endfound: boolean;
begin
WriteLn(outp);
WriteLn(outp, '<!-- Quiz:');
WriteLn(outp);

resetQuiz; { back to the beginning of file }

{ search beginning of AKFQuiz part }
repeat
  ReadRawLine(s);
  e := makeUpcase(stripWhitespace(s));
until (pos('AKFQUIZ', e)=1) or checkEOF;

{ write AKFQuiz line }
if not checkEOF then WriteLn(outp, s);

{ process the rest of the file }
endfound := false;
while (not checkEOF) and (not endfound) do
  begin
  ReadRawLine(s);
  s := replaceall(s, '--', '-\-');
  e := makeUpcase(stripWhitespace(s));
  endfound := (e='END') or (e='ENDE');
  WriteLn(outp, s)
  end;

{ keyword END must be used here! }
if not endfound then WriteLn(outp, 'END');
WriteLn(outp, '-->')
end;

procedure Tjavascriptquiz.EndQuiz;
begin
if not evaluated then evaluate;

writeLn(outp, '<div align="center" class="buttons">');
writeLn(outp, '<input type="button" accesskey="r" value=" ',
              msg_result, ' " ');
write(outp, ' onClick="Result(''',
            quote(msg_sol1), ''', ''', quote(msg_sol2), ''', ''', 
            quote(msg_sol3), ''', ''', quote(msg_sol4), ''', ');
if neutral 
  then write(outp, ''''', ')
  else write(outp, '''', quote(msg_sol5), ''', ');
writeLn(outp, '''', quote(msg_seen), ''', ', MaxPoints, 
              ')"',cet,'&nbsp;');

if not neutral then
  begin
  writeLn(outp, '<input type="button" accesskey="s" value=" ',
                msg_solution, ' "');
  writeLn(outp, ' onClick="Solution(''',
                quote(msg_really),''')"',cet,'&nbsp;')
  end;

writeLn(outp, '<input type="reset" accesskey="n" value=" ',
              msg_new, ' " onClick="New()"',cet);
writeLn(outp, '</div>');

WriteLn(outp);
WriteLn(outp, '</form>');

inherited EndQuiz;

attachQuizfile
end;

{ --------------------------------------------------------------------- }
{ Indexer }

procedure makeIndexEntry(const quizfile, htmlfile: string);
begin
WriteLn(idxfile, '<a href="', basename(htmlfile), '">');
WriteLn(idxfile, getQuizTitle( quizfile ), 
                 '</a><br>') { change-xhtml }
end;

procedure startIndex;
begin
Assign(idxfile, outpath + 'index.html');
{$I-}
rewrite(idxfile);
{$I+}
if IOResult<>0 then
  writeLn(stderr, 'error: cannot write ', outpath, 'index.html');

WriteLn(idxfile, HTMLDocType);
WriteLn(idxfile);
WriteLn(idxfile, '<html>');
WriteLn(idxfile, '<head>');
WriteLn(idxfile, '<meta name="generator" content="'+
                 AKFQuizName + ' ' + AKFQuizVersion+'">'); { change-xhtml }
WriteLn(idxfile, '<meta http-equiv="Content-Type" '+
                 'content="text/html; charset=UTF-8">');
WriteLn(idxfile);
WriteLn(idxfile, '<title>AKFQuiz</title>');
WriteLn(idxfile);
WriteLn(idxfile, '<style type="text/css">');
WriteLn(idxfile, '<!--');
WriteLn(idxfile, 'body { color:black; background:#d8d0c8; margin:1ex 8%; }');
WriteLn(idxfile, 'h1 { color:#ffffdd; background:#605030; padding:12px;');
WriteLn(idxfile, '     border:12px ridge; border-color:#605030; margin:1em 15%;');
WriteLn(idxfile, '     text-align:center; font-weight:bold; }');
WriteLn(idxfile, '.error { color:red; background:transparent;');
WriteLn(idxfile, '         font-weight:bold; font-style:italic;}');
WriteLn(idxfile, '-->');
WriteLn(idxfile, '</style>');
WriteLn(idxfile, '</head>');
WriteLn(idxfile);
WriteLn(idxfile, '<body>');
WriteLn(idxfile);
WriteLn(idxfile, '<div align="center">');
WriteLn(idxfile, '<h1>AKFQuiz</h1>');
WriteLn(idxfile);
WriteLn(idxfile, '<noscript><p class="error"><strong>');
WriteLn(idxfile, UTF8toHTML(msg_noJS), ' <br>'); { change for xhtml }
WriteLn(idxfile, UTF8toHTML(msg_notusable));
WriteLn(idxfile, '</strong></p></noscript>');
WriteLn(idxfile)
end;

procedure endIndex;
begin
WriteLn(idxfile, '</div>');
WriteLn(idxfile, '</body>');
WriteLn(idxfile, '</html>');
close(idxfile);
WriteLn(stderr, 'index file "', outpath, 'index.html" written')
end;

{ --------------------------------------------------------------------- }
procedure convertfile(var infile, outfile: mystring);
var savelang: languages;
begin
{ I don't use the QUIZPATH variable here, 
  because it would have surprising side-effects in scripts. }
if not quizfileExists(infile) then
   begin
   WriteLn(stderr, msg_filenotfound);
   halt(1)
   end;

savelang := lang;
MyQuiz.Init(infile, outfile);
MyQuiz.process;
MyExitCode := MyQuiz.GetError;
MyQuiz.Done;
lang := savelang
end;

procedure convertsinglefile(const dir, s: string);
var infile, outfile: mystring;
begin
infile := s;
outfile := outpath+gethtmlname(infile);

WriteLn(stderr, '"', infile, '" -> "', outfile, '"');

if infile=outfile 
  then WriteLn(stderr, msg_error)
  else begin
       convertfile(infile, outfile);
       if makeIndex in modes then makeIndexEntry(infile, outfile)
       end
end;

procedure runautomode;
var found: boolean;
begin
{ use '' for actual directory - no Quizpath used }
found := ListEntries('', quizext, convertsinglefile);
if ListEntries('', quizext2, convertsinglefile) then found := true;
if not found then WriteLn(stderr, msg_noquizfound)
end;

procedure processParameters;
var 
  i: integer;
  count: LongInt;
  p: mystring;
  infile, outfile: mystring;
begin
{ empty strings mean standard input/output }
Infile  := '';
outfile := '';
outpath := '';
p       := '';

count := ParamCount;

i := 0;
if count<>0 then
  { handle options }
  repeat
    inc(i);
    p := makeUpcase(ParamStr(i));
    if (p='-H') or (p='--HELP') or (p='/?') then help;
    if (p='-A') or (p='--AUTO') then 
        begin modes := modes + [automode]; continue end;
    if (p='-I') or (p='--INDEX') then 
        begin 
	modes := modes + [makeindex];
	StartIndex;
	continue
	end;
    if p='-LFN' then
        begin setLFNsupport; continue end;
    if (p='-O') or (p='--OUT') or
       (p='-D') or (p='--DIR') then 
       { -d or --dir should not be used anymore, 
         because it can be confused with -d in the other programs }
       begin
       inc(i);
       outpath := useDirSeparator(ParamStr(i));
       continue
       end;
    if p='-' then 
        begin infile:=''; outfile :=''; continue end;
    if p[1]='-'    { "/" might be used in a path }
       then help; { unknown parameter }
  until (i=count) or (p[1]<>'-');

{ no parameters or last one is is option => no filenames 
  => stdin, stdout }
if ((count=0) or (p[1] = '-')) and not (automode in modes) 
   then convertfile(infile, outfile);

{ filenames }
if (count<>0) and (p[1]<>'-') then
  while i <= count do
    begin
    infile := ParamStr(i); { not Upcase }
    convertsinglefile('', infile);
    inc(i)
    end;

if automode in modes then runautomode;
if makeindex in modes then endIndex
end;

begin
outpath := '';
modes := [];
useSystemLanguage;

processParameters;

if MyExitCode<>0 then WriteLn(stderr, msg_error);
Halt(MyExitCode)
end.
