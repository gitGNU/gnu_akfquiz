{
* gtkquizchooser
* Copyright (c) 2005-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: FreePascal + GTK (1.2 or 2.0)
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

{$IfDef GTK1}
  {$UnDef GTK2}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
{$EndIf}

program gtkquizchooser(input, output, stderr, o);
uses
{$IfDef GTK2}
  glib2,gdk2,gtk2,
{$Else}
  glib,gdk,gtk, { GTK 1 - faster, smaller }
{$EndIf}
SysUtils, qsys;

{$I config.inc}
{$I template.inc}

{ GNU compliant format }
const PrgVersion = 'gtkquizchooser ('+ AKFQuizName + ') ' + AKFQuizVersion;

var 
  wChooser: PGtkWidget;
  QuizFile: myString;
  Prog, ProgParams: myString;

var allowNewQuiz: boolean = false;

procedure version;
begin
WriteLn(PrgVersion);
WriteLn;
WriteLn('Copyright (C) 2006 AKFoerster');
WriteLn;
WriteLn('License: GPL v2 or later');
WriteLn;
WriteLn('This program comes with NO WARRANTY, to the extent permitted by law.');
WriteLn('You may redistribute it under the terms of the GNU General Public License;');
WriteLn('see the file named COPYING for details.');
WriteLn;
WriteLn('Written by Andreas K. Foerster');
Halt
end;


procedure help;
begin
WriteLn('GTK+ filechooser as frontend to AKFQuiz programs');
WriteLn;
WriteLn('Usage: gtkquizchooser [options] program');
WriteLn(' or:   gtkquizchooser -h | --help | /?');
WriteLn(' or:   gtkquizchooser --version');
WriteLn;
WriteLn('Options:');
WriteLn(' -d <dir>    starting directory (overrides QUIZPATH)');
WriteLn(' -n, --new   allow new files (for use with an editor)');
WriteLn('             a new file will be created with a template');
WriteLn;
WriteLn('and GTK options supported');

if BugMail <> ''
  then begin WriteLn; WriteLn('Report bugs to <' + BugMail + '>.') end;

Halt
end;

procedure createQuiz(QuizFile: string);
var o: text;
begin
Assign(o, QuizFile);
Rewrite(o);
if pos('de', getsystemlanguage)=1
   then WriteLn(o, template_de)
   else WriteLn(o, template_en);
close(o);
end;

procedure fileSelected(w: PGtkWidget; fs: PGtkFileSelection); cdecl;
var s: mystring;
begin
s := gtk_file_selection_get_filename(GTK_FILE_SELECTION(fs));
QuizFile := s;

{ don't react on directories }
if DirectoryExists(QuizFile) then begin QuizFile := ''; exit end;

{ file exists as entered? }
if FileExists(QuizFile) then
  begin gtk_widget_hide(wChooser); gtk_main_quit; exit end;

{ search }
QuizFile := ExtractFileName(s);
if getquizfile(QuizFile) then
  begin gtk_widget_hide(wChooser); gtk_main_quit; exit end;

if allowNewQuiz
   then begin { new quiz }
        QuizFile := s; { it was reset by getquizfile }
        if pos('.', QuizFile)=0 
	  then QuizFile := Quizfile + quizext; { add extension }
	gtk_widget_hide(wChooser); 
	gtk_main_quit 
	end
   else QuizFile := '' { invalid entry }
end;

procedure destroy(widget:PGtkWidget; data: gPointer); cdecl;
begin
gtk_widget_hide(wChooser);
gtk_main_quit 
end;

procedure applyQuizPath;
begin
gtk_file_selection_set_filename(GTK_FILE_SELECTION(wChooser), 
                     pGchar(IncludeTrailingPathDelimiter(getquizdir)));
end;

procedure handleParameters;
var 
  i: integer;
  s: mystring;
begin
if ParamCount=0 then help;

s := '';
i := 0;
repeat
  inc(i);
  s := ParamStr(i);
  if (s='-h') or (s='--help') or (s='/?') then help;
  if (s='--version') then version;
  if (s='-d') or (s='-D') then begin inc(i); continue end;
  if (s='-n') or (s='--new') then allowNewQuiz := true;
until (s[1]<>'-') or (i>=ParamCount);

if s[1]='-' then help;

Prog := s;
if pos(DirectorySeparator, Prog)=0 then
   begin 
   { search with PATH }
   Prog := FileSearch(Prog, GetEnvironmentVariable('PATH'));

   if Prog='' then
     begin
     WriteLn(stderr, 'Error: program not found');
     Halt(1)
     end
   end;

{ further program parameters }
while i<ParamCount do
  begin
  inc(i);
  s := ParamStr(i);
  if ProgParams<>'' 
    then ProgParams := ProgParams + ' ' + s
    else ProgParams := s
  end;
end;

begin
QuizFile := '';
Prog := '';
ProgParams := '';
handleParameters;

gtk_init(@argc, @argv);

{ Create a new file selection widget }
wChooser := gtk_file_selection_new(AKFQuizName+' '+AKFQuizVersion);

gtk_signal_connect(GTK_OBJECT(wChooser), 'destroy',
                   GTK_SIGNAL_FUNC(@destroy), @wChooser);

{ Connect the ok_button to fileSelected }
gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(wChooser)^.ok_button),
                   'clicked', GTK_SIGNAL_FUNC(@fileSelected), wChooser);

{ Connect the cancel_button to destroy the widget }
gtk_signal_connect_object(
             GTK_OBJECT(GTK_FILE_SELECTION(wChooser)^.cancel_button),
             'clicked', GTK_SIGNAL_FUNC(@gtk_widget_destroy),
              GTK_OBJECT(wChooser));

applyQuizPath;

gtk_widget_show(wChooser);
QuestionSignal;
gtk_main;

if QuizFile<>'' then 
   begin
   if not FileExists(QuizFile) then createQuiz(QuizFile);
   ExecuteProcess(Prog, ProgParams + ' ' + QuizFile)
   end
end.
