{
* wquizchooser
* quizchooser for Windows
* wquizchosser starts in current working directory
*
* Copyright (c) 2005 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: FreePascal - Windows only
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
  {$AppType GUI}
  {$Mode Delphi}
{$EndIf}

{$R wquizchooser.res}
{$X+}

program wquizchooser(o);
uses windows, sysutils, qsys;

type TFileName = array[0..max_path] of char;

{$I config.inc}
{$I template.inc}

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

function GetQuizFile(var FName: TFileName; open: boolean): boolean;
var NameRec : OpenFileName;
Begin
  FillChar(NameRec,SizeOf(NameRec),0);
  FName[0] := #0;
  with NameRec do
    begin
    LStructSize     := SizeOf(NameRec);
    HWndOwner       := 0;
    lpstrTitle      := AKFQuizName+' '+AKFQuizVersion+' wquizchooser';
    { short name must be first for W95 }
    lpstrFilter     := 'AKFQuiz'#0'*.aqz;*.akfquiz'#0+
                        'All files (*.*)'#0'*.*'#0#0;
    lpstrInitialDir := PChar(getquizdir);
    lpstrFile       := @FName;
    NMaxFile        := max_path;
    Flags           := OFN_LONGNAMES or OFN_HIDEREADONLY or
                       OFN_NONETWORKBUTTON;
    if open then Flags := Flags or OFN_FileMustExist;
    lpstrDefExt     := 'aqz'  { W95: just 3 chars used }
    end;

GetQuizFile := GetOpenFileName(@NameRec)
end;

procedure editQuiz;
var QuizFile : TFileName;
begin
If GetQuizFile(QuizFile, false) then
  begin
  if not FileExists(QuizFile) then createQuiz(QuizFile);
  ShellExecute(0,'open','notepad.exe',PChar('"'+PChar(QuizFile)+'"'),'',
               SW_SHOWNORMAL)
  end
end;

procedure openQuiz;
var 
  QuizFile : TFileName;
  prog,
  param: mystring;
  i: integer;
begin
{ first Parameter is the program, the rest are parameters for the prog }
{ GetQuizfile changes the directory, so ExpandFileName must come first }
prog := ExpandFileName(ParamStr(1));
param := '';
for i:=2 to ParamCount do
  param := param + ParamStr(i) + ' ';

If GetQuizFile(QuizFile, true) then
  ShellExecute(0,'open',PChar(prog),
                        PChar(param + '"' + PChar(QuizFile) + '"'),
                        '.', SW_SHOWNORMAL)
end;

begin
If ParamCount=0 then editQuiz else openQuiz
end.
