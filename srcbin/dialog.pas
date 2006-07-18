{
* Dialog
* version 0.3
*
* small interface to dialog-systems like 
*   Xdialog, gdialog, dialog, whiptail
* for more information look into the manpages of the dialog-system
*
* This unit was originally written for AKFQuiz
*
* Copyright (c) 2003-2004 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* This Unit is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* This unit is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA
*
*}

unit Dialog;

{$I-}

{$IfDef FPC}
  {$MODE DELPHI}
  {$LONGSTRINGS ON}
  {$Smartlink on}
{$EndIf}


interface

{$IfDef __GPC__}
  import GPC (ExecuteNoTerminal => Shell);
{$EndIf}

{ some dialog Variants can be used with 0 sizes to switch to 
auto-sizing - but only some
So, use with care! }
const autosize = 0;

{ Strings are implemented differently }
{$IfDef FPC}
  type DialogString = AnsiString;    { like in Delphi }
  { since "LongStrings" is set, FPC always uses AnsiStrings }
{$else}  
  type DialogString = String(2048);  { conforming to ISO-10206 }
  { I use just "string" whenever possible, to avoid overhead }
{$EndIf}

{ these variables might be set from the main-program }
var
  { DialogCmd might be set to: dialog, cdialog, Xdialog, gdialog, whiptail }
  DialogCmd       : String[80] = 'dialog';
  DialogParams    : String[80] = '';          { normally none needed }
  DialogTitle     : String[80] = '';
  DialogBacktitle : String[80] = '';

var DialogCode : integer; { exit-code of dialog }

{ basic procedures }

procedure CallDialog(const s: string);
procedure GetDialog(const s1: string; var s2: DialogString);


{ normal procedures }

procedure clear;

function yesno(const txt: string; height, width: integer): boolean;
function yesno_a(const txt: string): boolean;

procedure msgbox(const txt: string; height, width: integer);
procedure msgbox_a(const txt: string);

{ might fall back to a msgbox in a GUI environment }
procedure infobox(const txt: string; height, width: integer);
procedure infobox_a(const txt: string);

procedure textbox(const filename: string; height, width: integer);
procedure textbox_a(const filename: string);

procedure inputbox(const txt: string; height, width: integer; 
                   const init: string; var answer: DialogString);
procedure inputbox_a(const txt: string;
                     const init: string; var answer: DialogString);

{ normally use an empty String for 'init'! }
procedure passwordbox(
  const txt: string; height, width: integer; 
  const init: string; var answer: DialogString);
procedure passwordbox_a(
  const txt: string;
  const init: string; var answer: DialogString);

{ adds menuitems to a menuitem-string }
procedure addmenuitem(var menuitems: DialogString; tag, item: string);

{ adds listitems to a listitem-string }
procedure addlistitem(var listitems: DialogString; tag, item: string; 
                      status: boolean);

procedure menu(
  const txt: string; height, width, menuheight: integer; 
  const menuitems: string; var answer: DialogString);
procedure menu_a(
  const txt: string; menuheight: integer; 
  const menuitems: string; var answer: DialogString);

procedure checklist(
  const txt: string; height, width, listheight: integer; 
  const listitems: string; var answer: DialogString);
procedure checklist_a(
  const txt: string; listheight: integer;
  const listitems: string; var answer: DialogString);

procedure radiolist(
  const txt: string; height, width, listheight: integer; 
  const listitems: string; var answer: DialogString);
procedure radiolist_a(
  const txt: string; listheight: integer;
  const listitems: string; var answer: DialogString);

{ gauge not implemeted }

{ The following boxes are not standard and should be used with care! }

procedure fselect(const default: string; height, width: integer; 
                  var answer: DialogString);
procedure fselect_a(const default: string;
                    var answer: DialogString);

procedure dselect(const default: string; height, width: integer; 
                  var answer: DialogString);
procedure dselect_a(const default: string;
                    var answer: DialogString);

{ to be continued... }



implementation
{$IfDef FPC}
  uses BaseUnix, Unix, SysUtils;
{$EndIf}

{$IfDef __GPC__}
  function IntToStr(i: integer): DialogString;
  var s: DialogString;
  begin
  str(i, s);
  IntToStr := s
  end;
{$EndIf}

function quote(const x: String): DialogString;
var 
  i: integer;
  e: DialogString;
begin
e := '';
for i := 1 to length(x) do
  case x[i] of 
    '"'  : e := e + '\"';
    '\'  : e := e + '\\';
    '$'  : e := e + '\$';
    otherwise e := e + x[i]
    end;
quote := e
end;

function DefaultParameters: DialogString;
var 
  params : DialogString;
begin
if DialogParams<>'' 
  then params := ' '+DialogParams
  else params := '';

if DialogBackTitle <> '' then
  params := params + ' --backtitle "' + quote(DialogBackTitle) + '"';
if DialogTitle <> '' then
  params := params + ' --title "' + quote(DialogTitle) + '"';
defaultparameters := params + ' '
end;

procedure GetDialog(const s1: string; var s2: DialogString); {@@@}
var 
  F : text;
  tmpfile : String[255];
begin
{ choosing a filename for a tempfile is not that easy 
  it may open up a security hole! }

{ leave the expertise to the compiler-makers when possible }
{$IfDef __GPC__}
   tmpfile := GetTempFileName;
{$Else}
   { on FPC there is no random element, so it's better to do it
     in the HOME directory }
   tmpfile := GetTempFileName(GetEnvironmentVariable('HOME'), '.akfquiz');
{$EndIf}

DialogCode :=
    Shell(DialogCmd + DefaultParameters + s1 + ' 2>'+tmpfile);

{ Read the line from tmpfile and erase tmpfile }
Assign(F, tmpfile);
Reset(F);
ReadLn(F, s2);
close(F);
erase(F);
If IOResult<>0 then s2 := ''
end;

procedure CallDialog(const s: string);
begin
DialogCode := Shell(DialogCmd + DefaultParameters + s)
end;


procedure clear;
begin
CallDialog('--clear')
end;

function yesno(const txt: string; height, width: integer): boolean;
begin
CallDialog('--yesno "' + quote(txt) + '" ' + 
           IntToStr(height) + ' ' + IntToStr(width));
yesno := DialogCode <> 0
end;

function yesno_a(const txt: string): boolean;
begin
yesno_a := yesno(txt, autosize, autosize)
end;

procedure msgbox(const txt: string; height, width: integer);
begin
CallDialog('--msgbox "' + quote(txt) + '" ' + 
           IntToStr(height) + ' ' + IntToStr(width))
end;

procedure msgbox_a(const txt: string);
begin
msgbox(txt, autosize, autosize)
end;

procedure infobox(const txt: string; height, width: integer);
begin
CallDialog('--infobox "' + quote(txt) + '" ' + 
           IntToStr(height) + ' ' + IntToStr(width))
end;

procedure infobox_a(const txt: string);
begin
infobox(txt, autosize, autosize)
end;

procedure textbox(const filename: string; height, width: integer);
begin
CallDialog('--textbox "'+quote(filename)+'" '+
           IntToStr(height)+' '+IntToStr(width))
end;

procedure textbox_a(const filename: string);
begin
textbox(filename, autosize, autosize)
end;

procedure inputbox(const txt: string; height, width: integer; 
                   const init:string; var answer: DialogString);
begin
GetDialog('--inputbox "' + quote(txt) + '" '+
          IntToStr(height)+' '+IntToStr(width)+' "'+quote(init)+'"', 
	  answer)
end;

procedure inputbox_a(const txt: string; 
                     const init:string; var answer: DialogString);
begin
inputbox(txt, autosize, autosize, init, answer)
end;

procedure passwordbox(const txt: string; height, width: integer; 
                      const init:string; var answer: DialogString);
begin
GetDialog('--passwordbox "'+quote(txt)+'" '+
          IntToStr(height)+' '+IntToStr(width)+' "'+quote(init)+'"', 
	  answer)
end;

procedure passwordbox_a(const txt: string; 
                        const init:string; var answer: DialogString);
begin
passwordbox(txt, autosize, autosize, init, answer)
end;

procedure addmenuitem(var menuitems: DialogString; tag, item: string);
begin
menuitems := menuitems + ' "' + quote(tag) + '" "'+quote(item) + '"'
end;

procedure addlistitem(var listitems: DialogString; tag, item: string; 
                      status: boolean);
begin
addmenuitem(listitems, tag, item);
if status 
  then listitems := listitems + ' on'
  else listitems := listitems + ' off'
end;

procedure menu(const txt: string; height, width, menuheight: integer; 
               const menuitems: string; var answer: DialogString);
begin
GetDialog('--menu "'+quote(txt)+'" '+
          IntToStr(height)+' '+IntToStr(width)+' '+
	  IntToStr(menuheight)+' '+menuitems, 
	  answer)
end;

procedure menu_a(const txt: string; menuheight: integer;
                 const menuitems: string; var answer: DialogString);
begin
menu(txt, autosize, autosize, menuheight, menuitems, answer)
end;

procedure checklist(const txt: string; height, width, listheight: integer; 
                    const listitems: string; var answer: DialogString);
begin
GetDialog('--checklist "'+quote(txt)+'" '+
          IntToStr(height)+' '+IntToStr(width)+' '+
	  IntToStr(listheight)+' '+listitems, 
	  answer)
end;

procedure checklist_a(const txt: string; listheight: integer;
                      const listitems: string; var answer: DialogString);
begin
checklist(txt, autosize, autosize, listheight, listitems, answer)
end;

procedure radiolist(const txt: string; height, width, listheight: integer; 
                    const listitems: string; var answer: DialogString);
begin
GetDialog('--radiolist "'+quote(txt)+'" '+
          IntToStr(height)+' '+IntToStr(width)+' '+
	  IntToStr(listheight)+' '+listitems, 
	  answer)
end;

procedure radiolist_a(const txt: string; listheight: integer;
                      const listitems: string; var answer: DialogString);
begin
radiolist(txt, autosize, autosize, listheight, listitems, answer)
end;


{ non standard }

procedure fselect(const default: string; height, width: integer; 
                  var answer: DialogString);
begin
GetDialog('--fselect "' + quote(default) + '" '+
          IntToStr(height)+' '+IntToStr(width), 
	  answer)
end;

procedure fselect_a(const default: string;
                    var answer: DialogString);
begin
fselect(default, autosize, autosize, answer)
end;

procedure dselect(const default: string; height, width: integer; 
                  var answer: DialogString);
begin
GetDialog('--dselect "' + quote(default) + '" '+
          IntToStr(height)+' '+IntToStr(width), 
	  answer)
end;

procedure dselect_a(const default: string;
                    var answer: DialogString);
begin
dselect(default, autosize, autosize, answer)
end;

end.
