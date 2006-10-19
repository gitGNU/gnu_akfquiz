{
* qmsgs (unit)
*
* $Id: qmsgs.pas,v 1.18 2006/10/19 17:54:44 akf Exp $
*
* Copyright (c) 2003-2005 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Thanks to
* Martin Guy for italiano
* Tommy Jensen for dansk
*
* This file contains characters in UTF-8 - keep that encoding
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
  {$Smartlink on}
{$EndIf}

{ compatiblity definition }
{$IfDef _WIN32} {$Define Win32} {$EndIf}


unit qmsgs;

interface
uses qsys;

type languages = (deutsch, english, italiano, dansk);

var lang : languages = english;
var msgRTL : boolean = false;

function msg_homepage: mystring;
function msg_GPL: mystring;
function msg_noWarranty: mystring;
function msg_contributions: mystring;
function msg_advertisement: mystring;
function msg_noquizfound: mystring;
function msg_filenotfound: mystring;
function msg_anykey(const key: string): mystring;
function msg_quiz: mystring;
function msg_author: mystring;
function msg_authorURI: mystring;
function msg_translator: mystring;
function msg_edited: mystring;
function msg_license: mystring;
function msg_licenseURI: mystring;
function msg_right: mystring;
function msg_wrong: mystring;
function msg_points: mystring;
function msg_seen: mystring;
function msg_really: mystring;
function msg_sol1: mystring;
function msg_sol2: mystring;
function msg_sol3: mystring;
function msg_sol4: mystring;
function msg_sol5: mystring;
function msg_timeout: mystring;
function msg_time: mystring;
function msg_timelimit: mystring;
function msg_error: mystring;
function msg_fileerror: mystring;
function msg_result: mystring;
function msg_solution: mystring;
function msg_new: mystring;
function msg_made: mystring;
function msg_back: mystring;
function msg_noJS: mystring;
function msg_notusable: mystring;
function msg_assessment: mystring;
function msg_more: mystring;
function msg_Results: mystring;
function msg_showResults: mystring;
function msg_name: mystring;
function msg_passwd: mystring;
function msg_newpasswd: mystring;
function msg_reconfigure: mystring;
function msg_login: mystring;
function msg_loggedin: mystring;
function msg_logout: mystring;
function msg_loggedout: mystring;
function msg_inconsistent: mystring;
function msg_view: mystring;

procedure setmsgconverter(p: Tconverter);
procedure setmsgconv(display: DisplayType);
procedure useSystemLanguage;


Implementation

var cnv: Tconverter = noconversion;

{ Translators: }
{ if you need an apostrophe, use it twice like this: 'That''s it' }
{ use the function cnv if and only if you use non-ASCII chars }


{ Address, where to get the source code }
{ When you make changes, please provide the address where your version
  of source-code is published }
function msg_homepage: mystring;
begin
case lang of
  deutsch : msg_homepage := 'http://akfquiz.nongnu.org/index.de.html'
  otherwise msg_homepage := 'http://akfquiz.nongnu.org/'
  end
end;

function msg_GPL: mystring;
begin

{ You may not change the license,
  but you may relicense it to a newer version of the GPL, 
  when a newer version is officially released }

case lang of
  italiano: msg_GPL := cnv('GPL v2 o version più recente');
  deutsch : msg_GPL := 'GPL V2 oder neuer'
  otherwise msg_GPL := 'GPL v2 or later'
  end
end;

function msg_noWarranty: mystring;
begin
case lang of
  italiano:  msg_noWarranty := cnv(
    'Questo programma è distribuito SENZA ALCUNA GARANZIA' + nl
    + 'per quanto ciò sia consentito dalla legge.' + nl
    + 'È lecito redistribuirlo secondo i termini della' + nl
    + 'GNU General Public License;' + nl
    + 'per dettagli vedere la cartella COPYING.' + nl
    + 'Gli archivi dei Quiz non sono coperte da questa licenza');
  deutsch : msg_noWarranty := cnv(
    'Dieses Programm wird ohne Gewährleistung geliefert, ' + nl 
    + 'soweit dies gesetzlich zulässig ist.' + nl 
    + 'Sie können es unter den Bedingungen der' + nl
    + 'GNU General Public License weitergeben.'+ nl 
    + 'Details dazu enthält die Datei COPYING.' + nl
    + nl
    + 'Quiz-Dateien sind von dieser Lizenz nicht betroffen.')
  otherwise msg_noWarranty := 
    'This program comes with NO WARRANTY,' + nl
    + 'to the extent permitted by law.' + nl
    + 'You may redistribute it under the terms of the ' + nl
    + 'GNU General Public License;' + nl
    + 'see the file named COPYING for details.' + nl
    + nl
    + 'Quiz-files are not affected by this license.'
  end
end;

function msg_contributions: mystring;
begin
case lang of
  italiano : msg_contributions := 'Contribuenti:';
  deutsch  : msg_contributions := cnv('Beiträge:')
  otherwise  msg_contributions := 'Contributions:'
  end
end;

function msg_advertisement: mystring;
begin
{ may be used for other advertisements }
{ just one line! }

{$IfDef Advertisement}
  case lang of
    deutsch  : msg_advertisement := 
        cnv('Das original AKFQuiz ist für GNU/Linux erhältlich');
    italiano :  msg_advertisement := 
        cnv('La versione originale di AKFQuiz è disponibile per GNU/Linux');
    dansk    :  msg_advertisement := 
        'Den oprindelige AKFQuiz findes til GNU/Linux'
    otherwise  msg_advertisement := 
        'The original of AKFQuiz is available for GNU/Linux'
    end
{$Else}
  msg_advertisement := '' { to reduce the size of the binary  }
{$EndIf}
end;

function msg_noquizfound: mystring;
begin
case lang of
  deutsch : msg_noquizfound := 'Keine AKFQuiz-Dateien gefunden';
  italiano: msg_noquizfound := 'Nessun archivio AKFQuiz trovato';
  dansk   : msg_noquizfound := 'Ingen AKFQuiz filer fundet'
  otherwise msg_noquizfound := 'No AKFQuiz-files found'
  end
end;

function msg_filenotfound: mystring;
begin
case lang of 
  deutsch : msg_filenotfound := 'Fehler: Datei nicht gefunden';
  italiano: msg_filenotfound := 'Errore: Archivio non trovato';
  dansk   : msg_filenotfound := 'Fejl: Fil ikke fundet'
  otherwise msg_filenotfound := 'Error: file not found'
  end
end;

function msg_anykey(const key: string): mystring;
begin
case lang of
  deutsch : 
    if key='' 
      then msg_anykey := 'beliebige Taste...'
      else msg_anykey := 
             'beliebige Taste zum Fortfahren, oder '+key+' zum Beenden...';
  italiano : 
    if key='' 
      then msg_anykey := 'Premere un tasto...'
      else msg_anykey := 
            'Premere un tasto per continuare o '+key+' per uscire...';
  dansk :
    if key='' 
      then msg_anykey := cnv('Tryk på en tast...')
      else msg_anykey := cnv('Tryk på en tast for at fortsætte eller '+
                             key+' for at stoppe...')
  otherwise if key='' 
              then msg_anykey := 'press any key to continue...'
              else msg_anykey := 
	           'press any key to continue or '+key+' to stop...'
  end
end;

function msg_quiz: mystring;
begin
case lang of
  deutsch,
  italiano,
  dansk    : msg_quiz := 'Quiz: '
  otherwise  msg_quiz := 'Quiz: '
  end
end;

function msg_author: mystring;
begin
case lang of
  deutsch  : msg_author := 'Autor: ';
  italiano : msg_author := 'Autore: ';
  dansk    : msg_author := 'Forfatter: '
  otherwise  msg_author := 'Author: '
  end
end;

function msg_authorURI: mystring;
begin
case lang of
  deutsch  : msg_authorURI := 'Autor-Link: ';
  italiano : msg_authorURI := 'Autore-link: ';
  dansk    : msg_authorURI := 'Forfatter-Link: '
  otherwise  msg_authorURI := 'Author-link: '
  end
end;

function msg_translator: mystring;
begin
case lang of
  deutsch  : msg_translator := cnv('Übersetzer: ');
  italiano : msg_translator := 'Traduttore: ';
  dansk    : msg_translator := cnv('Oversætter: ')
  otherwise  msg_translator := 'Translator: '
  end
end;

function msg_edited: mystring;
begin
case lang of
  italiano : msg_edited := 'Modificato da: ';
  deutsch  : msg_edited := cnv('Überarbeitet von: ')
  otherwise  msg_edited := 'Edited by: '
  end
end;

function msg_license: mystring;
begin
case lang of
  italiano : msg_license := 'Licenza: ';
  deutsch  : msg_license := 'Lizenz: '
  otherwise  msg_license := 'License: '
  end
end;

function msg_licenseURI: mystring;
begin
case lang of
  italiano : msg_licenseURI := 'Licenza-link: ';
  deutsch  : msg_licenseURI := 'Lizenz-Link: '
  otherwise  msg_licenseURI := 'License-Link: '
  end
end;

function msg_right: mystring;
begin
case lang of
  deutsch  : msg_right := 'richtig';
  italiano : msg_right := 'giusto';
  dansk    : msg_right := 'rigtigt'
  otherwise  msg_right := 'right'
  end
end;
	 
function msg_wrong: mystring;
begin
case lang of
  deutsch  : msg_wrong := 'leider falsch';
  italiano : msg_wrong := 'sbagliato, mi dispiace';
  dansk    : msg_wrong := 'beklager, det er forkert'
  otherwise  msg_wrong := 'wrong, sorry'
  end
end;

function msg_points: mystring;
begin
case lang of
  deutsch  : msg_points := 'Punkte: ';
  italiano : msg_points := 'punti: ';
  dansk    : msg_points := 'point: ' {???}
  otherwise  msg_points := 'points: '
  end
end;
 
function msg_seen: mystring;
begin
case lang of
  deutsch  : msg_seen := cnv('Du hast die Lösung doch schon gesehen!');
  italiano : msg_seen := cnv('Ehi! Hai già visto la soluzione!');
  dansk    : msg_seen := cnv('Du har jo allerede set løsningen!')
  otherwise  msg_seen := 'Hey, you have seen the solution already!'
  end
end;

function msg_really: mystring;
begin
case lang of
  deutsch  : msg_really := cnv('Wirklich die Lösung zeigen?');
  italiano : msg_really := 'Vuoi vedere la soluzione davvero?';
  dansk    : msg_really := cnv('Ønsker du virkelig at se løsningen?')
  otherwise  msg_really := 'You really want to see the solution?'
  end
end;

function msg_sol1: mystring;
begin
case lang of
  deutsch  : msg_sol1 := 'Es wurden ';
  italiano : msg_sol1 := 'Hai guagadnato ';
  dansk    : msg_sol1 := cnv('Du har fået ')
  otherwise  msg_sol1 := 'You have gained '
  end
end;

function msg_sol2: mystring;
begin
case lang of
  deutsch  : msg_sol2 := ' von maximal ';
  italiano : msg_sol2 := ' dei ';
  dansk    : msg_sol2 := ' af '
  otherwise  msg_sol2 := ' of '
  end
end;

function msg_sol3: mystring;
begin
case lang of
  deutsch  : msg_sol3 := ' Punkten erreicht.';
  italiano : msg_sol3 := ' punti possibili.';
  dansk    : msg_sol3 := ' mulige point.'
  otherwise  msg_sol3 := ' possible points.'
  end
end;

function msg_sol4: mystring;
begin
case lang of
  deutsch  : msg_sol4 := 'Das sind ';
  italiano : msg_sol4 := cnv('Ciò fà ');
  dansk    : msg_sol4 := 'Det giver '
  otherwise  msg_sol4 := 'That makes '
  end
end;

function msg_sol5: mystring;
begin
case lang of
  deutsch  : msg_sol5 := 'Das sind leider nicht genug.';
  italiano : msg_sol5 := 'Mi dispiace, ma non basta.';
  dansk    : msg_sol5 := 'Beklager, det er ikke nok.'
  otherwise  msg_sol5 := 'Sorry, that''s not enough.'
  end
end;

function msg_timeout: mystring;
begin
case lang of
  italiano : msg_timeout := cnv('Il tempo è finito!');
  deutsch  : msg_timeout := 'Die Zeit ist abgelaufen!'
  otherwise  msg_timeout := 'The time is up!'
  end
end;

function msg_time: mystring;
begin
case lang of
  italiano : msg_time := 'Tempo trascorso: ';
  deutsch  : msg_time := cnv('Benötigte Zeit: ')
  otherwise  msg_time := 'Elapsed time: '
  end
end;

function msg_timelimit: mystring;
begin
case lang of
  italiano : msg_timelimit := 'Limite di tempo: ';
  deutsch  : msg_timelimit := 'Zeit-Limit: '
  otherwise  msg_timelimit := 'time limit: '
  end
end;


function msg_error: mystring;
begin
case lang of
  deutsch  : msg_error := 'Fehler';
  italiano : msg_error := 'Errore';
  dansk    : msg_error := 'Fejl'
  otherwise  msg_error := 'Error'
  end
end;

function msg_fileerror: mystring;
begin
case lang of
  deutsch  : msg_fileerror := 'Fehler in der Eingabe-Datei';
  italiano : msg_fileerror := 'Errore nell''archivio di input';
  dansk    : msg_fileerror := 'Fejl i input filen'
  otherwise  msg_fileerror := 'Error in input file'
  end
end;

function msg_result: mystring;
begin
case lang of
  deutsch  : msg_result := 'Auswertung';
  italiano : msg_result := 'risultato';
  dansk    : msg_result := 'resultat'
  otherwise  msg_result := 'result'
  end
end;

function msg_solution: mystring;
begin
case lang of
  deutsch  : msg_solution := cnv('Auflösung');
  italiano : msg_solution := 'soluzione';
  dansk    : msg_solution := cnv('løsning')
  otherwise  msg_solution := 'solution'
  end
end;

function msg_new: mystring;
begin
case lang of
  deutsch  : msg_new := 'neu';
  italiano : msg_new := 'nuovo';
  dansk    : msg_new := 'ny'
  otherwise  msg_new := 'new'
  end
end;

function msg_made: mystring;
begin
case lang of
  deutsch  : msg_made := 'erstellt mit';
  italiano : msg_made := 'creato con';
  dansk    : msg_made := 'skabt ved brug af' {???}
  otherwise  msg_made := 'made using'
  end
end;

function msg_back: mystring;
begin
case lang of
  deutsch  : msg_back := cnv('zurück');
  italiano : msg_back := 'indietro';
  dansk    : msg_back := 'tilbage'
  otherwise  msg_back := 'back'
  end
end;

function msg_noJS: mystring;
begin
case lang of
  deutsch  : 
    msg_noJS := cnv('JavaScript nicht verfügbar oder deaktiviert');
  italiano : 
    msg_noJS := cnv('JavaScript o non è disponibile o è disattivato');
  dansk    : msg_noJS := 'JavaScript ikke installeret eller deaktiveret'
  otherwise  msg_noJS := 'JavaScript not available or deactivated'
  end
end;

function msg_notusable: mystring;
begin
case lang of
  deutsch  : msg_notusable := 'Quiz nicht benutzbar!';
  italiano : msg_notusable := 'Quiz non utilizzabile!';
  dansk    : msg_notusable := 'Quizzen kan ikke bruges!'
  otherwise  msg_notusable := 'Quiz not usable!'
  end
end;

function msg_assessment: mystring;
begin
case lang of
  deutsch  : msg_assessment := 'Auswertung';
  italiano : msg_assessment := 'valutazione';
  dansk    : msg_assessment := cnv('bedømmelse')
  otherwise  msg_assessment := 'assessment'
  end
end;

function msg_more: mystring;
begin
case lang of
  deutsch : msg_more := 'mehr...';
  italiano: msg_more := 'ancora...';
  english : msg_more := 'more...'
  otherwise msg_more := '...'
  end
end;

function msg_Results: mystring;
begin
case lang of
  italiano: msg_Results := 'Risultati';
  deutsch : msg_Results := 'Ergebnisse'
  otherwise msg_Results := 'Results'
  end
end;

function msg_showResults: mystring;
begin
case lang of
  italiano: msg_showResults := 'Mostrare i risultati';
  deutsch : msg_showResults := 'Zeige Ergebnisse'
  otherwise msg_showResults := 'Show results'
  end
end;

function msg_name: mystring;
begin
case lang of
  italiano:  msg_name := 'Nome: ';
  deutsch :  msg_name := 'Name: '
  otherwise  msg_name := 'Name: '
  end
end;

function msg_passwd: mystring;
begin
case lang of
  italiano:  msg_passwd := 'Parola d''ordine';
  deutsch :  msg_passwd := 'Passwort'
  otherwise  msg_passwd := 'Password'
  end
end;

function msg_newpasswd: mystring;
begin
case lang of
  italiano:  msg_newpasswd := 'Nuova parola d''ordine';
  deutsch :  msg_newpasswd := 'Neues Passwort'
  otherwise  msg_newpasswd := 'New Password'
  end
end;

function msg_reconfigure: mystring;
begin
case lang of
  italiano:  msg_reconfigure := 'Riconfigurare';
  deutsch :  msg_reconfigure := 'Umkonfigurieren'
  otherwise  msg_reconfigure := 'Reconfigure'
  end
end;

function msg_login: mystring;
begin
case lang of
  italiano:  msg_login := 'Entrare come docente';
  deutsch :  msg_login := 'Als Lehrer anmelden'
  otherwise  msg_login := 'Log in as teacher'
  end
end;

function msg_loggedin: mystring;
begin
case lang of
  italiano:  msg_loggedin := 'Ingresso riuscito';
  deutsch :  msg_loggedin := 'Anmeldung erlogreich'
  otherwise  msg_loggedin := 'Login successful'
  end
end;

function msg_logout: mystring;
begin
case lang of
  italiano:  msg_logout := 'Uscire';
  deutsch :  msg_logout := 'Abmelden'
  otherwise  msg_logout := 'Log out'
  end
end;

function msg_loggedout: mystring;
begin
case lang of
  italiano:  msg_loggedout := 'Sconnesso';
  deutsch :  msg_loggedout := 'Abgemeldet'
  otherwise  msg_loggedout := 'Logged out'
  end
end;

function msg_inconsistent: mystring;
begin
case lang of
  italiano:  msg_inconsistent :=
        cnv('Il risultato non è uguale a quello originale');
  deutsch :  msg_inconsistent := 
        cnv('Das Ergebnis entspricht nicht dem ursprünglichen Ergebnis')
  otherwise  msg_inconsistent := 
          'The result is not the same as the original result'
  end
end;

function msg_view: mystring;
begin
case lang of
  { italiano:  msg_view := 'vedere'; }
  italiano:  msg_view := 'sfogliare';
  deutsch :  msg_view := 'anzeigen'
  otherwise msg_view := 'view'
  end
end;

procedure useSystemLanguage;
var l: string[2];
begin
l := copy(getSystemLanguage, 1, 2); { first 2 chars }
l := makeUpcase(l);
if l='EN' then lang := english;
if l='DE' then lang := deutsch;
if l='DA' then lang := dansk;
if l='IT' then lang := italiano
{ else unchanged }
end;

procedure setmsgconverter(p: Tconverter);
begin
cnv := p;
msgRTL := false
end;

procedure setmsgconv(display: DisplayType);
begin
case display of
  ISOdisplay:  setmsgconverter(UTF8toISO1);
  OEMdisplay:  setmsgconverter(UTF8toOEM);
  UTF8display: setmsgconverter(noconversion)
  end
end;


begin
ident('$Id: qmsgs.pas,v 1.18 2006/10/19 17:54:44 akf Exp $')
end.
