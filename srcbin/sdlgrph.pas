{
* sdlgrph (unit)
* some graph functions with SDL
*
* $Id: sdlgrph.pas,v 1.3 2006/08/27 06:47:35 akf Exp $
*
* Copyright (c) 2005-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: FreePascal and SDL4FreePascal
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
{$EndIf}

{$X+}

unit sdlgrph;

interface
uses uakfquiz, sdl, sdl_video, sdl_events, sdl_keyboard, sdl_mouse;

{$I hginfo.inc}

type TscreenPos = Integer;

const MaxAnswers = 35;

procedure initializeGraphicMode(const title, short: string; 
                                fullscreen: boolean);
procedure endGraphics;
procedure LockScreen;
procedure UnlockScreen;
procedure setColors(foreground, background: word);
procedure PutPixel(x, y: TscreenPos; color: word);
function  GetPixel(x, y: TscreenPos): word;
procedure showimage(x, y: Integer; var img);
procedure drawBackground(var img);
procedure MoveTo(x, y: TscreenPos);
function  GetX: TscreenPos;
function  GetY: TscreenPos;
function  GetMaxX: TscreenPos;
function  GetMaxY: TscreenPos;
function  GetRGBColor(r, g, b: byte): word;

procedure DefineTextArea(x1, y1, x2, y2: TscreenPos; useTextArea: boolean);
procedure ClearTextArea;

procedure setExitKey(c: char);
function GetKey: char;

procedure showmouse(on: boolean);
procedure answerStarts(ans: word);
procedure answerEnds(ans: word);


implementation

{$I mainicon.inc}

type TScrMap = array[0..ScreenHeight-1, 0..ScreenWidth-1] of word;

type 
  Tgrfimage = record
              Width    : longint;
              Height   : longint;
              reserved : longint;
              Image    : array[0..ScreenWidth*ScreenHeight] of word
              end;

var answerposition: array[1..MaxAnswers] of record f, t: TscreenPos end;

var screen, textarea, icon: pSDL_Surface;
var taRect: SDL_Rect;
var taMaxX, taMaxY: TscreenPos;
var xPos, yPos : TscreenPos;
var mode : LongInt;
var mustlock: Boolean;
var graphicActive: Boolean;
var ExitKey : char;
var mouseactive, mouseshown: boolean;

procedure resetAnswerPositions;
var i: integer;
begin
for i:=1 to MaxAnswers do
    begin
    answerposition[i].f := -1;
    answerposition[i].t := -1
    end
end;

procedure LockScreen;
begin
if mustlock then SDL_LockSurface(screen)
end;

procedure UnlockScreen;
begin
if mustlock then SDL_UnlockSurface(screen)
end;

procedure answerStarts(ans: word);
begin
answerposition[ans].f := ypos
end;

procedure answerEnds(ans: word);
begin 
answerposition[ans].t := ypos
end;

procedure showmouse(on: boolean);
begin
mouseshown := on;
if mouseshown 
  then begin SDL_ShowCursor(SDL_ENABLE); mouseactive := true end
  else SDL_ShowCursor(SDL_DISABLE)
end;

function SDL_SetColors(surface:pSDL_Surface; 
                       colors:pSDL_Color; 
                       firstcolor:longint; 
		       ncolors:longint):longint;
		       cdecl;external 'SDL';

procedure createIcon;
var 
  colormap : array[32..127] of SDL_Color;
  i : integer;
begin { createIcon }

icon := SDL_CreateRGBSurfaceFrom(addr(icon_AKFQuiz), 32, 32, 8, 32, 
                                 0, 0, 0, 0);
if icon=NIL then 
   begin
   WriteLn(stderr, 'grquiz error: error creatin the icon');
   Halt(1)
   end;

for i := 1 to icon_AKFQuiz_colornum do
  begin
  colormap[icon_AKFQuiz_colors[i, 0]].r := icon_AKFQuiz_colors[i, 1];
  colormap[icon_AKFQuiz_colors[i, 0]].g := icon_AKFQuiz_colors[i, 2];
  colormap[icon_AKFQuiz_colors[i, 0]].b := icon_AKFQuiz_colors[i, 3]
  end;

SDL_SetColors(icon, addr(colormap), 32, 127-32);

{ set space-char as being transparent }
SDL_SetColorKey(icon, SDL_SRCCOLORKEY, 32);

SDL_WM_SetIcon(icon, NIL)
end;


procedure initializeGraphicMode(const title, short: string; 
                                fullscreen: boolean);
begin
xPos := 0;
yPos := 0;

taMaxX := ScreenWidth;
taMaxY := ScreenHeight;

with taRect do
  begin
  x := 0;
  y := 0;
  w := taMaxX;
  h := taMaxY
  end;

if SDL_Init(SDL_INIT_VIDEO)<0 then
   begin
   WriteLn(stderr, 'grquiz error: cannot open video output');
   Halt(1)
   end;

SDL_WM_SetCaption(PChar(title + ' (SDL)'), PChar(short));

createIcon;

mode := SDL_SWSURFACE;
if fullscreen then mode := mode or SDL_FULLSCREEN;

screen := SDL_SetVideoMode(ScreenWidth, ScreenHeight, 
                           ScreenDepth, mode);
if screen=NIL then 
   begin
   WriteLn(stderr, 'grquiz error: graphic mode not supported');
   Halt(1)
   end;

graphicActive := true;

mustlock := SDL_MustLock(screen);

showmouse(true);

{ ignore some events }
SDL_EventState(SDL_MOUSEMOTION,   SDL_IGNORE);
SDL_EventState(SDL_MOUSEBUTTONUP, SDL_IGNORE);
SDL_EventState(SDL_KEYUP,         SDL_IGNORE);
SDL_EventState(SDL_EVENTACTIVE,   SDL_IGNORE);

{ enable unicode handling }
SDL_EnableUNICODE(1)
end;

procedure setExitKey(c: char);
begin
ExitKey := c
end;

procedure endGraphics;
begin
if graphicActive then
  begin
  if textarea<>NIL then SDL_FreeSurface(textarea);
  if icon<>NIL     then SDL_FreeSurface(icon);
  { the screen is freed by SDL_Quit }
  {if screen<>NIL   then SDL_FreeSurface(screen);}
  SDL_Quit;
  textarea := NIL;
  icon := NIL;
  screen := NIL;
  graphicActive := false
  end
end;

procedure setColors(foreground, background: word);
begin end;

function GetRGBColor(r, g, b: byte): word;
begin
GetRGBColor := word(SDL_MapRGB(screen^.format, r, g, b))
end;

procedure PutPixel(x, y: TscreenPos; color: word);
begin
x := x + taRect.x;
y := y + taRect.y;
if (x<=taMaxX) and (y<=taMaxY) then
  TscrMap(screen^.pixels^)[y, x] := color
end;

function GetPixel(x, y: TscreenPos): word;
begin
GetPixel := TscrMap(screen^.pixels^)[y+taRect.y, x+taRect.x]
end;

procedure showimage(x, y: TScreenPos; var img);
var 
  image : pSDL_Surface;
  rect: SDL_Rect;
begin
image := SDL_CreateRGBSurfaceFrom(addr(Tgrfimage(img).Image), 
                                  Tgrfimage(img).Width, 
                                  Tgrfimage(img).Height,
                                  16,
                                  Tgrfimage(img).Width*SizeOf(word),
                                  $F800, $07E0, $001F, $0000);
if image=NIL then exit;
rect.x := x+taRect.x;
rect.y := y+taRect.y;

SDL_BlitSurface(image, NIL, screen, addr(rect));
SDL_FreeSurface(image);
end;

procedure drawBackground(var img);
var image : pSDL_Surface;
begin
image := 
  SDL_CreateRGBSurfaceFrom(addr(Tgrfimage(img).Image), 
                           Tgrfimage(img).Width, 
                           Tgrfimage(img).Height,
                           16,
                           Tgrfimage(img).Width*SizeOf(word),
                           $F800, $07E0, $001F, $0000);
if image=NIL then exit;

SDL_BlitSurface(image, NIL, screen, NIL);
SDL_UpdateRect(screen, 0, 0, 0, 0);
SDL_FreeSurface(image)
end;

procedure MoveTo(x, y: TscreenPos);
begin
xPos := x;
yPos := y
end;

function GetX: TscreenPos;
begin
GetX := xPos
end;

function  GetY: TscreenPos;
begin
GetY := yPos
end;

function GetMaxX: TscreenPos;
begin
GetMaxX := ScreenWidth
end;

function  GetMaxY: TscreenPos;
begin
GetMaxY := ScreenHeight
end;

procedure defineTextArea(x1, y1, x2, y2: TscreenPos; useTextArea: boolean);
begin
{ needed to speedup PutPixel }
taMaxX := x2;
taMaxY := y2;

with taRect do
  begin
  x := x1;
  y := y1;
  w := taMaxX-x1+1;
  h := taMaxY-y1+1
  end;

if textarea<>NIL then SDL_FreeSurface(textarea);
textarea := SDL_CreateRGBSurface(SDL_SWSURFACE, 
                                 taRect.w, taRect.h,
                                 ScreenDepth,
                                 screen^.format^.rmask,
                                 screen^.format^.gmask,
                                 screen^.format^.bmask,
                                 screen^.format^.amask);

SDL_BlitSurface(screen, addr(taRect), textarea, NIL);
SDL_SetClipRect(screen, addr(taRect));
resetAnswerPositions
end;

procedure ClearTextArea;
begin
{ I could use the background image to copy that part from it,
  but that slows things down, since the color-mode must possibly 
  be converted }

SDL_BlitSurface(textarea, NIL, screen, addr(taRect));

xPos := 0;
yPos := 0;
resetAnswerPositions
end;

procedure switchfullscreen;
var oldscreen: pSDL_Surface;
begin
oldscreen := SDL_CreateRGBSurface(SDL_SWSURFACE, 
                                  ScreenWidth, ScreenHeight,
                                  ScreenDepth,
                                  screen^.format^.rmask,
                                  screen^.format^.gmask,
                                  screen^.format^.bmask,
                                  screen^.format^.amask);

SDL_BlitSurface(screen, NIL, oldscreen, NIL);

mode := mode xor SDL_FULLSCREEN;
SDL_FreeSurface(screen);
screen := SDL_SetVideomode(ScreenWidth, ScreenHeight, 
                           ScreenDepth, mode);
SDL_BlitSurface(oldscreen, NIL, screen, NIL);
SDL_UpdateRect(screen, 0, 0, 0, 0);
SDL_SetClipRect(screen, addr(taRect));
mustlock := SDL_MustLock(screen);

SDL_FreeSurface(oldscreen)
end;

function GetMouseAnswer(const mouse): char;
const defaultanswer = chr(06); { 06 = ACK: no answer - but it's "any key" }
var 
  x, y : LongInt;
  button: byte;
  i : integer;
  answer: char;
begin
x := SDL_MouseButtonEvent(mouse).x - taRect.x;
y := SDL_MouseButtonEvent(mouse).y - taRect.y;
button := SDL_MouseButtonEvent(mouse).button;
answer := defaultanswer;

{ left mouse button in textarea? }
if (button=SDL_BUTTON_LEFT) and 
   (x>=0) and (x<=taRect.w) and (y>=0) and (y<=taRect.h) then
     begin
     i := 1;
     repeat
       if (y>=answerposition[i].f) and (y<=answerposition[i].t)
          then answer := ValueToKey(i);
       inc(i)
     until (answer<>defaultanswer) or (i>MaxAnswers);
     end;

{ right mouse button = Enter }
if button=SDL_BUTTON_RIGHT then answer := chr(13);

{ if none of the first 3 buttons pressed }
if button>3 then answer:=#0;

GetMouseAnswer := answer
end;

function getKeyboardAnswer(var event: SDL_Event): char;
const KMOD_CTRLALT=KMOD_CTRL or KMOD_ALT;
var c: char;
begin
c := chr(0); { interpreted as no valid key pressed }

with SDL_KeyboardEvent(event).keysym do
  begin
  if unicode<=255
    then c := chr(unicode)
    else { unicode to ISO-8859-15 }
      case unicode of
        $20AC : c := chr($A4); { euro }
        $0160 : c := chr($A6); { S with caron }
        $0161 : c := chr($A8); { s with caron } { missing in the font }
        $017D : c := chr($B4); { Z with caron }
        $017E : c := chr($B8); { z with caron }
        $0152 : c := chr($BC); { ligature OE }
        $0153 : c := chr($BD); { ligature oe }
        $0178 : c := chr($BE); { Y with diaeresis }
        end;

  if unicode=0 then
     case sym of
       SDLK_F4,  { F4, F11 - known conventions from some other programs }
       SDLK_F11 : switchfullscreen;
       SDLK_KP0 : c := '0';
       SDLK_KP1 : c := '1';
       SDLK_KP2 : c := '2';
       SDLK_KP3 : c := '3';
       SDLK_KP4 : c := '4';
       SDLK_KP5 : c := '5';
       SDLK_KP6 : c := '6';
       SDLK_KP7 : c := '7';
       SDLK_KP8 : c := '8';
       SDLK_KP9 : c := '9';
       SDLK_KP_Enter : c := chr(13);
       SDLK_EURO : c:=chr($A4); { ISO-8859-15 }
       end;

  { Alt + Enter => switchfullscreen }
  if (((sym=SDLK_RETURN) or (sym=SDLK_KP_Enter)) and 
     ((modifier and KMOD_ALT)<>0)) then
     begin
     switchfullscreen;
     c := chr(0)
     end
  end; { with }

getKeyboardAnswer := c
end;

function GetKey: char;
var 
 event: SDL_Event;
 c: char;
begin
{ the textarea is only updated here, when the program waits for events }
with taRect do SDL_UpdateRect(screen, x, y, w, h);

c := chr(0);
repeat
  SDL_WaitEvent(addr(event));
  case event.eventtype of
    SDL_EventQuit: c:=ExitKey;
    SDL_MouseButtonDown: if mouseactive then c := GetMouseAnswer(event);
    SDL_KeyDown: c := getKeyboardAnswer(event)
    end { case event.eventtype }
until c<>chr(0);
GetKey := c
end;

Initialization

  textarea      := NIL;
  screen        := NIL;
  icon          := NIL;
  exitkey       := chr(27);
  mustlock      := false;
  graphicActive := false;

Finalization

  endGraphics;

end.
