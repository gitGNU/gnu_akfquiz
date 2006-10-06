{
* sdlgrph (unit)
* some graph functions with SDL
*
* $Id: sdlgrph.pas,v 1.10 2006/10/06 11:32:10 akf Exp $
*
* Copyright (c) 2005-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
* Copyright (c) 1997-2004 Sam Lantinga
*
* Environment: GNU Pascal or Free Pascal
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
  {$PackRecords 4}
{$EndIf}

{$X+}

unit sdlgrph;

interface
uses uakfquiz, qsys;

{$I hginfo.inc}

type TscreenPos = Integer;

procedure initializeGraphicMode(const title, short: string; 
                                fullscreen: boolean);
procedure endGraphics;
procedure LockScreen;
procedure UnlockScreen;
procedure setColors(foreground, background: Uint16);
procedure PutPixel(x, y: TscreenPos; color: Uint16);
function  GetPixel(x, y: TscreenPos): Uint16;
procedure showimage(x, y: Integer; const img);
procedure drawBackground(const img);
procedure MoveTo(x, y: TscreenPos);
function  GetX: TscreenPos;
function  GetY: TscreenPos;
function  GetMaxX: TscreenPos;
function  GetMaxY: TscreenPos;
function  GetRGBColor(r, g, b: byte): Uint16;

procedure DefineTextArea(x1, y1, x2, y2: TscreenPos; useTextArea: boolean);
procedure ClearTextArea;

procedure setExitKey(c: char);
function GetKey: char;

procedure showmouse(on: boolean);
procedure answerStarts(ans: word);
procedure answerEnds(ans: word);


implementation

{$IfDef __GPC__}
  {$DEFINE cdecl attribute(cdecl)}
  {$L SDL}
{$EndIf}

{$I mainicon.inc}


type 
  PScrMap = ^TScrMap;
  TScrMap = array[0..ScreenHeight-1, 0..ScreenWidth-1] of Uint16;

type 
  Tgrfimage = record
              Width    : Sint32;
              Height   : Sint32;
              reserved : Sint32;
              Image    : array[0..ScreenWidth*ScreenHeight] of Uint16
              end;

{ --------------------------------------------------------------------- }

type SDL_Bool = LongBool;

type
  pSDL_Color = ^SDL_Color;
  SDL_Color = record r, g, b, unused : Uint8 end;

type 
  pSDL_Rect = ^SDL_Rect;
  SDL_Rect = record x, y : Sint16; w, h : Uint16 end;

type
  pSDL_PixelFormat = ^SDL_PixelFormat;
  SDL_PixelFormat = 
    record
      palette : pointer;
      BitsPerPixel,
      BytesPerPixel : Uint8;
      Rloss, Gloss, Bloss, Aloss, 
      Rshift, Gshift, Bshift, Ashift : Uint8;
      Rmask, Gmask, Bmask, Amask : Uint32;
      colorkey : Uint32;
      alpha : Uint8
      end;

type
  pSDL_Surface = ^SDL_Surface;
  SDL_Surface = 
    record
      flags : Uint32;
      format : ^SDL_PixelFormat;
      w, h : CInteger;
      pitch : Uint16;
      pixels : PScrMap;
      offset : CInteger;
      hwdata : pointer; 
      clip_rect: SDL_Rect;
      unused1: Uint32;
      locked: Uint32;
      map : pointer;
      format_version: CInteger;
      refcount : CInteger
      end;


type SDL_keysym = 
       record
       scancode : Uint8;
       sym : CInteger;
       modifier : CInteger;
       unicode : Uint16
       end;

type SDL_KeyboardEvent = 
       record
       eventtype : Uint8;
       which : Uint8;
       state : Uint8;
       keysym : SDL_keysym
       end;

type SDL_MouseMotionEvent = 
       record
       eventtype : Uint8;
       which : Uint8;
       state : Uint8;
       x, y : Uint16;
       xrel : Sint16;
       yrel : Sint16
       end;

type SDL_MouseButtonEvent = 
       record
       eventtype : Uint8;
       which : Uint8;
       button : Uint8;
       state : Uint8;
       x, y : Uint16
       end;

type 
  PSDL_Event = ^SDL_Event;
  SDL_Event = 
    record
      case longint of
         0 : ( eventtype : Uint8 );
         2 : ( key : SDL_KeyboardEvent );
         3 : ( motion : SDL_MouseMotionEvent );
         4 : ( button : SDL_MouseButtonEvent )
        end;

const
  SDL_QUERY = -1;
  SDL_IGNORE = 0;
  SDL_DISABLE = 0;
  SDL_ENABLE = 1;

const
  SDL_NOEVENT=0;
  SDL_EVENTACTIVE=1;
  SDL_KEYDOWN=2;
  SDL_KEYUP=3;
  SDL_MOUSEMOTION=4;
  SDL_MOUSEBUTTONDOWN=5;
  SDL_MOUSEBUTTONUP=6;
  SDL_EVENTQUIT=12;

const 
  SDL_INIT_VIDEO = $00000020;
  SDL_SWSURFACE  = $00000000;
  SDL_HWSURFACE  = $00000001;
  SDL_ASYNCBLIT  = $00000004;
  SDL_RLEACCEL   = $00004000;
  SDL_FULLSCREEN = $80000000;
  SDL_BUTTON_LEFT = 1;
  SDL_BUTTON_MIDDLE = 2;
  SDL_BUTTON_RIGHT = 3;
  SDL_SRCCOLORKEY = $00001000;

{ --------------------------------------------------------------------- }

const MaxAnswers = 35;
  
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

{ --------------------------------------------------------------------- }

function SDL_Init(flags: Uint32): CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_Init';
	   
procedure SDL_Quit; cdecl; external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_Quit';

function SDL_SetVideoMode(width, height, bpp: CInteger; 
                          flags: Uint32): pSDL_Surface; cdecl;
	   external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_SetVideoMode';

function SDL_ShowCursor(toggle: CInteger): CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_ShowCursor';

function SDL_EnableUNICODE(enable: CInteger): CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_EnableUNICODE';

function SDL_LockSurface(surface : pSDL_Surface ): CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_LockSurface';

procedure SDL_UnlockSurface(surface: pSDL_Surface); cdecl;
            external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_UnlockSurface';

function SDL_CreateRGBSurface(flags: Uint32; width, height, depth: CInteger; 
                             Rmask, Gmask, Bmask, Amask: Uint32):
			       pSDL_Surface; cdecl;
	   external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_CreateRGBSurface';

function SDL_CreateRGBSurfaceFrom(pixels: pointer; 
              width, height, depth, pitch: CInteger;
              Rmask, Gmask, Bmask, Amask: Uint32): pSDL_Surface; cdecl; 
		external {$IfDef FPC}'SDL'{$EndIf} 
		  name 'SDL_CreateRGBSurfaceFrom';

function SDL_SetColors(surface: pSDL_Surface; 
                       colors: pointer; 
                       firstcolor: CInteger; ncolors: CInteger): CInteger;
           cdecl; external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_SetColors';

function SDL_SetColorKey(surface: pSDL_Surface; flag, key: Uint32):
           CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_SetColorKey';

procedure SDL_WM_SetIcon(icon: pSDL_Surface; mask: pByte); cdecl;
            external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_WM_SetIcon';

procedure SDL_WM_SetCaption(title, icon: CString); cdecl;
            external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_WM_SetCaption';

function SDL_MUSTLOCK(surface: pSDL_Surface): boolean;
begin
SDL_MUSTLOCK := 
  (surface^.offset<>0) or 
    ((surface^.flags and (SDL_HWSURFACE or SDL_ASYNCBLIT or SDL_RLEACCEL))<>0)
end;

procedure SDL_FreeSurface(surface:pSDL_Surface); cdecl;
            external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_FreeSurface';

function SDL_MapRGB(format: pSDL_PixelFormat; r, g, b: Uint8): Uint32; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_MapRGB';

function SDL_BlitSurface(src: pSDL_Surface; srcrect: pSDL_Rect; 
                         dst: pSDL_Surface; dstrect: pSDL_Rect): CInteger;
           cdecl; 
	   external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_UpperBlit'; { sic }
           
function SDL_SetClipRect(surface: pSDL_Surface; rect: pSDL_Rect): SDL_Bool;
           cdecl; external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_SetClipRect';

procedure SDL_UpdateRect(screen: pSDL_Surface; x, y: Sint32; 
                         w, h: Uint32); cdecl;
	    external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_UpdateRect';

function SDL_EventState(eventtype: Uint8; state: CInteger): Uint8; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_EventState';

function SDL_WaitEvent(event:pSDL_Event): CInteger; cdecl;
           external {$IfDef FPC}'SDL'{$EndIf} name 'SDL_WaitEvent';

{ --------------------------------------------------------------------- }

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

{$IfDef FPC}
  SDL_WM_SetCaption(CString(title + ' (SDL)'), CString(short));
{$Else}
  SDL_WM_SetCaption(title + ' (SDL)', short);
{$EndIf}

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

procedure setColors(foreground, background: Uint16);
begin end;

function GetRGBColor(r, g, b: byte): Uint16;
begin
GetRGBColor := Uint16(SDL_MapRGB(screen^.format, r, g, b))
end;

procedure PutPixel(x, y: TscreenPos; color: Uint16);
begin
x := x + taRect.x;
y := y + taRect.y;
if (x<=taMaxX) and (y<=taMaxY) then
  screen^.pixels^[y, x] := color
end;

function GetPixel(x, y: TscreenPos): Uint16;
begin
GetPixel := screen^.pixels^[y+taRect.y, x+taRect.x]
end;

procedure showimage(x, y: TScreenPos; const img);
var 
  image : pSDL_Surface;
  rect: SDL_Rect;
begin
image := SDL_CreateRGBSurfaceFrom(addr(Tgrfimage(img).Image), 
                                  Tgrfimage(img).Width, 
                                  Tgrfimage(img).Height,
                                  16,
                                  Tgrfimage(img).Width*SizeOf(Uint16),
                                  $F800, $07E0, $001F, $0000);
if image=NIL then exit;
rect.x := x+taRect.x;
rect.y := y+taRect.y;

SDL_BlitSurface(image, NIL, screen, addr(rect));
SDL_FreeSurface(image)
end;

procedure drawBackground(const img);
var image : pSDL_Surface;
begin
image := 
  SDL_CreateRGBSurfaceFrom(addr(Tgrfimage(img).Image), 
                           Tgrfimage(img).Width, 
                           Tgrfimage(img).Height,
                           16,
                           Tgrfimage(img).Width*SizeOf(Uint16),
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
const
  SDLK_F4       = 285;
  SDLK_F11      = 292;
  SDLK_KP0      = 256;
  SDLK_KP_Enter = 271;
  SDLK_EURO     = 321;
  SDLK_RETURN   = 13;
const 
  KMOD_CTRL = $0040 or $0080;
  KMOD_ALT  = $0100 or $0200;
  KMOD_CTRLALT = KMOD_CTRL or KMOD_ALT;
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
        $0178 : c := chr($BE) { Y with diaeresis }
        end;

  if unicode=0 then
     case sym of
       SDLK_F4,  { F4, F11 - known conventions from some other programs }
       SDLK_F11 : switchfullscreen;
       SDLK_KP0 : c := '0';
       (SDLK_KP0+1) .. (SDLK_KP0+9) : 
                  c := chr(ord('1') + sym - SDLK_KP0 - 1);
       SDLK_KP_Enter : c := chr(13);
       SDLK_EURO : c:=chr($A4) { ISO-8859-15 }
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

var ident : ShortString;

Initialization

  ident := '$Id: sdlgrph.pas,v 1.10 2006/10/06 11:32:10 akf Exp $';

  textarea      := NIL;
  screen        := NIL;
  icon          := NIL;
  exitkey       := chr(27);
  mustlock      := false;
  graphicActive := false;

Finalization

  endGraphics

end.
