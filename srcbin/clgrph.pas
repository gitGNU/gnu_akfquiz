{
* clgrph (unit)
* functions for the classical graph unit for grquiz
*
* $Id: clgrph.pas,v 1.9 2006/12/10 17:09:49 akf Exp $
*
* Copyright (c) 2005-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
*
* Environment: 
* FreePascal
* GNU-Pascal and GRX library with GPC support (no other addon needed)
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

{$IfDef FPC} {$IfDef Unix}
  {$Define FPCSVGALIB} { Under FPC + Unix SVGAlib is used }
{$EndIf} {$EndIf}

{$I-}
{$X+}

unit clgrph;

interface

{$IfDef FPC}
  {$IfDef FPCSVGALIB}
    uses graph; { SVGAlib: another GetKey }
  {$Else}
    uses graph, crt; {$Define CRT}
  {$EndIf}
{$EndIf}

{$IfDef __GPC__}
  import graph; grx; {$IfDef GRXMouse} uakfquiz; {$EndIf}
{$EndIf}

{$I hginfo.inc}

{ should be compatible to what is used in the Graph unit }
type TscreenPos = {$IfDef FPC} smallInt; {$Else} Integer; {$EndIf}

procedure initializeGraphicMode(const title, short: string; 
                                fullscreen: boolean);
procedure LockScreen;
procedure UnlockScreen;
procedure setExitKey(ExitKey: char);
procedure answerStarts(ans: cardinal);
procedure answerEnds(ans: cardinal);
function  GetKey: char;
procedure showimage(x, y: Integer; var img);
procedure drawBackground(var img);
procedure defineTextArea(x1, y1, x2, y2: TscreenPos; useTextArea: boolean);
procedure ShowTextArea;
procedure setColors(foreground, background: cardinal);
function GetRGBColor(r, g, b: byte): LongInt;
procedure clearTextarea;
procedure endGraphics;
procedure showmouse(on: boolean);

Implementation
const MaxAnswers = 35;

var
  TextArea: ^byte; { can't be just "pointer" in GPC? }
  TextareaSize: longInt;

var graphicActive: boolean;

{$IfDef __GPC__}
  var tx1, ty1, tx2, ty2, tw, th : TscreenPos;
{$EndIf}

{$IfDef GRXMouse}
  var answerposition: array[1..MaxAnswers] of record f, t: TscreenPos end;
  var mouseshown: boolean;

  procedure resetAnswerPositions;
  var i: integer;
  begin
  for i:=1 to MaxAnswers do
      begin
      answerposition[i].f := -1;
      answerposition[i].t := -1
      end
  end;
{$EndIf}

procedure LockScreen;
begin end;

procedure UnlockScreen;
begin end;

procedure setExitKey(ExitKey: char);
begin end;

procedure answerStarts(ans: cardinal);
begin 
{$IfDef GRXMouse}
  answerposition[ans].f := GetY
{$EndIf}
end;

procedure answerEnds(ans: cardinal);
begin 
{$IfDef GRXMouse}
  answerposition[ans].t := GetY
{$EndIf}
end;

procedure showmouse(on: boolean);
begin 
{$IfDef GRXMouse}
  if GrMouseDetect 
    then mouseshown := on
    else mouseshown := false;
  if mouseshown
    then GrMouseDisplayCursor
    else GrMouseEraseCursor
{$EndIf}
end;

procedure setColors(foreground, background: cardinal);
begin
SetColor(foreground);
SetBkColor(background);

{$IfDef GRXMouse}
  GrMouseSetColors(foreground, background)
{$EndIf}
end;

{$IfDef __GPC__}

  function GetRGBColor(r, g, b: byte): LongInt;
  begin
  SetRGBColor(r, g, b);
  GetRGBColor := GetColor
  end;

{$Else}
  
  function GetRGBColor(r, g, b: byte): LongInt;
  begin
  { requires 16bit mode! }
  GetRGBColor := (((r shr 3) shl 11) or
                 ((g shr 2) shl 5)  or
                 (b shr 3))
		 and $FFFF
  end;
{$EndIf} { __GPC__ }


{$IfDef FPCSVGALIB}
   { the following functions only work as expected,
     when SVGAlib is already initialized! }

  function GetKey: char;
  var c: char;
  begin
  Read(c);
  GetKey := c
  end;

{$Else}

  {$IfDef GRXMouse}
     {$M GRXMouse}
     
     function GetMouse: char;
     const anykey = #06; { #06 = ACK: no answer - but it's "any key" }
     var 
       c: char;
       event: GrMouseEvent;
       x, y: integer;
       i: integer;
     begin
     c := #0;
     repeat
     GrMouseGetEventT(GR_M_KEYPRESS or GR_M_LEFT_DOWN or GR_M_RIGHT_DOWN, 
                      @event, 50);
     if event.key<>0 then c := chr(event.key);
     if ((event.flags and GR_M_RIGHT_DOWN)<>0) then c := #13;
     if ((event.flags and GR_M_LEFT_DOWN)<>0) then
       begin
       c := anykey; { a key was pressed }
       x := event.x - tx1;
       y := event.y - ty1;
       if (x>=0) and (x<=tw) and (y>=0) and (y<=th) then
          begin
	  i := 1;
          repeat
            if (y>=answerposition[i].f) and (y<=answerposition[i].t)
                  then c := ValueToKey(i);
            inc(i)
          until (c<>anykey) or (i>MaxAnswers);
          end
       end
     until c<>#0;
     GetMouse := c
     end;

     function GetKey: char;
     begin
     if not mouseshown { GrMouseGetEventT would show the mouse otherwise }
       then GetKey := ReadKey
       else GetKey := GetMouse
     end;
  
  {$Else}
  
     { not FPCSVGALIB not GRXMouse }
     function GetKey: char;
     var c: char;
     begin
     c := ReadKey;
     if c=#0 then begin c:=ReadKey; c:= #0 end;
     GetKey := c
     end;
  {$EndIf} {GRXMouse Else }    
{$EndIf} { FPCSVGALIB Else }

{$IfDef __GPC__}
  procedure drawBackground(var img);
  begin GrLoadContextFromPnmBuffer(NIL, img) end;

  procedure showimage(x, y: Integer; var img);
  var 
    width, height, maxval: integer;
    gc : GrContextPtr;
  begin
  GrQueryPnmBuffer(img, width, height, maxval);
  gc := GrCreateSubContext(x+tx1,y+ty1,x+tx1+width,y+ty1+height,NIL,NIL);
  GrLoadContextFromPnmBuffer(gc, img);
  GrDestroyContext(gc) { needed? }
  end;

{$Else}

  procedure drawBackground(var img);
  begin PutImage(0, 0, img, copyput) end;

  procedure showimage(x, y: Integer; var img);
  begin PutImage(x, y, img, copyput) end;

{$EndIf}
  
procedure initializeGraphicMode(const title, short: string; 
                                fullscreen: boolean);
var gd, gm, gerror: {$IfDef FPC} smallint {$Else} integer {$EndIf};
begin
{$IfDef FPC}
  gd := D16Bit;
  gm := m640x480;
{$EndIf}

{$IfDef __GPC__}
  gd := VESA16M; { PPM images can use that }
  gm := Res640x480;
  SetBGIMode(gd, gm);
{$EndIf}

initgraph(gd, gm, '');

gerror := graphResult;
if gerror<>GrOk then
   begin
   WriteLn(stderr, 'grquiz error: graphic mode not supported');
   Halt(1)
   { exitcode 1 seems also be used internally in svgalib,
     so it must be 1! }
   end;

graphicActive := true;

{$IfDef __GPC__}
   GrSetWindowTitle(title);
{$EndIf}

{$IfDef GRXMouse}
  GrMouseInit;
  showmouse(false); { just as initialization }
{$EndIf}
end;

procedure defineTextArea(x1, y1, x2, y2: TscreenPos; useTextArea: boolean);
begin
if useTextArea then
  begin
  TextAreaSize := ImageSize(x1, y1, x2, y2);
  GetMem(TextArea, TextAreaSize);
  GetImage(x1, y1, x2, y2, TextArea^)
  end;
setViewPort(x1, y1, x2, y2, clipon);

{$IfDef __GPC__}
  tx1 := x1;
  ty1 := y1;
  tx2 := x2;
  ty2 := y2;
  tw  := tx2-tx1+1;
  th  := ty2-ty1+1;
{$EndIf}

{$IfDef GRXMouse}
  resetAnswerPositions
{$EndIf}
end;

procedure clearTextarea;
begin
if TextArea=NIL
  then ClearViewPort { faster }
  else if (GetY<>0) or (GetX<>0) then { avoid redundant redrawing }
          begin
          PutImage(0, 0, Textarea^, copyput);
          MoveTo(0, 0)
          end;

{$IfDef GRXMouse}
  resetAnswerPositions
{$EndIf}
end;

procedure ShowTextArea;
begin end;

procedure endGraphics;
begin
if graphicActive then
  begin
  {$IfDef GRXMouse}
    GrMouseUnInit;
  {$EndIf}

  if Textarea<>NIL then FreeMem(TextArea, TextAreaSize);
  TextArea:=NIL;
  CloseGraph;
  graphicActive := false
  end
end;

procedure ident(s: string);
begin end;

Initialization

  ident('$Id: clgrph.pas,v 1.9 2006/12/10 17:09:49 akf Exp $');
  TextArea := NIL;
  TextAreaSize := 0;
  graphicActive := false;
  
Finalization

  endGraphics;
  
end.
