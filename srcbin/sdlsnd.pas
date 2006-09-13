{
* sdlsnd (unit)
* sound support with SDL
*
* $Id: sdlsnd.pas,v 1.7 2006/09/13 08:00:44 akf Exp $
*
* Copyright (c) 2005-2006 Andreas K. Foerster <akfquiz@akfoerster.de>
* Copyright (c) 1997-2004 Sam Lantinga
*
* Environment: FreePascal
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
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
  {$PackRecords 4}
{$EndIf}

unit sdlsnd;

interface
uses qsys;

{ initializes SDL with Audio,
  if sub is true, then it is opened as subsystem }
procedure InitAudio(sub: boolean);

{ closes the Audio-subsystem or all of SDL }
procedure CloseAudio(sub: boolean);

procedure useSDLsounds; { also started from InitAudio }


implementation

{$IfDef __GPC__}
  {$DEFINE cdecl attribute(cdecl)}
  {$pointer-arithmetic}
  {$L SDL}
  
  {$L introsnd.o}
  {$L rightsnd.o}
  {$L wrongsnd.o}
  {$L neutralsnd.o}
  {$L errorsnd.o}
  {$L infosnd.o}
  
  { change the sizes here, if you exchange a soundfile }
  var 
    IntroSound:   array[1 .. 38208] of byte; external name 'IntroSound';
    RightSound:   array[1 .. 13312] of byte; external name 'RightSound';
    WrongSound:   array[1 .. 11456] of byte; external name 'WrongSound';
    NeutralSound: array[1 ..  7424] of byte; external name 'NeutralSound';
    ErrorSound:   array[1 ..  9216] of byte; external name 'ErrorSound';
    InfoSound:    array[1 .. 16384] of byte; external name 'InfoSound';
{$EndIf} { __GPC__ }

{$IfDef FPC}
  {$I introsnd.inc}
  {$I rightsnd.inc}
  {$I wrongsnd.inc}
  {$I neutralsnd.inc}
  {$I errorsnd.inc}
  {$I infosnd.inc}

  {$LinkLib SDL}
{$EndIf}

type
  pSDL_AudioSpec = ^SDL_AudioSpec;
  SDL_AudioSpec = record
    freq     : CInteger;
    format   : Uint16;
    channels : Uint8;
    silence  : Uint8;
    samples  : Uint16;
    size     : Uint32;
    callback : pointer;
    userdata : pointer;
    end;

{ various constands - just the ones used here }
const 
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_AUDIO = $00000010;
  SDL_MIX_MAXVOLUME = 128;
  AUDIO_U8 = $0008;  { Unsigned 8-bit samples  }

var 
  AudioAvailable : Boolean = false;
  sndData : pByte = NIL;
  sndPos  : pByte = NIL;
  sndlen  : LongInt = 0;

function SDL_Init(flags: Uint32): CInteger; cdecl; 
           external name 'SDL_Init';

function SDL_InitSubSystem(flags: Uint32): CInteger; cdecl; 
           external name 'SDL_InitSubSystem';

procedure SDL_Quit; cdecl; 
            external name 'SDL_Quit';

procedure SDL_QuitSubSystem(flags: Uint32); cdecl;
            external name 'SDL_QuitSubSystem';

function SDL_OpenAudio(var desired: SDL_AudioSpec; 
                       obtained: pSDL_AudioSpec): CInteger; cdecl;
           external name 'SDL_OpenAudio';

procedure SDL_LockAudio; cdecl; 
           external name 'SDL_LockAudio';

procedure SDL_UnlockAudio; cdecl; 
           external name 'SDL_UnlockAudio';

procedure SDL_PauseAudio(pause_on: CInteger); cdecl; 
            external name 'SDL_PauseAudio';

procedure SDL_MixAudio(dst: pByte; src: pByte;
                       len: UInt32; volume: CInteger); cdecl; 
           external name 'SDL_MixAudio';

procedure playSound(s: pointer; len: LongInt);
begin
{ only play, when no other sound is playing,
  otherwise don't interrupt, don't wait, just forget it -
  rationale: a human speaker doesn't interrupt himself }

if AudioAvailable and (sndLen<=0) then
  begin
  SDL_LockAudio;
  sndData := pByte(s);
  sndPos  := sndData;
  sndLen  := len;
  SDL_UnLockAudio;
  SDL_PauseAudio(0)
  end
end;

procedure playIntroSound;
begin
playSound(addr(IntroSound), SizeOf(IntroSound))
end;

procedure playRightSound;
begin
playSound(addr(RightSound), SizeOf(RightSound))
end;

procedure playWrongSound;
begin
playSound(addr(WrongSound), SizeOf(WrongSound))
end;

procedure playNeutralSound;
begin
playSound(addr(NeutralSound), SizeOf(NeutralSound))
end;

procedure playErrorSound;
begin
playSound(addr(ErrorSound), SizeOf(ErrorSound))
end;

procedure playInfoSound;
begin
playSound(addr(InfoSound), SizeOf(InfoSound))
end;

procedure useSDLsounds;
begin
IntroSignal    := playIntroSound;
QuestionSignal := NoSignal;
RightSignal    := playRightSound;
FalseSignal    := playWrongSound;
NeutralSignal  := playNeutralSound;
InfoSignal     := playInfoSound;
ErrorSignal    := playErrorSound
end;

{ This is the callback procedure }
procedure fillAudio(var userdata; stream: pByte; len: CInteger); cdecl;
begin
if sndlen>0 then 
  begin
  if len > sndlen then len := sndlen;
  SDL_MixAudio(stream, sndPos, len, SDL_MIX_MAXVOLUME);
  inc(sndPos, len);
  dec(sndLen, len)
  end
end;

procedure InitAudio(sub: boolean); 
var desired : SDL_AudioSpec;
begin
if sub
  then AudioAvailable := SDL_InitSubSystem(SDL_INIT_AUDIO)=0
  else AudioAvailable := SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO)=0;

if AudioAvailable then 
  begin
  with desired do
    begin
    freq     := 22050;
    format   := AUDIO_U8;
    channels := 1;
    samples  := 1024;
    userdata := NIL;
    callback := Addr(fillAudio);
    end;

  AudioAvailable := SDL_OpenAudio(desired, NIL)=0;
  if AudioAvailable then useSDLsounds;
  end
end;

procedure CloseAudio(sub: boolean);
begin
if sub
  then SDL_QuitSubSystem(SDL_INIT_AUDIO)
  else SDL_Quit;
end;

end.
