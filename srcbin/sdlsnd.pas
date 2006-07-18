{
* sdlsnd (unit)
* sound support with SDL
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
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*}

unit sdlsnd;

interface
uses qsys, SDL, SDL_Audio;

{ initializes SDL with Audio,
  if sub is true, then it is opened as subsystem }
procedure InitAudio(sub: boolean);
procedure useSDLsounds; { also started from InitAudio }



implementation

{$I introsnd.inc}
{$I rightsnd.inc}
{$I wrongsnd.inc}
{$I neutralsnd.inc}
{$I errorsnd.inc}
{$I infosnd.inc}

var 
  AudioAvailable : Boolean = false;
  sndData : pByte = NIL;
  sndPos  : pByte = NIL;
  sndlen  : LongInt = 0;

{ I don't like the definition in SDL4Freepascal }
function SDL_OpenAudio(var desired:SDL_AudioSpec; 
                       obtained:pSDL_AudioSpec):longint;cdecl;
		       external 'SDL';

procedure playSound(s: pByte; len: LongInt);
begin
if AudioAvailable and (sndLen<=0) then
  begin
  SDL_LockAudio;
  sndData := s;
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

procedure fillAudio(var userdata; stream:pByte; len:LongInt);cdecl;
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
  else AudioAvailable := SDL_Init(SDL_INIT_AUDIO)=0;
  
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

end.
