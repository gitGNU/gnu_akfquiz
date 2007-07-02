{
* sdlsnd (unit)
* sound support with SDL
*
* $Id: sdlsnd.pas,v 1.16 2007/07/02 10:14:10 akf Exp $
*
* Copyright (c) 2005-2006,2007 Andreas K. Foerster <akfquiz@akfoerster.de>
* Copyright (c) 1997-2004 Sam Lantinga
*
* Environment: GNU Pascal or FreePascal
*
* This file is part of AKFQuiz
*
* AKFQuiz is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AKFQuiz is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*}

{$IfDef FPC}
  {$Mode Delphi}
  {$LongStrings on}
  {$PackRecords 4}
{$EndIf}

{$I-}

unit sdlsnd;

interface
uses qsys;

{ initializes SDL with Audio,
  if sub is true, then it is opened as subsystem }
procedure InitAudio(sub: boolean);

{ closes the Audio-subsystem or all of SDL }
procedure CloseAudio;

procedure useSDLsounds; { also started from InitAudio }


implementation

{$IfDef __GPC__}
  {$L SDL}
  {$DEFINE libSDL external name}
  {$DEFINE cdecl attribute(cdecl)}
  {$pointer-arithmetic}
  
  {$IfDef NEEDPTHREAD}
    {$L pthread}
  {$EndIf}
{$EndIf} { __GPC__ }

{$IfDef FPC}
  {$MACRO ON}
  {$DEFINE libSDL:=cdecl; external 'SDL' name}
  
  {$IfDef NEEDPTHREAD}
    {$LinkLib pthread}
  {$EndIf}
{$EndIf} { FPC }

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

type TSound = object
         size : LongInt;
	 data : pByte;
	 
	 constructor Init(SndName: String);
	 destructor Done;
	 procedure play;
	 end;

{ various constands - just the ones used here }
const 
  SDL_INIT_AUDIO = $00000010;
  SDL_MIX_MAXVOLUME = 128;
  AUDIO_U8 = $0008;  { Unsigned 8-bit samples  }

var 
  IntroSound, RightSound, WrongSound, NeutralSound, 
  ErrorSound, InfoSound : TSound;

var
  isSubSystem: boolean = false; { is it initialzed as subsystem? }
  AudioAvailable : boolean = false;
  sndData : pByte = NIL;
  sndPos  : pByte = NIL;
  sndlen  : LongInt = 0;


function SDL_Init(flags: Uint32): CInteger; libSDL 'SDL_Init';

function SDL_InitSubSystem(flags: Uint32): CInteger;  
           libSDL 'SDL_InitSubSystem';

procedure SDL_Quit; libSDL 'SDL_Quit';

procedure SDL_QuitSubSystem(flags: Uint32); libSDL 'SDL_QuitSubSystem';

function SDL_OpenAudio(var desired: SDL_AudioSpec; 
                       obtained: pSDL_AudioSpec): CInteger; 
	   libSDL 'SDL_OpenAudio';

procedure SDL_LockAudio; libSDL 'SDL_LockAudio';

procedure SDL_UnlockAudio; libSDL 'SDL_UnlockAudio';

procedure SDL_PauseAudio(pause_on: CInteger); libSDL 'SDL_PauseAudio';

procedure SDL_MixAudio(dst: pByte; src: pByte;
                       len: UInt32; 
		       volume: CInteger); libSDL 'SDL_MixAudio';


constructor TSound.Init(sndName: string);
var f: file;
begin
{$IfDef FPC}
  { needed to read from readonly-files
    - bad Borland heritage :-( }
  FileMode := 0;
{$EndIf}

assign(f, getSoundDir + sndName + '.ub');
Reset(f, 1);
size := FileSize(f);
GetMem(data, size);
if data <> NIL 
  then BlockRead(f, data^, size)
  else size := 0;
close(f);

if IOResult<>0 then
  begin FreeMem(data, size); size := 0; data := NIL end
end;

destructor TSound.Done;
begin
FreeMem(data, size);
data := NIL;
size := 0
end;

procedure TSound.play;
begin
{ only play, when no other sound is playing,
  otherwise don't interrupt, don't wait, just forget it -
  rationale: a human speaker doesn't interrupt himself }

if AudioAvailable and (sndLen<=0) then
  begin
  SDL_LockAudio;
  sndData := data;
  sndPos  := sndData;
  sndLen  := size;
  SDL_UnLockAudio;
  SDL_PauseAudio(0)
  end
end;

procedure LoadSounds;
begin
IntroSound.Init('introsnd');
RightSound.Init('rightsnd');
WrongSound.Init('wrongsnd');
NeutralSound.Init('neutralsnd');
ErrorSound.Init('errorsnd');
InfoSound.Init('infosnd')
end;

procedure FreeSounds;
begin
IntroSound.Done;
RightSound.Done;
WrongSound.Done;
NeutralSound.Done;
ErrorSound.Done;
InfoSound.Done
end;

procedure playIntroSound;
begin IntroSound.play end;

procedure playRightSound;
begin RightSound.play end;

procedure playWrongSound;
begin WrongSound.play end;

procedure playNeutralSound;
begin NeutralSound.play end;

procedure playErrorSound;
begin ErrorSound.play end;

procedure playInfoSound;
begin InfoSound.play end;

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

procedure closeSDLAudio;
begin
if isSubSystem
    then SDL_QuitSubSystem(SDL_INIT_AUDIO)
    else SDL_Quit;

AudioAvailable := false
end;

procedure InitAudio(sub: boolean); 
var desired : SDL_AudioSpec;
begin
isSubSystem := sub;
AudioAvailable := false;
if not SoundFilesAvailable then exit;

if isSubSystem
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
    callback := Addr(fillAudio)
    end;

  AudioAvailable := SDL_OpenAudio(desired, NIL)=0;

  if AudioAvailable 
    then begin LoadSounds; useSDLsounds end
    else closeSDLAudio
  end
end;

procedure CloseAudio;
begin
DisableSignals;
FreeSounds;

{ SDL_Quit might already be called... }
if AudioAvailable then closeSDLAudio
end;

Initialization

  ident('$Id: sdlsnd.pas,v 1.16 2007/07/02 10:14:10 akf Exp $')

Finalization

  CloseAudio

end.
