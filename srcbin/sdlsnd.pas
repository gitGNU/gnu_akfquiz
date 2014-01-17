{
* sdlsnd (unit)
* sound support with SDL
*
* Copyright (c) 2005-2006,2007,2010,2014
* Andreas K. Foerster <akfquiz@akfoerster.de>
* Copyright (c) 1997-2004 Sam Lantinga
*
* Environment: GNU Pascal or FreePascal
*
* This file is part of AKFQuiz
*
* AKFQuiz is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version.
*
* AKFQuiz is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
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

{ Translation table for the audio encoding mu-law }
const mu_law: Array[0..255] of Sint16
= (
  -32124, -31100, -30076, -29052, -28028, -27004, -25980, -24956, -23932,
  -22908, -21884, -20860, -19836, -18812, -17788, -16764, -15996, -15484,
  -14972, -14460, -13948, -13436, -12924, -12412, -11900, -11388, -10876,
  -10364, -9852, -9340, -8828, -8316, -7932, -7676, -7420, -7164, -6908,
  -6652, -6396, -6140, -5884, -5628, -5372, -5116, -4860, -4604, -4348,
  -4092, -3900, -3772, -3644, -3516, -3388, -3260, -3132, -3004, -2876,
  -2748, -2620, -2492, -2364, -2236, -2108, -1980, -1884, -1820, -1756,
  -1692, -1628, -1564, -1500, -1436, -1372, -1308, -1244, -1180, -1116,
  -1052, -988, -924, -876, -844, -812, -780, -748, -716, -684, -652, -620,
  -588, -556, -524, -492, -460, -428, -396, -372, -356, -340, -324, -308,
  -292, -276, -260, -244, -228, -212, -196, -180, -164, -148, -132, -120,
  -112, -104, -96, -88, -80, -72, -64, -56, -48, -40, -32, -24, -16, -8, 0,
  32124, 31100, 30076, 29052, 28028, 27004, 25980, 24956, 23932, 22908,
  21884, 20860, 19836, 18812, 17788, 16764, 15996, 15484, 14972, 14460,
  13948, 13436, 12924, 12412, 11900, 11388, 10876, 10364, 9852, 9340, 8828,
  8316, 7932, 7676, 7420, 7164, 6908, 6652, 6396, 6140, 5884, 5628, 5372,
  5116, 4860, 4604, 4348, 4092, 3900, 3772, 3644, 3516, 3388, 3260, 3132,
  3004, 2876, 2748, 2620, 2492, 2364, 2236, 2108, 1980, 1884, 1820, 1756,
  1692, 1628, 1564, 1500, 1436, 1372, 1308, 1244, 1180, 1116, 1052, 988,
  924, 876, 844, 812, 780, 748, 716, 684, 652, 620, 588, 556, 524, 492, 460,
  428, 396, 372, 356, 340, 324, 308, 292, 276, 260, 244, 228, 212, 196, 180,
  164, 148, 132, 120, 112, 104, 96, 88, 80, 72, 64, 56, 48, 40, 32, 24, 16,
  8, 0
);

{ various constants - just the ones used here }
const SDL_INIT_AUDIO = $00000010;

{$IfDef ENDIAN_BIG}
const AUDIO_S16 = $9010;  { Signed 16-bit samples, big endian }
{$Else}
const AUDIO_S16 = $8010;  { Signed 16-bit samples, little endian }
{$EndIf}

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


constructor TSound.Init(sndName: string);
var f: file;
begin
{$IfDef FPC}
  { needed to read from readonly-files
    - bad Borland heritage :-( }
  FileMode := 0;
{$EndIf}

assign(f, getSoundDir + sndName + '.ul');
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
procedure fillAudio(var userdata; stream: pSint16; len: CInteger); cdecl;
var i, l: CInteger;
begin
if sndlen>0 then 
  begin
  len := len div sizeof(Sint16);
  l := len;
  if l > sndlen then l := sndlen;

  for i := 1 to l do
    begin
    stream^ := mu_law[sndPos^];
    inc(stream);
    inc(sndPos)
    end;

  dec(sndLen, l);
  dec(len, l);

  for i := 1 to len do
    begin
    stream^ := 0;
    inc(stream)
    end

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
    freq     := 16000;
    format   := AUDIO_S16;
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

Finalization

  CloseAudio

end.
