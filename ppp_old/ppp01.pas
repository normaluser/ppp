{**************************************************************************
Copyright (C) 2015-2018 Parallel Realities

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

***************************************************************************
converted from "C" to "Pascal" by Ulrich 2022
***************************************************************************
* changed all PChar to string Types for better string handling!
* without momory holes; testet with: fpc -Criot -gl -gh ppp01.pas
***************************************************************************}

PROGRAM ppp01;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image, SDL2_Mixer, sysutils;

CONST SCREEN_WIDTH      = 1280;            { size of the grafic window }
      SCREEN_HEIGHT     = 720;             { size of the grafic window }
      MAX_TILES         = 7;
      TILE_SIZE         = 64;
      MAP_WIDTH         = 40;
      MAP_HEIGHT        = 20;
      MAP_RENDER_WIDTH  = 20;
      MAP_RENDER_HEIGHT = 12;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SND_CHANNELS  = 16;

TYPE                                        { "T" short for "TYPE" }
     TDelegating = (Game);
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     PTextur     = ^TTexture;
     TTexture    = RECORD
                     name : string;
                     texture : PSDL_Texture;
                     next : PTextur;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                     textureHead, textureTail : PTextur;
                     Delegate : TDelegate;
                   end;
     TStage      = RECORD
                     map : ARRAY[0..PRED(MAP_WIDTH),0..PRED(MAP_HEIGHT)] of integer;
                   end;

VAR app      : TApp;
    stage    : TStage;
    event    : TSDL_EVENT;
    exitLoop : BOOLEAN;
    gTicks   : UInt32;
    gRemainder : double;
    tiles    : ARRAY[1..MAX_TILES] of PSDL_Texture;

// *****************   UTIL   *****************

procedure errorMessage(Message : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(Message),NIL);
  HALT(1);
end;

// *****************   DRAW   *****************

procedure blit(texture : PSDL_Texture; x, y, center : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  SDL_QueryTexture(texture, NIL, NIL, @dest.w, @dest.h);

  if center <> 0 then
  begin
    dest.x := dest.w DIV 2;
    dest.y := dest.h DIV 2;
  end;

  SDL_RenderCopy(app.Renderer, texture, NIL, @dest);
end;

procedure blitRect(texture : PSDL_Texture; src : PSDL_Rect; x, y : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, texture, src, @dest);
end;

// ****************   TEXTURE   ***************

procedure addTextureToCache(LName : string; LTexture : PSDL_Texture);
VAR cache : PTextur;
begin
  NEW(cache);
  app.textureTail^.next := cache;
  app.textureTail := cache;
  cache^.name := LName;
  cache^.texture := LTexture;
  cache^.next := NIL;
end;

function getTexture(name : string) : PSDL_Texture;
VAR tf : PTextur;
begin
  getTexture := NIL;
  tf := app.textureHead^.next;
  while (tf <> NIL) do
  begin
    if tf^.name = name then
      getTexture := tf^.texture;
    tf := tf^.next;
  end;
end;

function loadTexture(Pfad : string) : PSDL_Texture;
VAR tg : PSDL_Texture;
begin
  tg := getTexture(Pfad);
  if tg = NIL then
  begin
    tg := IMG_LoadTexture(app.Renderer, PChar(Pfad));
    if tg = NIL then
      errorMessage(SDL_GetError());
    addTextureToCache(Pfad, tg);
  end;
  loadTexture := tg;
end;

procedure loadTiles;
VAR i : integer;
    filename : string;
begin
  for i := 1 to MAX_TILES do
  begin
    filename := 'gfx/tile' + IntToStr(i) + '.png';
    tiles[i] := loadTexture(filename);
  end;
end;

procedure initStage;
begin
  NEW(app.textureHead);
  app.textureHead^.name := '';
  app.textureHead^.texture := NIL;
  app.textureHead^.next := NIL;
  app.textureTail := app.textureHead;
end;

procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 0, 0, 0, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene;
begin
  SDL_RenderPresent(app.Renderer);
end;
// *****************    MAP   *****************

procedure drawMap;
VAR x, y, n : integer;
begin
  for y := 0 to PRED(MAP_RENDER_HEIGHT) do
  begin
    for x := 0 to PRED(MAP_RENDER_WIDTH) do
    begin
      n := stage.map[x,y];
      if (n > 0) then
      begin
        blit(tiles[n], x * TILE_SIZE, y * TILE_SIZE, 0);
      end;
    end;
  end;
end;

procedure loadMap(filename : string);
VAR i, x, y, le : integer;
    FileIn : text;
    line : string;
begin
  x := 0;
  assign (FileIn, filename);
  {$i-}; reset(FileIn); {$i+};
  if IOresult = 0 then
  begin
    for y := 0 to PRED(MAP_HEIGHT) do
    begin
      x := 0;
      readln(FileIn,line);
      line := stringReplace(line, ' ','',[rfReplaceAll]);
      le := length(line);

      for i := 1 to le do
      begin
        stage.map[x,y] := ORD(line[i]) - 48;
        INC(x);
      end;
    end;
    close(FileIn);
  end
  else errorMessage(filename + ' not found!');
end;

procedure initMap;
begin
  FillChar(stage.map, sizeof(stage.map), 0);
  loadTiles;
  loadMap('data/map01.dat');
end;

// *****************   STAGE   *****************

procedure draw_Game;
begin
  SDL_SetRenderDrawColor(app.renderer, 128, 192, 255, 255);
  SDL_RenderFillRect(app.renderer, NIL);

  drawMap;
end;

procedure logic_Game;
begin

end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Pete''s Pizza Party 1', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure destroyTexture;
VAR t, a : PTextur;
begin
  a := app.textureHead^.next;
  while (a <> NIL) do
  begin
    t := a^.next;
    DISPOSE(a);
    a := t;
  end;
  DISPOSE(app.TextureHead);
end;

procedure cleanUp;
begin
  destroyTexture;
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure atExit;
VAR i : byte;
begin
  for i := 1 to MAX_TILES do
    SDL_DestroyTexture (Tiles[i]);

  if ExitCode <> 0 then cleanUp;
  Mix_CloseAudio;
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow (app.Window);
  MIX_Quit;   { Quits the Music / Sound }
  IMG_Quit;   { Quits the SDL_Image }
  SDL_Quit;   { Quits the SDL }
  if Exitcode <> 0 then WriteLn(SDL_GetError());
  SDL_ShowCursor(1);
end;

// *****************   INPUT   *****************

procedure doKeyDown;
begin
  if event.key.repeat_ = 0 then
  begin
    CASE event.key.keysym.sym of
      SDL_KEYDOWN : begin
                      if ((event.key.repeat_ = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                        app.keyboard[event.key.keysym.scancode] := 1;
                       if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                    end;   { SDL_Keydown }
    end; { CASE }
  end;   { IF }
end;

procedure doKeyUp;
begin
  if event.key.repeat_ = 0 then
  begin
    CASE event.key.keysym.sym of
      SDL_KEYUP : begin
                    if ((event.key.repeat_ = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                      app.keyboard[event.key.keysym.scancode] := 0;
                    end;   { SDL_Keyup }
    end; { CASE }
  end;   { IF }
end;

procedure doInput;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    CASE event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: doKeyDown;
      SDL_KEYUP:   doKeyUp;

    end;  { CASE event }
  end;    { SDL_PollEvent }
end;

// ***************   DELEGATE   ***************

procedure delegate_Logic(Wahl : TDelegating);
begin
  CASE Wahl of
    Game : begin
             logic_Game;
             draw_Game;
           end;
  end;   { case }
end;

// *************   CAPFRAMERATE   *************

procedure CapFrameRate(VAR remainder : double; VAR Ticks : UInt32);
VAR wait, FrameTime : longint;
begin
  wait := 16 + TRUNC(remainder);
  remainder := remainder - TRUNC(remainder);
  frameTime := SDL_GetTicks - Ticks;
  DEC(wait, frameTime);
  if (wait < 1) then wait := 1;
  SDL_Delay(wait);
  remainder := remainder + 0.667;
  Ticks := SDL_GetTicks;
end;

// *****************   MAIN   *****************

begin
  CLRSCR;
  initSDL;
  initStage;
  initMap;
  addExitProc(@atExit);
  exitLoop := FALSE;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    delegate_Logic(Game);
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  cleanUp;
  atExit;
end.
