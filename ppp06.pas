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
* Procedural Parameters for Tick (Platform/Pizza) and Delegate (Draw/Logic)
* Procedural Parameter for Touch Pizza integrated
* corrected the sin-function for the pizza
* added SDL_Log Message
* SCANF Funktion aus C braucht keine Integervariable
* without momory holes; tested with: fpc -Criot -gl -gh ppp06.pas
***************************************************************************}

PROGRAM ppp06;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image, SDL2_Mixer, Math, sysutils, cTypes;

CONST SCREEN_WIDTH      = 1280;            { size of the grafic window }
      SCREEN_HEIGHT     = 720;             { size of the grafic window }
      MAX_TILES         = 7;
      TILE_SIZE         = 64;
      MAP_WIDTH         = 40;
      MAP_HEIGHT        = 20;
      MAP_RENDER_WIDTH  = 20;
      MAP_RENDER_HEIGHT = 12;
      PLAYER_MOVE_SPEED = 6;
      PLATFORM_SPEED    = 4;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SND_CHANNELS  = 16;
      EF_NONE           = 0;
      EF_WEIGHTLESS     = (2 << 0);   //2
      EF_SOLID          = (2 << 1);   //4
      EF_PUSH           = (2 << 2);   //8

      GLYPH_WIDTH       = 18;
      GLYPH_HEIGHT      = 29;

      Map_Path          = 'data/map06.dat';
      Ents_Path         = 'data/ents06.dat';

TYPE                                        { "T" short for "TYPE" }
      TDelegating = Procedure;
      TDelegate   = RECORD
                      logic, draw : TDelegating;
                    end;
      PTexture    = ^TTexture;
      TTexture    = RECORD
                      name : string;
                      texture : PSDL_Texture;
                      next : PTexture;
                    end;
      TApp        = RECORD
                      Window   : PSDL_Window;
                      Renderer : PSDL_Renderer;
                      Keyboard : ARRAY[0..MAX_KEYBOARD_KEYS] OF integer;
                      TextureHead, TextureTail : PTexture;
                      Delegate : TDelegate;
                    end;
      PEntity     = ^TEntity;
      TTouch      = Procedure(Wert1 : PEntity);
      TTick       = Procedure;
      TEntity     = RECORD
                      x, y, ex, ey, sx, sy, dx, dy, value : double;
                      w, h : cint;
                      health : integer;
                      isOnGround : Boolean;
                      texture : PSDL_Texture;
                      touch : TTouch;
                      tick : TTick;
                      flags : UInt32;
                      riding : PEntity;
                      next : PEntity;
                    end;
      TStage      = RECORD
                      camera : TSDL_Point;
                      map : ARRAY[0..PRED(MAP_WIDTH), 0..PRED(MAP_HEIGHT)] of integer;
                      EntityHead, EntityTail : PEntity;
                      pizzaTotal, pizzaFound : integer;
                    end;

      TAlignment  = (TEXT_LEFT, TEXT_CENTER, TEXT_RIGHT);
      TSound      = (SND_JUMP, SND_PIZZA, SND_PIZZA_DONE, SND_MAX);
      TChannel    = (CH_PLAYER, CH_PIZZA);

VAR   app         : TApp;
      stage       : TStage;
      event       : TSDL_EVENT;
      exitLoop    : Boolean;
      gTicks      : UInt32;
      gRemainder  : double;
      fontTexture : PSDL_Texture;
      tiles       : ARRAY[1..MAX_TILES] of PSDL_Texture;
      pete        : ARRAY[0..1] of PSDL_Texture;
      music       : PMix_Music;
      sounds      : ARRAY[0..PRED(ORD(SND_MAX))] OF PMix_Chunk;
      player,
      selv        : PEntity;

// *****************   UTIL   *****************

procedure errorMessage(Message1 : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(Message1),NIL);
  HALT(1);
end;

procedure pathTest;
begin
  if NOT FileExists(Map_Path) then ErrorMessage(Map_Path + ' not found!');
end;

procedure logMessage(Message1 : string);
VAR Fmt : PChar;
begin
  Fmt := 'File not found: %s'#13;    // Formatstring und "ARRAY of const" als Parameteruebergabe in [ ]
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_WARN, Fmt, [PChar(Message1)]);
end;

procedure calcSlope(x1, y1, x2, y2 : double; VAR dx, dy : double);   { DX=DY => -1 oder 1 }
VAR steps : integer;
begin
  steps := TRUNC(MAX(abs(x1 - x2), abs(y1 - y2)));

  if (steps = 0) then
  begin
    dx := 0.0; dy := 0.0;
  end
  else
  begin
    dx := (x1 - x2) / steps;
    dy := (y1 - y2) / steps;
  end;
end;

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : Boolean;
begin
  collision := (MAX(x1, x2) < MIN(x1 + w1, x2 + w2)) AND (MAX(y1, y2) < MIN(y1 + h1, y2 + h2));
end;

procedure initEntity(e : PEntity);
begin
  e^.x := 0.0; e^.ex := 0.0; e^.sx := 0.0; e^.dx := 0.0; e^.w := 0;
  e^.y := 0.0; e^.ey := 0.0; e^.sy := 0.0; e^.dy := 0.0; e^.h := 0;
  e^.riding := NIL; e^.next := NIL; e^.tick := NIL; e^.touch := NIL;
  e^.isOnGround := FALSE; e^.flags := EF_NONE; e^.health := 1;
  e^.value := 0.0;  e^.texture := NIL;
end;

procedure initTexture;
begin
  NEW(app.TextureHead);
  app.TextureHead^.name := '';
  app.TextureHead^.texture := NIL;
  app.TextureHead^.next := NIL;
  app.TextureTail := app.TextureHead;
end;

// *****************   SOUND   ****************

procedure loadSounds;
VAR i : byte;
begin
  sounds[ORD(SND_JUMP)] := Mix_LoadWAV('sound/331381__qubodup__public-domain-jump-sound.ogg');
  if sounds[ORD(SND_JUMP)] = NIL then logMessage('"/sound/331381__qubodup__public-domain-jump-sound.ogg"');
  sounds[ORD(SND_PIZZA)] := Mix_LoadWAV('sound/90134__pierrecartoons1979__found-item.ogg');
  if sounds[ORD(SND_PIZZA)] = NIL then logMessage('"/sound/90134__pierrecartoons1979__found-item.ogg"');
  sounds[ORD(SND_PIZZA_DONE)] := Mix_LoadWAV('sound/449069__ricniclas__fanfare.ogg');
  if sounds[ORD(SND_PIZZA_DONE)] = NIL then logMessage('"/sound/449069__ricniclas__fanfare.ogg"');

  for i := 0 to PRED(ORD(SND_MAX)) do
    Mix_VolumeChunk(sounds[i], MIX_MAX_VOLUME);
end;

procedure playSound(id, channel : integer);
begin
  Mix_PlayChannel(channel, sounds[id], 0);
end;

procedure playMusic(play : Boolean);
VAR m : integer;
begin
  if play = TRUE then m := -1 else m := 0;
  Mix_PlayMusic(music, m);
end;

procedure loadMusic(filename : string);
begin
  if music <> NIL then
  begin
    Mix_HaltMusic;
    Mix_FreeMusic(music);
    music := NIL;
  end;
  music := Mix_LoadMUS(PChar(filename));
  if music = NIL then logMessage('"/' + filename + '"');
  Mix_VolumeMusic(MIX_MAX_VOLUME);
end;

procedure initSounds;
begin
  music := NIL;
  loadSounds;
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
    dest.x := dest.x - dest.w DIV 2;
    dest.y := dest.y - dest.h DIV 2;
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

procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 128, 192, 255, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene;
begin
  SDL_RenderPresent(app.Renderer);
end;

// ****************   TEXTURE   ***************

procedure addTextureToCache(LName : string; LTexture : PSDL_Texture);
VAR cache : PTexture;
begin
  NEW(cache);
  app.TextureTail^.next := cache;
  app.TextureTail := cache;
  cache^.name := LName;
  cache^.texture := LTexture;
  cache^.next := NIL;
end;

function getTexture(name : string) : PSDL_Texture;
VAR tf : PTexture;
begin
  getTexture := NIL;
  tf := app.TextureHead^.next;
  while (tf <> NIL) do
  begin
    if tf^.name = name then
      getTexture := tf^.texture;
    tf := tf^.next;
  end;
end;

function loadTexture(pfad : string) : PSDL_Texture;
VAR tg : PSDL_Texture;
begin
  tg := getTexture(pfad);
  if tg = NIL then
  begin
    tg := IMG_LoadTexture(app.Renderer, PChar(pfad));
    if tg = NIL then
      errorMessage(SDL_GetError());
    addTextureToCache(pfad, tg);
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

// *****************    MAP   *****************

procedure drawMap;
VAR x, y, n, x1, x2, y1, y2, mx, my : integer;
begin
  x1 := (stage.camera.x MOD TILE_SIZE) * (-1);
  if (x1 = 0) then x2 := x1 + MAP_RENDER_WIDTH * TILE_SIZE
              else x2 := x1 + TILE_SIZE + MAP_RENDER_WIDTH * TILE_SIZE;

  y1 := (stage.camera.y MOD TILE_SIZE) * (-1);
  if (y1 = 0) then y2 := y1 + MAP_RENDER_HEIGHT * TILE_SIZE
              else y2 := y1 + TILE_SIZE + MAP_RENDER_HEIGHT * TILE_SIZE;

  mx := stage.camera.x DIV TILE_SIZE;
  my := stage.camera.y DIV TILE_SIZE;

  y := y1;
  while y <= y2 do
  begin
    x := x1;
    while x <= x2 do
    begin
      if ((mx >= 0) AND (my >= 0) AND (mx < MAP_WIDTH) AND (my < MAP_HEIGHT)) then
      begin
        n := stage.map[mx,my];
        if (n > 0) then
          blit(tiles[n], x, y, 0);
      end;
      INC(mx);
      INC(x, TILE_SIZE);
    end;
    mx := stage.camera.x DIV TILE_SIZE;
    INC(my);
    INC(y, TILE_SIZE);
  end;
end;

procedure loadMap(filename : string);
VAR i, x, y, le : integer;
    FileIn : text;
    line : string;
    a : string[10];
begin
  assign (FileIn, filename);
  {$i-}; reset(FileIn); {$i+};
  if IOresult = 0 then
  begin
    for y := 0 to PRED(MAP_HEIGHT) do
    begin
      x := 0;                               // first tile of the line
      a := '';                              // new string / number
      readln(FileIn,line);
      le := length(line);

      for i := 1 to le do                   // parse through the line
      begin
        if line[i] <> ' ' then              // if line[i] is a number and not space
        begin
          a := a + line[i];                 // add number to the other numbers
          if i = le then                    // end of line, so add the last number!
          begin
            stage.map[x,y] := StrToInt(a);  // write it to stage.map as last number
          end;
        end
        else
        begin
          stage.map[x,y] := StrToInt(a);    // write number regular
          INC(x);                           // next tile
          a := '';                          // new string / number
        end;
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
  loadMap(Map_Path);
end;

// *****************   Block   ****************

procedure initBlock(line : string);
VAR e : PEntity;
    namen : string;
    a, b : integer;
begin
  NEW(e);
  initEntity(e);
  stage.EntityTail^.next := e;
  stage.EntityTail := e;
  SScanf(line, '%s %d %d', [@namen, @a, @b]);
  e^.x := a; e^.y := b;
  e^.texture := loadTexture('gfx/block.png');
  SDL_QueryTexture(e^.texture, NIL, NIL, @e^.w, @e^.h);
  e^.health := 1;
  e^.touch := NIL;
  e^.flags := EF_SOLID + EF_WEIGHTLESS;
end;

// ***************   PLATFORM   ***************

procedure tick_Platform;
begin
  if ((abs(selv^.x - selv^.sx) < PLATFORM_SPEED) AND (abs(selv^.y - selv^.sy) < PLATFORM_SPEED)) then
  begin
    calcSlope(TRUNC(selv^.ex), TRUNC(selv^.ey), TRUNC(selv^.x), TRUNC(selv^.y), selv^.dx, selv^.dy);
    selv^.dx := selv^.dx * PLATFORM_SPEED;      { also +1 oder -1 multipliziert Platform_Speed }
    selv^.dy := selv^.dy * PLATFORM_SPEED;      { also +1 oder -1 multipliziert Platform_Speed }
  end;

  if ((abs(selv^.x - selv^.ex) < PLATFORM_SPEED) AND (abs(selv^.y - selv^.ey) < PLATFORM_SPEED)) then
  begin
    calcSlope(TRUNC(selv^.sx), TRUNC(selv^.sy), TRUNC(selv^.x), TRUNC(selv^.y), selv^.dx, selv^.dy);
    selv^.dx := selv^.dx * PLATFORM_SPEED;      { also +1 oder -1 multipliziert Platform_Speed }
    selv^.dy := selv^.dy * PLATFORM_SPEED;      { also +1 oder -1 multipliziert Platform_Speed }
  end;
end;

procedure initPlatform(line : string);
VAR e : PEntity;
    namen : string;
    a, b, c, d : integer;
begin
  NEW(e);
  initEntity(e);
  stage.EntityTail^.next := e;
  stage.EntityTail := e;
  SScanf(line, '%s %d %d %d %d', [@namen, @a, @b, @c, @d]);
  e^.sx := a; e^.sy := b;     { sx, sy : StartX, StartY }
  e^.ex := c; e^.ey := d;     { ex, ey : EndX,   EndY   }

  e^.x := e^.sx;
  e^.y := e^.sy;
  e^.tick := @tick_Platform;
  e^.texture := loadTexture('gfx/platform.png');
  SDL_QueryTexture(e^.texture, NIL, NIL, @e^.w, @e^.h);
  e^.health := 1;
  e^.touch := NIL;
  e^.flags := EF_SOLID + EF_WEIGHTLESS + EF_PUSH;
end;

// *****************   PIZZA   ****************

procedure touch_Pizza(other : PEntity);
begin
  if (selv^.health > 0) AND (other = player) then
  begin
    selv^.health := 0;

    INC(stage.pizzaFound);

    if (stage.pizzaFound = stage.pizzaTotal) then
    begin
      playSound(ORD(SND_PIZZA_DONE), ORD(CH_PIZZA));
    end
    else
    begin
      playSound(ORD(SND_PIZZA), ORD(CH_PIZZA));
    end;
  end;
end;

procedure tick_Pizza;
begin
  if selv^.value > 2 * PI then selv^.value := 0.0;
  selv^.value := selv^.value + 0.1;
  // jumping pizza: new y := org y + 10 + sin( complete periode Pi )
  selv^.y := selv^.sy + 10 + 10 * sin(selv^.value);
end;

procedure initPizza(line : string);
VAR e : PEntity;
    namen : string;
    a, b : integer;
begin
  NEW(e);
  initEntity(e);
  stage.EntityTail^.next := e;
  stage.EntityTail := e;
  SScanf(line, '%s %d %d', [@namen, @a, @b]);
  e^.x := a; e^.y := b; e^.sy := b;    // sy: helping variable for orginal y value
  e^.texture := loadTexture('gfx/pizza.png');
  SDL_QueryTexture(e^.texture, NIL, NIL, @e^.w, @e^.h);

  e^.health := 1;
  e^.flags := EF_WEIGHTLESS;
  e^.tick := @tick_Pizza;
  e^.touch := @touch_Pizza;

  INC(stage.pizzaTotal);
end;

// ***************   ENTITIES   ***************

function isInsideMap(x, y : integer) : Boolean;
begin
  if ((x >= 0) AND (y >= 0) AND (x < MAP_WIDTH) AND (y < MAP_HEIGHT))
    then isInsideMap := TRUE
    else isInsideMap := FALSE;
end;

procedure addEntFromLine(line : string);
VAR namen : string;
begin
  SScanf(line, '%s', [@namen]);
  if namen = 'BLOCK' then
  begin
    initBlock(line);
  end
  else if namen = 'PLATFORM' then
  begin
    initPlatform(line);
  end
  else if namen = 'PIZZA' then
  begin
    initPizza(line);
  end
  else errorMessage(('unknown entity: ' + namen));
end;

procedure loadEnts(filename : string);
VAR Datei: Text;               (* Dateizeiger *)
    zeile : string;
BEGIN
  assign (Datei, filename);    (* pfad festlegen *)
  {$i-}; reset(Datei); {$i+};  (* Datei zum Lesen oeffnen *)
  if IOResult = 0 then
  begin
    REPEAT
      readLn (Datei, zeile);   (* eine Zeile lesen *)
      addEntFromLine(zeile);
    UNTIL EOF (Datei);  (* Abbruch, wenn das Zeilenende erreicht ist; also wenn EOF TRUE liefert *)
    close (Datei);      (* Datei schliessen *)
  end
  else logMessage(filename);
end;

procedure drawEntities;
VAR e : PEntity;
begin
  e := stage.EntityHead^.next;
  while e <> NIL do
  begin
    blit(e^.texture, ROUND(e^.x - stage.camera.x), ROUND(e^.y - stage.camera.y), 0);
    e := e^.next;
  end;
end;

procedure moveToWorld(e : PEntity; dx, dy : double);
VAR mx, my, hit, adj : integer;
begin
  if (dx <> 0) then
  begin
    //ORG C-Code: mx = dx > 0 ? (e->x + e->w) : e->x;
    if dx > 0 then mx := TRUNC(e^.x + e^.w)
              else mx := TRUNC(e^.x);
    mx := mx DIV TILE_SIZE;
    my := TRUNC(e^.y) DIV TILE_SIZE;
    hit := 0;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    my := (TRUNC(e^.y + e^.h) - 1) DIV TILE_SIZE;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    if hit = 1 then
    begin
      //ORG C-Code: adj = dx > 0 ? -e^.w : TILE_SIZE
      if dx > 0 then adj := -e^.w
                else adj := TILE_SIZE;
      e^.x := (mx * TILE_SIZE) + adj;
      e^.dx := 0;
    end;
  end;

  if (dy <> 0) then
  begin
    //ORG C-Code: my = dy > 0 ? (e^.y + e^.h) : e^.y;
    if dy > 0 then my := TRUNC(e^.y + e^.h)
              else my := TRUNC(e^.y);
    my := my DIV TILE_SIZE;
    mx := TRUNC(e^.x) DIV TILE_SIZE;
    hit := 0;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    mx := (TRUNC(e^.x + e^.w) - 1) DIV TILE_SIZE;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    if (hit = 1) then
    begin
      //ORG C-Code: adj = dy > 0 ? -e^.h : TILE_SIZE;
      if dy > 0 then adj := -e^.h
                else adj := TILE_SIZE;
      e^.y := (my * TILE_SIZE) + adj;
      e^.dy := 0;

      //e^.isOnGround = dy > 0;
      if dy > 0 then e^.isOnGround := TRUE;
    end;
  end;
end;

{*************** FORWARD Declaration !! **************}
procedure push(e : PEntity; dx, dy : double); FORWARD;
{*****************************************************}

procedure moveToEntities(e : PEntity; dx, dy : double);
VAR other : PEntity;
    adj : integer;
begin
  other := stage.EntityHead^.next;
  while other <> NIL do
  begin
    if ((other <> e) AND (collision(e^.x, e^.y, e^.w, e^.h, other^.x, other^.y, other^.w, other^.h))) then
    begin
      if (other^.flags AND EF_SOLID) <> 0 then
      begin
        if (dy <> 0) then
        begin
          //ORG C-Code: adj = dy > 0 ? -e^.h : other^.h;
          if dy > 0 then
            adj := -e^.h
          else
            adj := other^.h;

          e^.y := other^.y + adj;
          e^.dy := 0;

          if dy > 0 then
          begin
            e^.isOnGround := TRUE;
            e^.riding := other;
          end;
        end;

        if (dx <> 0) then
        begin
          //ORG C-Code: adj = dx > 0 ? -e^.w : other^.w;
          if dx > 0 then
            adj := -e^.w
          else
            adj := other^.w;

          e^.x := other^.x + adj;
          e^.dx := 0;
        end;
      end
      else if (e^.flags AND EF_PUSH) <> 0 then
      begin
        other^.x := other^.x + e^.dx;
        push(other, e^.dx, 0);

        other^.y := other^.y + e^.dy;
        push(other, 0, e^.dy);
      end;
      if assigned(e^.touch) then
        touch_Pizza(other);
    end;
    other := other^.next;
  end;
end;

procedure push(e : PEntity; dx, dy : double);
begin
  moveToWorld(e, dx, dy);
  moveToEntities(e, dx, dy);
end;

procedure move(e : PEntity);
begin
  if (NOT(e^.flags AND EF_WEIGHTLESS <> 0)) then
  begin
    e^.dy := e^.dy + 1.5;
    e^.dy := MAX(MIN(e^.dy, 18), -999);
  end;

  if (e^.riding <> NIL) AND (e^.riding^.dy > 0) then
    e^.dy := e^.riding^.dy + 1;

  e^.riding := NIL;

  e^.isOnGround := FALSE;

  e^.x := e^.x + e^.dx;
  push(e, e^.dx, 0);

  e^.y := e^.y + e^.dy;
  push(e, 0, e^.dy);
end;

procedure doEntities;
VAR e, prev : PEntity;
begin
  prev := stage.EntityHead;
  e := stage.EntityHead^.next;
  while e <> NIL do
  begin
    selv := e;
    if assigned(e^.tick) then
      e^.tick;
    move(e);
    if (e^.health <= 0) then
    begin
      if (e = stage.EntityTail) then
        stage.EntityTail := prev;
      prev^.next := e^.next;
      DISPOSE(e);
      e := prev;
    end;
    prev := e;
    e := e^.next;
  end;
  e := stage.EntityHead^.next;
  while e <> NIL do
  begin
    if (e^.riding <> NIL) then
    begin
      e^.x := e^.x + e^.riding^.dx;
      push(e, e^.riding^.dx, 0);
    end;

    e^.x := MIN(MAX(e^.x, 0), (MAP_WIDTH  * TILE_SIZE));
    e^.y := MIN(MAX(e^.y, 0), (MAP_HEIGHT * TILE_SIZE));
    e := e^.next;
  end;
end;

procedure initEntities;
begin
  loadEnts(Ents_Path);
end;

// ****************   CAMERA   ****************

procedure doCamera;
begin
  stage.camera.x := ROUND(player^.x + (player^.w DIV 2));
  stage.camera.y := ROUND(player^.y + (player^.h DIV 2));

  stage.camera.x := stage.camera.x - (SCREEN_WIDTH DIV 2);
  stage.camera.y := stage.camera.y - (SCREEN_HEIGHT DIV 2);

  stage.camera.x := MIN(MAX(stage.camera.x, 0), (MAP_WIDTH  * TILE_SIZE) - SCREEN_WIDTH);
  stage.camera.y := MIN(MAX(stage.camera.y, 0), (MAP_HEIGHT * TILE_SIZE) - SCREEN_HEIGHT);
end;

procedure doPlayer;
begin
  player^.dx := 0;

  if ((app.Keyboard[SDL_SCANCODE_A] = 1) OR (app.Keyboard[SDL_SCANCODE_LEFT] = 1)) then
  begin
    player^.dx := player^.dx - PLAYER_MOVE_SPEED;
    player^.texture := pete[1];
  end;

  if ((app.Keyboard[SDL_SCANCODE_D] = 1) OR (app.Keyboard[SDL_SCANCODE_RIGHT] = 1)) then
  begin
    player^.dx := player^.dx + PLAYER_MOVE_SPEED;
    player^.texture := pete[0];
  end;

  if ((app.Keyboard[SDL_SCANCODE_I] = 1) AND (player^.isOnGround = TRUE)) then
  begin
    player^.riding := NIL;
    player^.dy := -20;
    playSound(ORD(SND_JUMP), ORD(CH_PLAYER));
  end;

  if (app.Keyboard[SDL_SCANCODE_SPACE] = 1) then
  begin
    player^.x := 0;
    player^.y := 0;
    app.Keyboard[SDL_SCANCODE_SPACE] := 0;
  end;
end;

procedure initPlayer;
begin
  NEW(player);
  initEntity(player);
  stage.EntityTail^.next := player;
  stage.EntityTail := player;

  pete[0] := loadTexture('gfx/pete01.png');
  pete[1] := loadTexture('gfx/pete02.png');

  player^.texture := pete[0];

  SDL_QueryTexture(player^.texture, NIL, NIL, @player^.w, @player^.h);
  player^.health := 1;
end;

// *****************   TEXT   *****************

procedure drawText(x, y, r, g, b : integer; align : TAlignment; outText : string);
VAR i, len : integer;
    rect : TSDL_Rect;
begin
  len := LENGTH(outText);
  CASE align of
    TEXT_RIGHT :  begin x := x - (len * GLYPH_WIDTH);       end;
    TEXT_CENTER : begin x := x - (len * GLYPH_WIDTH) DIV 2; end;
  end;
  outText := UPCASE(outText);  { all capital letters }
  rect.w := GLYPH_WIDTH;
  rect.h := GLYPH_HEIGHT;
  rect.y := 0;
  SDL_SetTextureColorMod(fontTexture, r, g, b);

  for i := 1 to len do
  begin
    if (outText[i] IN [' '..'Z']) then
    begin
      rect.x := (ORD(outText[i]) - ORD(' ')) * GLYPH_WIDTH;
      blitRect(fontTexture, @rect, x, y);
      INC(x, GLYPH_WIDTH);
    end;
  end;
end;

function numberfill(a : integer) : string;
VAR FMT : string;
begin
  Fmt := '[%d]';                   { Fmt: arguments for Format }
  numberfill := Format(Fmt,[a]);   { Format: format a string with given arguments (=> Fmt) }
end;

procedure initFonts;
begin
  fontTexture := loadTexture('gfx/font.png');
end;

// *****************   STAGE   *****************

procedure drawHud;
VAR r : TSDL_Rect;
begin
  r.x := 0;
  r.y := 0;
  r.w := SCREEN_WIDTH;
  r.h := 35;

  SDL_SetRenderDrawBlendMode(app.renderer, SDL_BLENDMODE_BLEND);
  SDL_SetRenderDrawColor(app.renderer, 0, 0, 0, 196);
  SDL_RenderFillRect(app.renderer, @r);
  SDL_SetRenderDrawBlendMode(app.renderer, SDL_BLENDMODE_NONE);

  //drawText(SCREEN_WIDTH - 5, 5, 255, 255, 255, TEXT_RIGHT, 'PIZZA ' + numberfill(stage.pizzaFound) + '/' + numberfill(stage.pizzaTotal));
  drawText(SCREEN_WIDTH - 5, 5, 255, 255, 255, TEXT_RIGHT, 'PIZZA ' + IntToStr(stage.pizzaFound) + '/' + IntToStr(stage.pizzaTotal));
end;

procedure draw_Game;
begin
  drawMap;
  drawEntities;
  drawHud;
end;

procedure logic_Game;
begin
  doPlayer;
  doEntities;
  doCamera;
end;

procedure initStage;
begin
  NEW(stage.EntityHead);
  initEntity(stage.EntityHead);
  stage.EntityHead^.next := NIL;
  stage.EntityTail := stage.EntityHead;

  gTicks := SDL_GetTicks;
  gRemainder := 0;
  initEntities;
  initPlayer;
  initMap;
  app.Delegate.Logic := @logic_Game;
  app.Delegate.Draw  := @draw_Game;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := {SDL_RENDERER_PRESENTVSYNC OR} SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  app.Window := SDL_CreateWindow('Pete''s Pizza Party 6', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure destroyTexture;
VAR t, a : PTexture;
begin
  a := app.TextureHead^.next;
  while (a <> NIL) do
  begin
    t := a^.next;
    DISPOSE(a);
    a := t;
  end;
  DISPOSE(app.TextureHead);
end;

procedure destroyEntity;
VAR t, ent : PEntity;
begin
  ent := stage.EntityHead^.next;
  while (ent <> NIL) do
  begin
    t := ent^.next;
    DISPOSE(ent);
    ent := t;
  end;
  DISPOSE(stage.EntityHead);
end;

procedure cleanUp;
begin
  destroyTexture;
  destroyEntity;
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure initGame;
begin
  initTexture;
  initFonts();
  initSounds();
  loadMusic('music/one_0.mp3');
  playMusic(TRUE);
end;

procedure atExit;
VAR i : integer;
begin
  for i := 1 to MAX_TILES do
    SDL_DestroyTexture(Tiles[i]);

  if ExitCode <> 0 then cleanUp;
  Mix_CloseAudio;
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow(app.Window);
  MIX_Quit;   { Quits the Music / Sound }
  IMG_Quit;   { Quits the SDL_Image }
  SDL_Quit;   { Quits the SDL }
  if Exitcode <> 0 then WriteLn(SDL_GetError());
  SDL_ShowCursor(1);
end;

// *****************   INPUT   *****************

procedure doInput;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    CASE event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((event.key.repeat_ = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.Keyboard[event.key.keysym.scancode] := 1;
                     if (app.Keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((event.key.repeat_ = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.Keyboard[event.key.keysym.scancode] := 0;
                   end;   { SDL_Keyup }
    end;  { CASE event }
  end;    { SDL_PollEvent }
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
  PathTest;
  initSDL;
  addExitProc(@atExit);
  initGame;
  initStage;
  exitLoop := FALSE;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    app.delegate.Logic;
    app.delegate.Draw;
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  cleanUp;
  atExit;
end.
