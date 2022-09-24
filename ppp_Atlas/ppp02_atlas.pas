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
* picture atlas integerated
***************************************************************************}

PROGRAM ppp02;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image, SDL2_Mixer, Math, Jsontools, sysutils;

CONST SCREEN_WIDTH      = 1280;            { size of the grafic window }
      SCREEN_HEIGHT     = 720;             { size of the grafic window }
      MAX_TILES         = 12;              { ALLE !! Bilder!!          }
      TILE_SIZE         = 64;
      MAP_WIDTH         = 40;
      MAP_HEIGHT        = 20;
      MAP_RENDER_WIDTH  = 20;
      MAP_RENDER_HEIGHT = 12;
      PLAYER_MOVE_SPEED = 12;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SND_CHANNELS  = 16;

TYPE                                        { "T" short for "TYPE" }
     TAtlasRec   = record
                     Name : string;
                     Rec  : TSDL_Rect;
                     rot  : integer;
                   end;
     TDelegating = Procedure;
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                     Delegate : TDelegate;
                   end;
     TStage      = RECORD
                     camera : TSDL_Point;
                     map : ARRAY[0..PRED(MAP_WIDTH),0..PRED(MAP_HEIGHT)] of integer;
                   end;

VAR app        : TApp;
    stage      : TStage;
    event      : TSDL_EVENT;
    exitLoop   : BOOLEAN;
    gTicks     : UInt32;
    gRemainder : double;
    tiles      : ARRAY[1..MAX_TILES] of string;  //PSDL_Texture;
    a          : array[1..max_Tiles] of TAtlasRec;
    atlas_Te   : PSDL_Texture;

// *****************   UTIL   *****************

procedure errorMessage(Message : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(Message),NIL);
  HALT(1);
end;

// ****************   CAMERA   ****************

procedure doCamera;
begin
  stage.camera.x := MIN(MAX(stage.camera.x, 0), (MAP_WIDTH * TILE_SIZE) - SCREEN_WIDTH);
  stage.camera.y := MIN(MAX(stage.camera.y, 0), (MAP_HEIGHT * TILE_SIZE) - SCREEN_HEIGHT);
end;

procedure doPlayer;
begin
  if ((app.keyboard[SDL_SCANCODE_A] = 1) OR (app.keyboard[SDL_SCANCODE_LEFT] = 1)) then
    stage.camera.x := stage.camera.x - PLAYER_MOVE_SPEED;

  if ((app.keyboard[SDL_SCANCODE_D] = 1) OR (app.keyboard[SDL_SCANCODE_RIGHT] = 1)) then
    stage.camera.x := stage.camera.x + PLAYER_MOVE_SPEED;

  if ((app.keyboard[SDL_SCANCODE_W] = 1) OR (app.keyboard[SDL_SCANCODE_UP] = 1)) then
    stage.camera.y := stage.camera.y - PLAYER_MOVE_SPEED;

  if ((app.keyboard[SDL_SCANCODE_S] = 1) OR (app.keyboard[SDL_SCANCODE_DOWN] = 1)) then
    stage.camera.y := stage.camera.y + PLAYER_MOVE_SPEED;
end;

// *****************   DRAW   *****************

procedure blitAtlasImage(name : string; x, y, center : integer);
VAR dest1, dest2 : TSDL_Rect;
    i : integer;
    found : boolean;
begin
  i := 0; found := FALSE;
  repeat
    INC(i);
    if name = a[i].name then found := TRUE;
  until (found = TRUE) or (i = Max_Tiles);

  dest1.x := a[i].rec.x;  dest1.w := a[i].rec.w;
  dest1.y := a[i].rec.y;  dest1.h := a[i].rec.h;
  dest2.x := x;
  dest2.y := y;
  dest2.w := dest1.w;
  dest2.h := dest1.h;

  if center <> 0 then
  begin
    dest2.x := dest2.w DIV 2;
    dest2.y := dest2.h DIV 2;
  end;

  if found = TRUE then
    SDL_RenderCopy(app.Renderer, atlas_te, @dest1, @dest2);
end;

// ****************   TEXTURE   ***************

procedure loadTiles;
VAR i, max : integer;
    N,C : TJsonNode;
    dest1 : TSDL_Rect;
begin
  i:=1;
  if FileExists('data/atlas.json') then
  begin
    //Get the JSON data
    N := TJsonNode.Create;
    N.LoadFromFile('data/atlas.json');

    for c in n do
    begin
      a[i].name  := c.Find('filename').AsString;
      a[i].rec.x := c.Find('x').AsInteger;
      a[i].rec.y := c.Find('y').AsInteger;
      a[i].rec.w := c.Find('w').AsInteger;
      a[i].rec.h := c.Find('h').AsInteger;
      a[i].rot   := c.Find('rotated').AsInteger;

      INC(i);
    end;
    N.free;
  end
  else
  begin writeln('File not found!'); Halt(1); end;

  atlas_te := IMG_LoadTexture(app.Renderer, PChar('gfx/atlas.png'));
  if atlas_te = NIL then
    errorMessage(SDL_GetError());
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
VAR x, y, n, x1, x2, y1, y2, mx, my : integer;
    filename : string;
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
        begin
          filename := 'gfx/tile' + IntToStr(n) + '.png';
          blitAtlasImage(filename, x, y, 0);
        end;
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
      x := 0;                     // first tile of the line
      a := '';                    // new string / number
      readln(FileIn,line);
      le := length(line);

      for i := 1 to le do         // parse through the line
      begin
        if line[i] <> ' ' then    // if line[i] is a number and not space
        begin
          a := a + line[i];       // add number to the other numbers
          if i = le then          // end of line, so add the last number!
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
  doPlayer;
  doCamera;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Pete''s Pizza Party 2', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  SDL_SetHint(SDL_HINT_RENDER_BATCHING, '1');
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure atExit;
begin
  SDL_DestroyTexture (atlas_TE);
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

procedure doInput;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    CASE event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((event.key._repeat = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[event.key.keysym.scancode] := 1;
                     if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((event.key._repeat = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[event.key.keysym.scancode] := 0;
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
  initSDL;
  initMap;
  addExitProc(@atExit);
  exitLoop := FALSE;
  app.Delegate.Logic := @logic_Game;
  app.Delegate.Draw  := @draw_Game;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    app.delegate.Logic;
    app.delegate.Draw;
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  atExit;
end.
