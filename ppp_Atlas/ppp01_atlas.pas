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
* optimized picture atlas integrated
* code slightly reorganized
* without momory holes; tested with: fpc -Criot -gl -gh ppp01_atlas.pas
***************************************************************************}

PROGRAM ppp01_Atlas;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES SDL2, SDL2_Image, JsonTools, sysutils;

CONST SCREEN_WIDTH      = 1280;            { size of the grafic window }
      SCREEN_HEIGHT     = 720;             { size of the grafic window }
      TILE_SIZE         = 64;
      MAP_WIDTH         = 40;
      MAP_HEIGHT        = 20;
      MAP_RENDER_WIDTH  = 20;
      MAP_RENDER_HEIGHT = 12;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SND_CHANNELS  = 16;
      NUMATLASBUCKETS   = 17;
      MAX_TILES         = 7;               { Anz. Tiles in der Map }
      Map_Path          = 'data/map01.dat';
      Json_Path         = 'data/atlas.json';
      Tex_Path          = 'gfx/atlas.png';

TYPE                                       { "T" short for "TYPE" }
      String255   = String[255];           { max. length of Path + Filename }
      PAtlasImage = ^TAtlasImage;
      TAtlasImage = RECORD
                      FNam : String255;
                      Rec  : TSDL_Rect;
                      Rot  : integer;
                      Tex  : PSDL_Texture;
                      next : PAtlasImage;
                    end;
      TDelegating = Procedure;
      TDelegate   = RECORD
                      logic, draw : TDelegating;
                    end;
      TApp        = RECORD
                      Window   : PSDL_Window;
                      Renderer : PSDL_Renderer;
                      keyboard : ARRAY[0..MAX_KEYBOARD_KEYS] OF integer;
                      Delegate : TDelegate;
                    end;
      TStage      = RECORD
                      map : ARRAY[0..PRED(MAP_WIDTH), 0..PRED(MAP_HEIGHT)] of integer;
                    end;
      AtlasArr    = ARRAY[0..NUMATLASBUCKETS] of PAtlasImage;

VAR   app         : TApp;
      stage       : TStage;
      event       : TSDL_Event;
      exitLoop    : Boolean;
      gTicks      : UInt32;
      gRemainder  : double;
      atlasTex    : PSDL_Texture;
      atlases     : AtlasArr;
      tilesArr    : ARRAY[1..Max_Tiles] of PAtlasImage;

// *****************   UTIL   *****************

procedure initAtlasImage(VAR e : PAtlasImage);
begin
  e^.FNam := ''; e^.Rot := 0; e^.Tex := NIL; e^.next := NIL;
end;

function HashCode(Value : String255) : UInt32;     // DJB hash function
VAR i, x, Result : UInt32;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
  HashCode := Result;
end;

procedure errorMessage(Message1 : String);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(Message1),NIL);
  HALT(1);
end;

procedure pathTest;
begin
  if NOT FileExists(Map_Path) then ErrorMessage(Map_Path + ' nicht gefunden!');
  if NOT FileExists(Json_Path) then ErrorMessage(Json_Path + ' nicht gefunden!');
  if NOT FileExists(Tex_Path) then ErrorMessage(Tex_Path + ' nicht gefunden!');
end;

// *****************   DRAW   *****************

procedure blitRect(texture : PSDL_Texture; src : PSDL_Rect; x, y : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, texture, src, @dest);
end;

procedure blitAtlasImage(atlas : PAtlasImage; x, y, center : integer);
VAR dest : TSDL_Rect;
    p : TSDL_Point;
begin
  dest.x := x;
  dest.y := y;
  dest.w := atlas^.Rec.w;
  dest.h := atlas^.Rec.h;

  if atlas^.Rot = 0 then
  begin
    if center <> 0 then
    begin
      dest.x := dest.x - (dest.w DIV 2);
      dest.y := dest.y - (dest.h DIV 2);
    end;

    SDL_RenderCopy(app.Renderer, atlas^.Tex, @atlas^.Rec, @dest);
  end
  else
  begin
    if center <> 0 then
    begin
      dest.x := dest.x - (dest.h DIV 2);
      dest.y := dest.y - (dest.w DIV 2);
    end;
    p.x := 0;
    p.y := 0;
    dest.y := dest.y + atlas^.Rec.w;

    SDL_RenderCopyEx(app.Renderer, atlas^.Tex, @atlas^.Rec, @dest, -90, @p, SDL_FLIP_NONE);
  end;
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

function getAtlasImage(filename : String255) : PAtlasImage;
VAR a : PAtlasImage;
    i : UInt32;
begin
  i := HashCode(filename) MOD NUMATLASBUCKETS;
  a := atlases[i]^.next;
  getAtlasImage := NIL;
  while (a <> NIL) do
  begin
    if a^.fnam = filename then
      getAtlasImage := a;

    a := a^.next;
  end;
end;

procedure loadAtlasTexture;
begin
  atlasTex := IMG_LoadTexture(app.Renderer, Tex_Path);
  if atlasTex = NIL then
    errorMessage(SDL_GetError());
end;

procedure loadAtlasData;
VAR i, x, y, w, h, r : integer;
    a, AtlasNew : PAtlasImage;
    N, C : TJsonNode;
    filename : String255;
begin
  if FileExists(Json_Path) then
  begin
    //Get the JSON data
    N := TJsonNode.Create;
    N.LoadFromFile(Json_Path);

    for c in n do
    begin
      filename  := c.Find('filename').AsString;
      x := c.Find('x').AsInteger;
      y := c.Find('y').AsInteger;
      w := c.Find('w').AsInteger;
      h := c.Find('h').AsInteger;
      r := c.Find('rotated').AsInteger;

      i := HashCode(filename) MOD NUMATLASBUCKETS;

      a := atlases[i];            // must be created and initialized before!

      while (a^.next <> NIL) do
        begin a := a^.next; end;

      NEW(AtlasNEW);
      initAtlasImage(AtlasNEW);

      AtlasNEW^.Fnam := filename;
      AtlasNEW^.Rec.x := x;
      AtlasNEW^.Rec.y := y;
      AtlasNEW^.Rec.w := w;
      AtlasNEW^.Rec.h := h;
      AtlasNEW^.Rot   := r;
      AtlasNEW^.Tex   := atlasTex;
      AtlasNEW^.next  := NIL;

      a^.next := atlasNEW;
    end;
    N.free;
  end
  else
  errorMessage('Atlas-Json not found!');
end;

procedure initTiles;
VAR i : integer;
    filename : string255;
begin
  for i := 1 to Max_Tiles do
  begin
    filename := 'gfx/tile' + IntToStr(i) + '.png';
    tilesArr[i] := getAtlasImage(filename);
  end;
end;

procedure initAtlas;
VAR i : integer;
begin
  for i := 0 to NUMATLASBUCKETS do
  begin
    NEW(atlases[i]);
    initAtlasImage(atlases[i]);                // create and initialize PAtlasImage
  end;

  loadAtlasTexture;
  loadAtlasData;
  initTiles;
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
        blitAtlasImage(tilesArr[n], x * TILE_SIZE, y * TILE_SIZE, 0);
      end;
    end;
  end;
end;

procedure loadMap(filename : String255);
VAR i, x, y, le : integer;
    FileIn : Text;
    line : String255;
    a : String[10];
begin
  assign (FileIn, filename);
  {$i-}; reset(FileIn); {$i+};
  if IOresult = 0 then
  begin
    for y := 0 to PRED(MAP_HEIGHT) do
    begin
      x := 0;                               // first tile of the line
      a := '';                              // new String / number
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
          a := '';                          // new String / number
        end;
      end;
    end;
    close(FileIn);
  end
  else errorMessage(filename + ' not found!');
end;

procedure initMap;
begin
  FillChar(stage.map, SizeOf(stage.map), 0);
  loadMap(Map_Path);
end;

// *****************   STAGE   *****************

procedure draw_Game;
begin
  //SDL_SetRenderDrawColor(app.Renderer, 128, 192, 255, 255);
  //SDL_RenderFillRect(app.Renderer, NIL);
  drawMap;
end;

procedure logic_Game;
begin
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := {SDL_RENDERER_PRESENTVSYNC OR} SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Pete''s Pizza Party 1 with Atlas', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  SDL_SetHint(SDL_HINT_RENDER_BATCHING, '1');
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
  if Exitcode <> 0 then WriteLn(SDL_GetError());
end;

procedure emptyArray;
VAR i : integer;
    c, b : PAtlasImage;
begin
  for i := 0 to NUMATLASBUCKETS do
  begin
    c := atlases[i]^.next;    // Dispose the list
    while (c <> NIL) do
    begin
      b := c^.next;
      DISPOSE(c);
      c := b;
    end;
    DISPOSE(atlases[i]);      // Dispose element / header of the array
  end;
end;

procedure atExit;
begin
  SDL_DestroyTexture(atlasTex);
  if ExitCode <> 0 then emptyArray;
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow(app.Window);
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
  pathTest;
  initSDL;
  addExitProc(@atExit);
  gTicks := SDL_GetTicks;
  initAtlas;
  initMap;
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

  emptyArray;
  atExit;
end.
