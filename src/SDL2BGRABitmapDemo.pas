program SDL2BGRABitmapDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, SDL2, SDL2_image, BGRABitmap, BGRABitmapTypes, Math;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  running: Boolean = True;
  original_texture, rotated_texture: PSDL_Texture;
  original_rect, rotated_rect: TSDL_Rect;

procedure load_and_process_images;
var
  surface: PSDL_Surface;
  bgraBitmap, rotatedBitmap: TBGRABitmap;
  rotated_surface: PSDL_Surface;
begin
  // Load image with SDL2
  surface := IMG_Load('/home/fred/gork_sdl/image.png');
  if surface = nil then
  begin
    WriteLn('Unable to load image! SDL_image Error: ', IMG_GetError());
    Exit;
  end;

  try
    // Convert SDL_Surface to BGRABitmap for manipulation
    bgraBitmap := TBGRABitmap.Create(surface^.w, surface^.h, BGRAWhite);
    Move(surface^.pixels^, bgraBitmap.Data^, surface^.w * surface^.h * 4);

    // Rotate using BGRABitmap
    rotatedBitmap := bgraBitmap.RotateCW;
    try
      // Convert back to SDL textures
      original_texture := SDL_CreateTextureFromSurface(renderer, surface);
      SDL_QueryTexture(original_texture, nil, nil, @original_rect.w, @original_rect.h);
      original_rect.x := 10;
      original_rect.y := 10;

      // Create a new surface for the rotated image with the same pixel format as the original
      rotated_surface := SDL_CreateRGBSurface(0, rotatedBitmap.Width, rotatedBitmap.Height, 
                                               surface^.format^.BitsPerPixel, 
                                               surface^.format^.Rmask, 
                                               surface^.format^.Gmask, 
                                               surface^.format^.Bmask, 
                                               surface^.format^.Amask);
      if rotated_surface <> nil then
      begin
        // Copy the bitmap data, making sure color channels are in the correct order
        Move(rotatedBitmap.Data^, rotated_surface^.pixels^, rotatedBitmap.NbPixels * 4);
        rotated_texture := SDL_CreateTextureFromSurface(renderer, rotated_surface);
        SDL_QueryTexture(rotated_texture, nil, nil, @rotated_rect.w, @rotated_rect.h);
        rotated_rect.x := 260;
        rotated_rect.y :=  10;
        SDL_FreeSurface(rotated_surface);
      end else
      begin
        WriteLn('Failed to create surface for rotated image: ', SDL_GetError());
      end;
    finally
      rotatedBitmap.Free;
    end;
  finally
    bgraBitmap.Free;
    SDL_FreeSurface(surface);
  end;
end;

procedure handle_events;
var
  event: TSDL_Event;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    case event.type_ of
      SDL_QUITEV: running := False;
    end;
  end;
end;

procedure render;
begin
  SDL_SetRenderDrawColor(renderer, 50, 50, 50, 255);
  SDL_RenderClear(renderer);

  if Assigned(original_texture) then
    SDL_RenderCopy(renderer, original_texture, nil, @original_rect);

  if Assigned(rotated_texture) then
    SDL_RenderCopy(renderer, rotated_texture, nil, @rotated_rect);

  SDL_RenderPresent(renderer);
end;

var
  img_flags: LongInt;

begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError());
    Exit;
  end;

  img_flags := IMG_INIT_PNG or IMG_INIT_JPG;
  if (IMG_Init(img_flags) and img_flags) <> img_flags then
  begin
    WriteLn('SDL_image could not initialize! SDL_image Error: ', IMG_GetError());
    Exit;
  end;

  window := SDL_CreateWindow('SDL2 and BGRABitmap Demo', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
  if window = nil then
  begin
    WriteLn('Window could not be created! SDL_Error: ', SDL_GetError());
    Exit;
  end;

  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if renderer = nil then
  begin
    WriteLn('Renderer could not be created! SDL_Error: ', SDL_GetError());
    Exit;
  end;

  load_and_process_images;

  while running do
  begin
    handle_events;
    render;
    SDL_Delay(16); // Cap at ~60 FPS
  end;

  if Assigned(original_texture) then SDL_DestroyTexture(original_texture);
  if Assigned(rotated_texture) then SDL_DestroyTexture(rotated_texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  IMG_Quit;
  SDL_Quit;
end.