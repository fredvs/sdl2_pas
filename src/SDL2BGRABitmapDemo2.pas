
(* Dessin avec BGRABitmap dans une fenêtre SDL2 *)
// By Roland Chastain 2025

program SDL2BGRABitmapDemo2;

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$mode objfpc}{$H+}

uses
  SysUtils,
  SDL2,
  BGRABitmap,
  BGRABitmapTypes, // TBGRAPixel_RedShift
  BGRADefaultBitmap; // TBGRAPtrBitmap

const
  CWindowWidth  = 400;
  CWindowHeight = 400;
  
var
  LWindow: PSDL_Window;
  LRenderer: PSDL_Renderer;
  LSurface: PSDL_Surface;
  LTexture: PSDL_Texture;
  LEvent: PSDL_Event;
  LBitmap: TBGRAPtrBitmap;
  LWindowWidth, LWindowHeight, LRendererWidth, LRendererHeight: integer;
  LLoop: boolean = TRUE;
  
begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    Halt;

  LWindow := SDL_CreateWindow('SDL2 and BGRABitmap', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, CWindowWidth, CWindowHeight, SDL_WINDOW_SHOWN);
  if LWindow = nil then
    Halt;

  SDL_GetWindowSize(LWindow, @LWindowWidth, @LWindowHeight);
  WriteLn(Format('LWindowWidth=%d LWindowHeight=%d', [LWindowWidth, LWindowHeight]));
  
  LRenderer := SDL_CreateRenderer(LWindow, -1, 0);
  if LRenderer = nil then
    Halt;
  
  SDL_GetRendererOutputSize(LRenderer, @LRendererWidth, @LRendererHeight);
  WriteLn(Format('LRendererWidth=%d LRendererHeight=%d', [LRendererWidth, LRendererHeight]));
  
  SDL_SetRenderDrawColor(LRenderer, $FF, $A5, $00, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(LRenderer);
  
  (* https://github.com/bgrabitmap/bgrabitmap/issues/263#issuecomment-2606911911 *)
  
  LSurface := SDL_CreateRGBSurface(
    0,
    LRendererWidth,
    LRendererHeight,
    32,
    $FF shl TBGRAPixel_RedShift,
    $FF shl TBGRAPixel_GreenShift,
    $FF shl TBGRAPixel_BlueShift,
    $FF shl TBGRAPixel_AlphaShift
  );
  
  LBitmap := TBGRAPtrBitmap.Create(LSurface^.w, LSurface^.h, LSurface^.pixels);
  
  LBitmap.RectangleAntialias(10, 10, LBitmap.Width - 10, LBitmap.Height - 10, BGRA(0, 0, 255), 10);
  
  LTexture := SDL_CreateTextureFromSurface(LRenderer, LSurface);
  
  LBitmap.Free;
  SDL_FreeSurface(LSurface);

  SDL_RenderCopy(LRenderer, LTexture, nil, nil);
  
  New(LEvent);
  
  while LLoop do
  begin
    while SDL_PollEvent(LEvent) = 1 do
      case LEvent^.type_ of
        SDL_KEYDOWN:
          case LEvent^.key.keysym.sym of
            SDLK_ESCAPE: LLoop := FALSE;
          end;
        SDL_QUITEV:
          LLoop := FALSE;
      end;
    
    SDL_RenderPresent(LRenderer);
    
    SDL_Delay(20);
  end;
  
  Dispose(LEvent);
  
  SDL_DestroyRenderer(LRenderer);
  SDL_DestroyWindow(LWindow);

  SDL_Quit;
end.
