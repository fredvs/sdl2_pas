program PureSDL2Demo3;

{$mode objfpc}{$H+}

uses
  SysUtils, SDL2, SDL2_TTF, SDL2_image, Math;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;
  FONT_SIZE = 24;

var
  window: PSDL_Window = nil;
  renderer: PSDL_Renderer = nil;
  running: Boolean = True;
  font: PTTF_Font = nil;
  label_text: AnsiString = 'Click to move image';
  button_pressed: Boolean = False;
  text_input: AnsiString = '';
  label_texture: PSDL_Texture = nil;
  text_input_texture: PSDL_Texture = nil;
  button_texture: PSDL_Texture = nil;
  image_texture: PSDL_Texture = nil;
  rotated_image_texture: PSDL_Texture = nil;
  button_rect: TSDL_Rect = (x: 200; y: 200; w: 100; h: 30); // Adjusted for smaller button
  image_rect: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
  rotated_image_rect: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
  white_color: TSDL_Color = (r: 255; g: 255; b: 255; a: 255);
  black_color: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);

procedure update_texture(var texture: PSDL_Texture; const text: AnsiString; color: TSDL_Color);
var
  surface: PSDL_Surface;
  trimmedText: AnsiString;
begin
  if Assigned(texture) then
    SDL_DestroyTexture(texture);

  trimmedText := Trim(text);
  
  if Length(trimmedText) = 0 then
    trimmedText := ' ';

  surface := TTF_RenderText_Solid(font, PAnsiChar(trimmedText), color);
  if surface = nil then
    raise Exception.Create('Failed to create text surface: ' + TTF_GetError());
  texture := SDL_CreateTextureFromSurface(renderer, surface);
  SDL_FreeSurface(surface);
end;

procedure load_images;
var
  image_surface: PSDL_Surface;
begin
  // Load first image
  image_surface := IMG_Load('image.png');
  if image_surface = nil then
  begin
    WriteLn('Unable to load image! SDL_image Error: ', IMG_GetError());
    Exit;
  end else
  begin
    WriteLn('First Image loaded successfully.');
  end;

  image_texture := SDL_CreateTextureFromSurface(renderer, image_surface);
  SDL_QueryTexture(image_texture, nil, nil, @image_rect.w, @image_rect.h);

  // Use the same texture for rotation during rendering
  rotated_image_texture := image_texture;
  SDL_QueryTexture(rotated_image_texture, nil, nil, @rotated_image_rect.w, @rotated_image_rect.h);
  
     // Adjust the rectangle for left alignment
    rotated_image_rect.x := image_rect.w + 250 ;// Left align
    rotated_image_rect.y := 0; // Position as before or adjust vertically if needed
   
  SDL_FreeSurface(image_surface);
end;

procedure handle_events;
var
  event: TSDL_Event;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    case event.type_ of
      SDL_MOUSEBUTTONDOWN:
        begin
          if (event.button.x >= button_rect.x) and (event.button.x <= button_rect.x + button_rect.w) and 
             (event.button.y >= button_rect.y) and (event.button.y <= button_rect.y + button_rect.h) then
          begin
            button_pressed := not button_pressed;
            if button_pressed then
            begin
              label_text := 'Button Pressed';
              Inc(rotated_image_rect.x, 30);
              Inc(rotated_image_rect.y, 30);
              rotated_image_rect.x := Math.Min(rotated_image_rect.x, SCREEN_WIDTH - rotated_image_rect.w);
              rotated_image_rect.y := Math.Min(rotated_image_rect.y, SCREEN_HEIGHT - rotated_image_rect.h);
            end
            else
              label_text := 'Click to move image';
            update_texture(label_texture, label_text, white_color);
          end;
        end;
      SDL_TEXTINPUT:
        begin
          text_input := text_input + AnsiString(event.text.text);
          update_texture(text_input_texture, text_input, white_color);
        end;
      SDL_KEYDOWN:
        begin
          if event.key.keysym.sym = SDLK_BACKSPACE then
          begin
            if Length(text_input) > 0 then
            begin
              Delete(text_input, Length(text_input), 1);
              update_texture(text_input_texture, text_input, white_color);
            end;
          end;
        end;
    end;
    
    // Handle SDL_QUIT event outside of case statement
   if event.type_ = SDL_QUITEV then running := False;
  end;
end;

procedure render;
var
  label_rect, input_rect, button_caption_rect: TSDL_Rect;
      rotated_width, rotated_height: integer;
  
begin
  SDL_SetRenderDrawColor(renderer, 50, 50, 50, 255);
  SDL_RenderClear(renderer);

  // Draw images first
  if Assigned(image_texture) then
  begin
    SDL_RenderCopy(renderer, image_texture, nil, @image_rect);
  end;

  // Draw rotated image
  if Assigned(rotated_image_texture) then
  begin
    SDL_RenderCopyEx(renderer, rotated_image_texture, nil, @rotated_image_rect, 90, nil, SDL_FLIP_NONE);
  end;
  
   SDL_QueryTexture(rotated_image_texture, nil, nil, @rotated_width, @rotated_height);
    
  // Button - Draw this after images to ensure it's on top
  SDL_SetRenderDrawColor(renderer, 150, 150, 150, 255);
  SDL_RenderFillRect(renderer, @button_rect);
  
  // Button Caption
  if Assigned(button_texture) then
  begin
    SDL_QueryTexture(button_texture, nil, nil, @button_caption_rect.w, @button_caption_rect.h);
    button_caption_rect.x := button_rect.x + (button_rect.w - button_caption_rect.w) div 2;
    button_caption_rect.y := button_rect.y + (button_rect.h - button_caption_rect.h) div 2;
    SDL_RenderCopy(renderer, button_texture, nil, @button_caption_rect);
  end;

  // Label - This can go anywhere after clearing the screen since it's not overlapping
  if Assigned(label_texture) then
  begin
    SDL_QueryTexture(label_texture, nil, nil, @label_rect.w, @label_rect.h);
    label_rect.x := 200;
    label_rect.y := 170;
    SDL_RenderCopy(renderer, label_texture, nil, @label_rect);
  end;

  // Text Input (Memo)
  SDL_SetRenderDrawColor(renderer, 100, 100, 100, 255);
  input_rect := Default(TSDL_Rect);
  input_rect.x := 50;
  input_rect.y := 300;
  input_rect.w := 700;
  input_rect.h := 200;
  SDL_RenderFillRect(renderer, @input_rect);

  if Assigned(text_input_texture) then
  begin
    SDL_QueryTexture(text_input_texture, nil, nil, @input_rect.w, @input_rect.h);
    input_rect.x := 60;
    input_rect.y := 310;
    SDL_RenderCopy(renderer, text_input_texture, nil, @input_rect);
  end;

  SDL_RenderPresent(renderer);
end;

var
  img_flags: LongInt;

begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_EVENTS) < 0 then
  begin
    WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError());
    Exit;
  end;

  if TTF_Init() = -1 then
  begin
    WriteLn('TTF could not initialize! TTF_Error: ', TTF_GetError());
    Exit;
  end;

  img_flags := IMG_INIT_PNG or IMG_INIT_JPG;
  if (IMG_Init(img_flags) and img_flags) <> img_flags then
  begin
    WriteLn('SDL_image could not initialize! SDL_image Error: ', IMG_GetError());
    Exit;
  end;

  window := SDL_CreateWindow('Pure SDL2 Demo', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
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

  font := TTF_OpenFont('courier.ttf', FONT_SIZE); // Replace with actual path
  if font = nil then
  begin
    WriteLn('Font could not load! TTF_Error: ', TTF_GetError());
    Exit;
  end;

  update_texture(label_texture, label_text, white_color);
  update_texture(text_input_texture, text_input, white_color);
  update_texture(button_texture, 'Click', black_color); // Button caption
  load_images;

  SDL_StartTextInput;
  text_input :='Write something here with the keyboard ';
  update_texture(text_input_texture, text_input, white_color);
    
  while running do
  begin
    handle_events;
    render;
    SDL_Delay(16); // To cap at ~60 FPS
  end;

  SDL_StopTextInput;

  if Assigned(label_texture) then SDL_DestroyTexture(label_texture);
  if Assigned(text_input_texture) then SDL_DestroyTexture(text_input_texture);
  if Assigned(button_texture) then SDL_DestroyTexture(button_texture);
  if Assigned(image_texture) then SDL_DestroyTexture(image_texture);
  if Assigned(rotated_image_texture) then SDL_DestroyTexture(rotated_image_texture);
  TTF_CloseFont(font);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  IMG_Quit;
  TTF_Quit;
  SDL_Quit;
end.