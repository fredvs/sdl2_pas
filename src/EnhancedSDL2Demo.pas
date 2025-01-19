program EnhancedSDL2Demo;

{$mode objfpc}{$H+}

uses
  SysUtils, SDL2, SDL2_TTF, SDL2_image, Math;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;
  UI_FONT_SIZE = 24; // General UI font size
  MEMO_FONT_SIZE = 20; // Smaller font size for memo
  FRAME_THICKNESS = 2;
  RADIUS = 5; // Radius for rounded corners

var
  window: PSDL_Window = nil;
  renderer: PSDL_Renderer = nil;
  running: Boolean = True;
  ui_font: PTTF_Font = nil;
  memo_font: PTTF_Font = nil;
  label_text: AnsiString = 'Click to move image';
  button_pressed: Boolean = False;
  text_input: AnsiString = '';
  label_texture: PSDL_Texture = nil;
  text_input_texture: PSDL_Texture = nil;
  button_texture: PSDL_Texture = nil;
  image_texture: PSDL_Texture = nil;
  rotated_image_texture: PSDL_Texture = nil;
  button_rect: TSDL_Rect = (x: 200; y: 200; w: 100; h: 30);
  image_rect: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
  rotated_image_rect: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
  input_rect: TSDL_Rect = (x: 50; y: 300; w: 700; h: 200);
  white_color: TSDL_Color = (r: 255; g: 255; b: 255; a: 255);
  black_color: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);
  memo_has_focus: Boolean = False;
  mouse_over_button: Boolean = False;

procedure update_texture(var texture: PSDL_Texture; const text: AnsiString; color: TSDL_Color; font: PTTF_Font);
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
  image_surface := IMG_Load('image.png');
  if image_surface = nil then
  begin
    WriteLn('Unable to load image! SDL_image Error: ', IMG_GetError());
    Exit;
  end else
  begin
  //  WriteLn('First Image loaded successfully.');
  end;

  image_texture := SDL_CreateTextureFromSurface(renderer, image_surface);
  SDL_QueryTexture(image_texture, nil, nil, @image_rect.w, @image_rect.h);

  rotated_image_texture := image_texture;
  SDL_QueryTexture(rotated_image_texture, nil, nil, @rotated_image_rect.w, @rotated_image_rect.h);
  
  rotated_image_rect.x := image_rect.w + 250;
  rotated_image_rect.y := 0;
  
  SDL_FreeSurface(image_surface);
end;

procedure draw_rounded_rect(x, y, w, h: Integer; r: Integer; color: TSDL_Color);
var
  i: Integer;
  rect1, rect2: TSDL_Rect;
begin
  SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
  
  // Draw the corners
  for i := 0 to r do
  begin
    SDL_RenderDrawLine(renderer, x + r - i, y + i, x + w - r + i, y + i); // Top line
    SDL_RenderDrawLine(renderer, x + i, y + r, x + i, y + h - r); // Left line
    SDL_RenderDrawLine(renderer, x + r - i, y + h - i, x + w - r + i, y + h - i); // Bottom line
    SDL_RenderDrawLine(renderer, x + w - i, y + r, x + w - i, y + h - r); // Right line
  end;
  
  // Fill the center
  rect1 := Default(TSDL_Rect);
  rect2 := Default(TSDL_Rect);
  rect1.x := x + r;
  rect1.y := y;
  rect1.w := w - 2 * r;
  rect1.h := h;
  rect2.x := x;
  rect2.y := y + r;
  rect2.w := w;
  rect2.h := h - 2 * r;
  SDL_RenderFillRect(renderer, @rect1);
  SDL_RenderFillRect(renderer, @rect2);
end;

procedure handle_events;
var
  event: TSDL_Event;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    mouse_over_button := (event.button.x >= button_rect.x) and (event.button.x <= button_rect.x + button_rect.w) and 
                         (event.button.y >= button_rect.y) and (event.button.y <= button_rect.y + button_rect.h);
    case event.type_ of
      SDL_MOUSEMOTION:
        begin
          // No need to change color here since it's done in render procedure
        end;
      SDL_MOUSEBUTTONDOWN:
        begin
          if mouse_over_button then
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
            update_texture(label_texture, label_text, white_color, ui_font);
          end;
        end;
      SDL_MOUSEBUTTONUP:
        begin
          if (event.button.x >= input_rect.x) and (event.button.x <= input_rect.x + input_rect.w) and 
             (event.button.y >= input_rect.y) and (event.button.y <= input_rect.y + input_rect.h) then
            memo_has_focus := not memo_has_focus;
            
          // Control text input based on focus
          if memo_has_focus then
            SDL_StartTextInput
          else
            SDL_StopTextInput;
        end;
      SDL_TEXTINPUT:
        begin
          if memo_has_focus then
          begin
            text_input := text_input + AnsiString(event.text.text);
            update_texture(text_input_texture, text_input, white_color, memo_font);
          end;
        end;
      SDL_KEYDOWN:
        begin
          if memo_has_focus then
          begin
            if event.key.keysym.sym = SDLK_BACKSPACE then
            begin
              if Length(text_input) > 0 then
              begin
                Delete(text_input, Length(text_input), 1);
                update_texture(text_input_texture, text_input, white_color, memo_font);
              end;
            end;
          end;
        end;
    end;
    
    if event.type_ = SDL_QUITEV then running := False;
  end;
end;

procedure render;
var
  label_rect, button_caption_rect, text_area_rect: TSDL_Rect;
  rotated_width, rotated_height: integer;
  button_color : TSDL_Color = (r: 200; g: 200; b: 200; a: 255);
  button_color2 : TSDL_Color = (r: 150; g: 150; b: 150; a: 255);
  memo_color : TSDL_Color = (r: 100; g: 100; b: 100; a: 255);
 
begin
  SDL_SetRenderDrawColor(renderer, 50, 140, 50, 255);
  SDL_RenderClear(renderer);

  if Assigned(image_texture) then
    SDL_RenderCopy(renderer, image_texture, nil, @image_rect);

  if Assigned(rotated_image_texture) then
  begin
    SDL_QueryTexture(rotated_image_texture, nil, nil, @rotated_width, @rotated_height);
    SDL_RenderCopyEx(renderer, rotated_image_texture, nil, @rotated_image_rect, 90, nil, SDL_FLIP_NONE);
  end;

  // Button with rounded frame
  if mouse_over_button then
    draw_rounded_rect(button_rect.x, button_rect.y, button_rect.w, button_rect.h, RADIUS, button_color)
  else
    draw_rounded_rect(button_rect.x, button_rect.y, button_rect.w, button_rect.h, RADIUS, button_color2);

  if Assigned(button_texture) then
  begin
    SDL_QueryTexture(button_texture, nil, nil, @button_caption_rect.w, @button_caption_rect.h);
    button_caption_rect.x := button_rect.x + (button_rect.w - button_caption_rect.w) div 2;
    button_caption_rect.y := button_rect.y + (button_rect.h - button_caption_rect.h) div 2;
    SDL_RenderCopy(renderer, button_texture, nil, @button_caption_rect);
  end;

  if Assigned(label_texture) then
  begin
    SDL_QueryTexture(label_texture, nil, nil, @label_rect.w, @label_rect.h);
    label_rect.x := 200;
    label_rect.y := 170;
    SDL_RenderCopy(renderer, label_texture, nil, @label_rect);
  end;

  // Memo with rounded frame
  draw_rounded_rect(input_rect.x, input_rect.y, input_rect.w, input_rect.h, RADIUS, memo_color);
{
  if Assigned(text_input_texture) then
  begin
    text_area_rect := input_rect;
    text_area_rect.x := input_rect.x + 5; // Little padding inside the memo
    text_area_rect.y := input_rect.y + 5;
    text_area_rect.w := input_rect.w - 10; // Adjust for padding on both sides
    text_area_rect.h := input_rect.h - 10; // Adjust for padding on top and bottom
    SDL_RenderCopy(renderer, text_input_texture, nil, @text_area_rect);
  end;
}
  
   if Assigned(text_input_texture) then
  begin
    text_area_rect := input_rect;
    text_area_rect.x := input_rect.x + 5; // Little padding inside the memo
    text_area_rect.y := input_rect.y + 5;
    text_area_rect.w := input_rect.w - 10; // Adjust for padding on both sides
    text_area_rect.h := input_rect.h - 10; // Adjust for padding on top and bottom
    SDL_QueryTexture(text_input_texture, nil, nil, @text_area_rect.w, @text_area_rect.h);
    SDL_RenderCopy(renderer, text_input_texture, nil, @text_area_rect);
  end;
  
{

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
}
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

  window := SDL_CreateWindow('Enhanced SDL2 Demo', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
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

  ui_font := TTF_OpenFont('courier.ttf', UI_FONT_SIZE); // Replace with actual path
  if ui_font = nil then
  begin
    WriteLn('UI Font could not load! TTF_Error: ', TTF_GetError());
    Exit;
  end;
  
  memo_font := TTF_OpenFont('courier.ttf', MEMO_FONT_SIZE); // Replace with actual path
  if memo_font = nil then
  begin
    WriteLn('Memo Font could not load! TTF_Error: ', TTF_GetError());
    Exit;
  end;

  update_texture(label_texture, label_text, white_color, ui_font);
  update_texture(text_input_texture, text_input, white_color, memo_font);
  update_texture(button_texture, 'Click', black_color, ui_font); 
  load_images;

  // Don't start text input by default, only when memo has focus
  SDL_StopTextInput;
  text_input := 'Write something here with the keyboard ';
  update_texture(text_input_texture, text_input, white_color, memo_font);
    
  while running do
  begin
    handle_events;
    render;
    SDL_Delay(16); // To cap at ~60 FPS
  end;

  // Cleanup
  if Assigned(label_texture) then SDL_DestroyTexture(label_texture);
  if Assigned(text_input_texture) then SDL_DestroyTexture(text_input_texture);
  if Assigned(button_texture) then SDL_DestroyTexture(button_texture);
  if Assigned(image_texture) then SDL_DestroyTexture(image_texture);
  if Assigned(rotated_image_texture) then SDL_DestroyTexture(rotated_image_texture);
  TTF_CloseFont(ui_font);
  TTF_CloseFont(memo_font);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  IMG_Quit;
  TTF_Quit;
  SDL_Quit;
end.