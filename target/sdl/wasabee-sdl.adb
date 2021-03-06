-- with Wasabee.Hypertext;                 use Wasabee.Hypertext;
-- with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
-- with Wasabee.Request;                   use Wasabee.Request;
with Wasabee.Util;                      use Wasabee.Util;

with Wasabee.Hypertext.Parsing ;        use Wasabee.Hypertext.Parsing ;
-- with Wasabee.Hypertext.Locations ;      use Wasabee.Hypertext.Locations ;

-- with Ada.Strings.Wide_Fixed;
-- with Ada.Strings.Unbounded ;            use Ada.Strings.Unbounded ;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;


-- with Ada.Characters.Handling ; use Ada.Characters.Handling ;
-- with Ada.Containers.Vectors ;

with Ada.Text_IO;

-- with DOM.Core;

-- with SDL_SDL_H ; use SDL_SDL_H ;
-- with SDL_SDL_Video_H ; use SDL_SDL_Video_H ;
-- with SDL_SDL_Events_H ; use SDL_SDL_Events_H ;
-- with SDL_SDL_Keyboard_H ; use SDL_SDL_Keyboard_H ;
-- with SDL_SDL_Ttf_H; use SDL_SDL_Ttf_H;
-- with SDL_SDL_Stdinc_H ; use SDL_SDL_Stdinc_H ;

-- with Interfaces ; use Interfaces ;
-- with Interfaces.C ; use Interfaces.C ;
-- with Interfaces.C.Strings ; use Interfaces.C.Strings ;

-- with System ; use System ;
-- with System.Storage_elements ; use System.Storage_elements ;

-- with Ada.Unchecked_Conversion ;



package body Wasabee.SDL is


   function To_SDL_Color (C : Color_Code) return SDL_Color is
      Color : SDL_Color := (125,125,125,0);
      Sr : constant String := Integer'Image(C) ;
   begin
      -- Ada.Text_IO.Put_Line("Selecting colors ..." & Integer'Image(C));
      Color.R := Unsigned_Char (C / 65536) ;
      Color.G := Unsigned_Char ((C / 256) mod 256) ;
      Color.B := Unsigned_Char (C mod 256) ;
      return Color ;
   end ;

   procedure Draw_Pixel (X,Y : Integer) is
      Bit32 : System.Address ;
      Pixel : System.Address ;
      Pitch : Uint16;
      Format : access SDL_PixelFormat ;
      IA : Integer_Address ;
      Lf : Long_Long_Float ;
   begin
      if X < 0 or Y < 0 then
	     return;
      end if;
      --Ada.Text_IO.Put_Line("***************** Draw Pixel *************************");
      --Ada.Text_IO.Put_Line("X: " & Integer'Image(X)) ;
      --Ada.Text_IO.Put_Line("Y: " & Integer'Image(Y)) ;
      Pixel := Window.Surface.Pixels ;
      Pitch := Uint16(Window.Surface.Pitch);
      Format := Window.Surface.Format ;
      Bit32 := Pixel ;

      -- IA := Integer_Address( Long_Float(Bit32) + (Long_Float(X)) * 8 + Long_Float((Pitch / 4) * (Long_Float(Y)*8))) ;

      Lf := Long_Long_Float(To_Integer(Bit32));
      Lf := Lf + Long_Long_Float(X*4) ;
      Lf := Lf + (Long_Long_Float(Pitch) * Long_Long_Float(Y)) ;

      Ia := Integer_Address(Lf);
      declare
         Element : Color_Code;
         for Element'Address use To_Address(IA);
      begin
         Element := Color_Code(Current_Color_Code) ;
      end ;
   end ;


   procedure Clear_area(on: in out SDL_plane) is
      Ret : Int ;
   begin
      Ret := SDL_FillRect (Window.Surface, Window.Surface.Clip_Rect'Access, 16#FFFFFF#) ;
   end;

   procedure Area_size (on: SDL_plane; w,h: out Natural) is
   begin
      W := Natural(Window.Surface.W) ;
      H := Natural(Window.Surface.H) ;
   end;

   procedure Text_at(on: in out SDL_plane; p: Point; text: UTF_16_String) is
      Ts : access SDL_Surface ;
      Font : System.Address ;
      Color : SDL_Color := (0,0,0,0);

      Rect : aliased SDL_Rect ;
      Ret : Int ;
   begin
      if text = "" then
        return;
      end if;
      Font := Current_Font ; -- Ttf_OpenFont(New_String("arial.ttf"),12);
      Ts := TTF_RenderText_Solid (Font,New_String(To_String(Text)),Current_Color) ;
      Rect.X := Sint16(p.X) ;
      Rect.Y := Sint16(p.Y) ;
      Rect.W := Uint16(Ts.W) ;
      Rect.H := Uint16(Ts.H) ;
      Ret := SDL_UpperBlit(Ts,null,Window.Surface,Rect'Access);
   end;

   procedure Text_size (on   : in out SDL_plane;
                        text : in     UTF_16_String;
                        x,y  :    out Natural
                       ) is
      Ts : access SDL_Surface ;
      Font : System.Address ;
      Color : SDL_Color := (0,0,0,0);
      -- F : SDL_PixelFormat ;
   begin
      Font := Current_Font ; --Ttf_OpenFont(New_String("arial.ttf"),12);
      Ts := TTF_RenderText_Solid (Font,New_String(To_String(Text)),Current_Color) ;
      X := Natural(Ts.W) ;
      Y := Natural(Ts.H) ;
   end;

   procedure Create_target_font(
                                on         : in out SDL_plane;
                                descriptor : in     Font_descriptor;
                                new_index  : in     Positive
                               ) is
      Font : System.Address ;
      Style : Uint16 ;
   begin
      -- Ada.Text_IO.Put_Line("Creating font " & Positive'Image(New_Index) & " with size " & Integer'Image(Descriptor.Size));
      Font := Ttf_OpenFont(New_String("arial.ttf"),Int(Descriptor.Size)/10);
      Style := TTF_STYLE_NORMAL;
      if Descriptor.Modifier(Bold) > 0 then
         Style := Style or TTF_STYLE_BOLD ;
      end if ;
      if Descriptor.Modifier(Italic) > 0 then
         Style := Style or TTF_STYLE_ITALIC ;
      end if ;
      if Descriptor.Modifier(Underlined) > 0 then
         Style := Style or TTF_STYLE_UNDERLINE ;
      end if ;
      if Descriptor.Modifier(STRIKETHROUGH) > 0 then
         Style := Style or TTF_STYLE_STRIKETHROUGH ;
      end if ;

      TTF_SetFontStyle(Font, Int(Style));

      SDL_Fonts_Vector.Append(Fonts,Font);
   end ;

   procedure Select_target_font(
                                on         : in out SDL_plane;
                                index      : in     Positive
                               ) is
   begin
      Current_Font := SDL_Fonts_Vector.Element(Fonts,index);
   end ;

   procedure Destroy_target_fonts(on: in out SDL_plane) is
   begin
      Put_Line("Destroyinging font ");
   end ;


   procedure Select_target_fore_color(on: in out SDL_plane;
                                      code: in Color_Code
                                     ) is
   begin
      Current_Color := To_SDL_Color(Code) ;
      Current_Color_Code := Code ;
   end ;

   procedure Select_target_back_color(on: in out SDL_plane;
                                      code: in Color_Code
                                     ) is
   begin
      Current_BG_Color := To_SDL_Color(Code) ;
   end ;

   procedure Draw_Point (On : in out SDL_Plane ; P: Point ; Color : Color_Code) is
      Bit32 : System.Address ;
      Pixel : System.Address ;
      Pitch : Uint16;
      Format : access SDL_PixelFormat ;
      IA : Integer_Address ;
      Lf : Long_Long_Float ;
      -- X : Integer ;
   begin
      --Ada.Text_IO.Put_Line("***************** Draw Pixel *************************");
      --Ada.Text_IO.Put_Line("X: " & Integer'Image(X)) ;
      --Ada.Text_IO.Put_Line("Y: " & Integer'Image(Y)) ;
      Pixel := Window.Surface.Pixels ;
      Pitch := Uint16(Window.Surface.Pitch);
      Format := Window.Surface.Format ;
      Bit32 := Pixel ;

      -- IA := Integer_Address( Long_Float(Bit32) + (Long_Float(X)) * 8 + Long_Float((Pitch / 4) * (Long_Float(Y)*8))) ;
      Lf := Long_Long_Float(To_Integer(Bit32));
      Lf := Lf + (Long_Long_Float(P.X*4)) ;
      Lf := Lf + (Long_Long_Float(Pitch) * Long_Long_Float((P.Y))) ;
      Ia := Integer_Address(Lf);
      declare
         Element : Color_Code;
         for Element'Address use To_Address(IA);
      begin
         Element := Color_Code(Color);
      end ;
      --Ret := SDL_Flip(On.Screen);
   end;

   procedure Flush (On : in out SDL_Plane) is
      Ret : Int;
   begin
      Ret := SDL_Flip(On.Screen);
   end ;


   -- function return ;

   --
   -- C'est parti pour un peu de SDL
   --

   procedure Scroll_Up (F : Integer) is
   begin
      Window.Ypos := Window.YPos + F ;
      if Window.Ypos > 0 then
         Window.Ypos := 0 ;
      end if ;
      -- Ada.Text_IO.Put_Line(Integer'Image(Window.YPos)) ;
   end ;

   procedure Scroll_Down (F : Integer) is
   begin
      Window.Ypos := Window.YPos - F ;
      if Window.Ypos > 0 then
         Window.Ypos := 0 ;
      end if ;
      -- Ada.Text_IO.Put_Line(Integer'Image(Window.YPos)) ;
   end ;

   procedure Init is
      Event : aliased SDL_Event ;

      Rect : aliased SDL_Rect := (0,0,1024,768);

   begin
      Ret := SDL_Init(SDL_INIT_EVERYTHING) ;
      Ret := TTF_Init ;
      Put_Line("Creating window");
      Window.Screen := SDL_SetVideoMode (1024, 1000, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
      Put_Line("Creating surface");
      Window.Surface := SDL_CreateRGBSurface (SDL_HWSURFACE, 1024 , 60000, 32, 0,0,0,0);

      Window.XPos := 0 ;
      Window.YPos := 0 ;

      if Window.Surface = null then
         Put_Line("error");
      end if ;

	  Window.Select_main_background(Object);
      Clear_Area(Window);

      Window.Draw(Object,invisible);
      -- (Area resizing happens here)
      Object.Fit_bounding_boxes;

      Window.Draw(Object,full);
      SDL_WM_SetCaption(
        New_String("Wasabee version " & Version & " - " & To_String(Object.Title)) ,
        New_String("")) ;
      Ret := SDL_UpperBlit(Window.Surface, null, Window.Screen, Rect'access) ;
      Ret := SDL_Flip(Window.Screen);
      loop
         Ret := SDL_PollEvent(Event'Access) ;

         if SDL_EventType(Event.C_Type) = SDL_KEYUP then
            declare
               Ke : SDL_KeyboardEvent ;
               Ks : SDL_Keysym ;
            begin
               Ke := Event.Key ;
               Ks := Ke.Keysym;
               -- Ada.Text_IO.Put_Line(Integer'Image(Integer(Ks.Scancode)));

               if Ks.Scancode = 72 then
                  Scroll_Up(10) ;
               end if ;
               if Ks.Scancode = 73 then
                  Scroll_Up(Integer(Window.Screen.H) - 100) ;
               end if ;

               if Ks.Scancode = 80 then
                  Scroll_Down(10) ;
               end if ;

               if Ks.Scancode = 81 then
                  Scroll_Down(Integer(Window.Screen.H) - 100) ;
               end if ;

               Rect.X := Short(Window.XPos) ;
               Rect.Y := Short(Window.YPos) ;
               Ret := SDL_UpperBlit(Window.Surface, null, Window.Screen, Rect'access) ;
               Ret := SDL_Flip(Window.Screen);

            end ;
         end if ;

         if SDL_EventType(Event.C_Type) = SDL_MOUSEBUTTONUP then
            declare
               Me : SDL_MouseButtonEvent ;
               Url : Unbounded_String ;
            begin
               Me := Event.Button ;
               -- Ada.Text_IO.Put_Line("Button: " & Integer'Image(Integer(Me.Button))) ;

               if Me.Button = 1 then
                  Ada.Text_IO.Put_Line(Mouse_URL(Object,
                                                 (Window.XPos + Integer(Me.X),
                                                 (Window.YPos * (-1)) + Integer(Me.Y)))) ;
                  Url := To_Unbounded_String(Mouse_Url(Object,
                                                               (Window.XPos + Integer(Me.X),
                                                               (Window.YPos * (-1)) + Integer(Me.Y))));
                  if URL /= "" then
                     Wasabee.Request.Retrieve_from_URL (To_String(Url) , HTML);
                     Load_frame(Object , To_String(HTML));
                     Clear_Area(Window);
                     Window.Draw(Object,full);
                  end if ;
               end if ;
               if Me.Button = 4 then
                  Scroll_Up(25) ;
               end if ;
               if Me.Button = 5 then
                  Scroll_Down(25) ;
               end if ;

               Rect.X := Short(Window.XPos) ;
               Rect.Y := Short(Window.YPos) ;
               Ret := SDL_UpperBlit(Window.Surface, null, Window.Screen, Rect'access) ;
               Ret := SDL_Flip(Window.Screen);

            end;
         end if ;

         if SDL_EventType(Event.C_Type) = SDL_MOUSEMOTION then
            declare
               Me : SDL_MouseMotionEvent;
               Mcs : Mouse_Cursor_Style ;
            begin
               Me := Event.Motion ;

               Mcs := Mouse_cursor(Object ,
                                   (Window.XPos + Integer(Me.X),
                                   (Window.YPos * (-1)) + Integer(Me.Y))) ;
               if Mcs = Finger then
                  null ; --Put_Line("=>");
               end if ;

            end ;
         end if ;

         if SDL_EventType(Event.C_Type) = SDL_SDL_Events_H.SDL_QUIT then
            exit ;
         end if ;
         Event.C_Type := Unsigned_Char(SDL_NOEVENT) ;
      end loop ;
   end ;

   procedure Rectangle (on: in out SDL_Plane ; coords: Box) is
      X,Y,W,H : Int ;
      Display_Rect : constant Boolean := True ;
   begin
      X := Int(coords.P1.X) ;
      Y := Int(coords.P1.Y) ;
      W := Int(coords.P2.X - coords.P1.X) ;
      H := Int(coords.P2.Y - coords.P1.Y) ;

      if W = 0 and H = 0 then
         return ;
      end if ;

      -- Put_Line("*****************************************");
      --Ada.Text_IO.Put_Line(Integer'Image(Integer(X)));
      --Ada.Text_IO.Put_Line(Integer'Image(Integer(Y)));
      --Ada.Text_IO.Put_Line(Integer'Image(Integer(W)));
      --Ada.Text_IO.Put_Line(Integer'Image(Integer(H)));

      if H > 0 then
         null;
      else
         null ;
      end if ;

      if Display_Rect then
         for I in X .. X+W loop
            Draw_Pixel(Integer(I), Integer(Y)) ;
            Draw_Pixel(Integer(I), Integer(Y+H)) ;
         end loop;
         for I in Y .. (Y+H) loop
            Draw_Pixel(Integer(X), Integer(I)) ;
            Draw_Pixel(Integer((X+W)), Integer(I)) ;
         end loop;

      end if ;
   exception
      when others =>
        null;
   end ;

   procedure Full_Rectangle (on: in out SDL_Plane ; coords: Box) is
      Ret : Int ;
      Fill_Color : SDL_Color ;
      Fill_Color_32: Uint32;
      rect : aliased SDL_Rect;
   begin
      Fill_Color := Current_BG_Color ;
      -- ada.text_IO.put(coords.P1.X'img);
      if coords.P1.X < 0 then
        return;
      end if;
      rect.X := Sint16(coords.P1.X) ;
      rect.Y := Sint16(coords.P1.Y) ;
      rect.W := Uint16(coords.P2.X - coords.P1.X) ;
      rect.H := Uint16(coords.P2.Y - coords.P1.Y) ;
      Fill_Color_32:= Uint32(Fill_Color.r) + 256 * Uint32(Fill_Color.g) + 65536 * Uint32(Fill_Color.b);
      Ret := SDL_FillRect (Window.Surface, rect'Access, Fill_Color_32) ;
   end Full_Rectangle;

   procedure Put_RGB_Bitmap (
     on     : in out SDL_Plane;
     bitmap :        Wasabee.Images.Bitmap_type;
     coords :        Box
   )
   is
     i: Integer:= 0;
     use Wasabee.Images;
   begin
     if bitmap.data = null then
       return;
     end if;
     -- !! Snail mode drawing !!
     for y in 1..bitmap.height loop
       for x in 1..bitmap.width loop
         Current_Color_Code:=
           Color_Code(bitmap.data(i+2))+
           Color_Code(bitmap.data(i+1))*256+
           Color_Code(bitmap.data(i  ))*65536
         ;
         i:= i + 4;
         -- Draw_Pixel(x-1+coords.p1.x, -y+1+coords.p2.y);
       end loop;
     end loop;
   end Put_RGB_Bitmap;

   procedure Quit is
   begin
      TTF_Quit ;
      SDL_SDL_H.SDL_Quit;
   end ;


end ;
