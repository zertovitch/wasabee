with Wasabee;                           use Wasabee;

with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Request;                   use Wasabee.Request;

with Wasabee.Hypertext.Locations ;      use Wasabee.Hypertext.Locations ;

with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;


with Ada.Characters.Handling ; use Ada.Characters.Handling ;
with Ada.Containers.Vectors ;

with Ada.Text_IO;

with DOM.Core;

with SDL_SDL_H ; use SDL_SDL_H ;
with SDL_SDL_Video_H ; use SDL_SDL_Video_H ;
with SDL_SDL_Events_H ; use SDL_SDL_Events_H ;
with SDL_SDL_Keyboard_H ; use SDL_SDL_Keyboard_H ;
with SDL_SDL_Ttf_H; use SDL_SDL_Ttf_H;
with SDL_SDL_Stdinc_H ; use SDL_SDL_Stdinc_H ;

with Interfaces ; use Interfaces ;
with Interfaces.C ; use Interfaces.C ;
with Interfaces.C.Strings ; use Interfaces.C.Strings ;

with System ; use System ;
with System.Storage_elements ; use System.Storage_elements ;

with Ada.Unchecked_Conversion ;
procedure Wasabee_Sdl is


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

   package SDL_Fonts_Vector is new Ada.Containers.Vectors(Positive,System.Address,"=") ;

   Fonts : SDL_Fonts_Vector.Vector ;

   Current_Font : System.Address ;

   Current_Color : SDL_Color ;

   Current_Color_Code : Color_Code ;

   --
   -- Declaration
   --

   type SDL_Plane is new Frame_Plane with record
      Surface : access SDL_Surface ;
      Screen : access SDL_Surface ;
      XPos : Integer ;
      YPos : Integer ;
   end record ;



   overriding procedure Clear_area(on: in out SDL_plane);
   overriding procedure Area_size (on: SDL_plane; w,h: out Natural);
   overriding procedure Extend_area_height (on: in out SDL_plane; to: Natural) is null;

   overriding procedure Create_target_font(
                                           on         : in out SDL_plane;
                                           descriptor : in     Font_descriptor;
                                           new_index  : in     Positive
                                          ) ;
   overriding procedure Select_target_font(
                                           on         : in out SDL_plane;
                                           index      : in     Positive
                                          ) ;
   overriding procedure Destroy_target_fonts(on: in out SDL_plane) ;
   overriding procedure Text_XY(on: in out SDL_plane; x,y: Integer; text: UTF_16_String);
   overriding procedure Text_size (
                                   on   : in out SDL_plane;
                                   text : in     UTF_16_String;
                                   x,y  :    out Natural
                                  );
   overriding procedure Select_target_text_color(
                                                 on: in out SDL_plane;
                                                 code: in Color_Code
                                                ) ;

   procedure Rectangle (on: in out SDL_Plane ; coords: Box);
   --
   -- Implementation
   --
   Window : SDL_Plane ;

   use SDL_Fonts_Vector ;

   procedure Draw_Pixel (X,Y : Integer) is
      Bit32 : System.Address ;
      Pixel : System.Address ;
      Pitch : Uint16;
      Format : access SDL_PixelFormat ;


      IA : Integer_Address ;
      Lf : Long_Long_Float ;
   begin

      --Ada.Text_IO.Put_Line("***************** Draw Pixel *************************");
      --Ada.Text_IO.Put_Line("X: " & Integer'Image(X)) ;
      --Ada.Text_IO.Put_Line("Y: " & Integer'Image(Y)) ;
      Pixel := Window.Surface.Pixels ;
      Pitch := Uint16(Window.Screen.Pitch);
      Format := Window.Screen.Format ;
      Bit32 := Pixel ;

      -- IA := Integer_Address( Long_Float(Bit32) + (Long_Float(X)) * 8 + Long_Float((Pitch / 4) * (Long_Float(Y)*8))) ;

      Lf := Long_Long_Float(To_Integer(Bit32));

      Lf := Lf + Long_Long_Float(X*8) ;

      Lf := Lf + Long_Long_Float(Pitch / 8) * Long_Long_Float(Y*8) ;

      Ia := Integer_Address(Lf);
      declare
         Element : Uint64;
         for Element'Address use To_Address(IA);
      begin
         Element := Uint64(Current_Color_Code) ;
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

   procedure Text_XY(on: in out SDL_plane; x,y: Integer; text: UTF_16_String) is
      Ts : access SDL_Surface ;
      Font : System.Address ;
      Color : SDL_Color := (0,0,0,0);

      Rect : aliased SDL_Rect ;
      Ret : Int ;
   begin
      Font := Current_Font ; -- Ttf_OpenFont(New_String("arial.ttf"),12);
      Ts := TTF_RenderText_Solid (Font,New_String(To_String(Text)),Current_Color) ;
      Rect.X := Sint16(X) ;
      Rect.Y := Sint16(Y) ;
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
      Font := Ttf_OpenFont(New_String("arial.ttf"),Int(Descriptor.Size));
      Style := TTF_STYLE_NORMAL;
      if Descriptor.Modifier(Bold) = True then
         Style := Style or TTF_STYLE_BOLD ;
      end if ;
      if Descriptor.Modifier(Italic) = True then
         Style := Style or TTF_STYLE_ITALIC ;
      end if ;
      if Descriptor.Modifier(Underlined) = True then
         Style := Style or TTF_STYLE_UNDERLINE ;
      end if ;
      if Descriptor.Modifier(STRIKETHROUGH) = True then
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


   procedure Select_target_text_color(on: in out SDL_plane;
                                      code: in Color_Code
                                     ) is
   begin
      Current_Color := To_SDL_Color(Code) ;
      Current_Color_Code := Code ;
   end ;

   Xhtml : DOM.Core.Node_List;
   o: HT_object;

   -- function return ;
   ret : Int ;

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
      Window.Screen := SDL_SetVideoMode (1280, 800, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
      Put_Line("Creating surface");
      Window.Surface := SDL_CreateRGBSurface (SDL_HWSURFACE, 1280,8000, 32, 0,0,0,0);

      Window.XPos := 0 ;
      Window.YPos := 0 ;

      if Window.Surface = null then
         Put_Line("error");
      end if ;

      Clear_Area(Window);

      Window.Draw(O,full);
      SDL_WM_SetCaption(New_String("Wasabee version 0.0.1 - " & Argument(1)) , New_String("")) ;
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
            begin
               Me := Event.Button ;
               -- Ada.Text_IO.Put_Line("Button: " & Integer'Image(Integer(Me.Button))) ;

               if Me.Button = 1 then
                  Ada.Text_IO.Put_Line(Mouse_Partial_Url(O,
                                                         Window.XPos + Integer(Me.X),
                                                         (Window.YPos * (-1)) + Integer(Me.Y))) ;
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

               Mcs := Mouse_cursor(o ,
                                   Window.XPos + Integer(Me.X),
                                   (Window.YPos * (-1)) + Integer(Me.Y)) ;
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
      Display_Rect : constant Boolean := false ;
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

      -- Draw_Pixel(0,0);

      if H > 0 then
         null;
      else
         null ;
      end if ;

      if Display_Rect then

         for I in X .. X+W loop
            Draw_Pixel(Integer(I/2), Integer(Y)) ;
            Draw_Pixel(Integer(I/2), Integer(Y+H)) ;
         end loop;

         for I in Y .. (Y+H) loop
            Draw_Pixel(Integer(X/2), Integer(I)) ;
            Draw_Pixel(Integer((X+W)/2), Integer(I)) ;
         end loop;

      end if ;
   exception
      when others =>
        null;


   end ;

   procedure Quit is
   begin
      TTF_Quit ;
      SDL_SDL_H.SDL_Quit;
   end ;

begin
   --
   -- Ouvrir une fenetre
   --
   if Argument_Count = 0 then
      Put_Line(Standard_Error, "Provide an URL as command-line argument");
   else
    Wasabee.Request.Open_Url (Argument(1), Xhtml);
    Load_frame(o, Xhtml);
    Init ;
    Quit ;
  end if;

end ;


