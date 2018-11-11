with Wasabee.Colors;                    use Wasabee.Colors;
with Wasabee.Encoding;                  use Wasabee.Encoding;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Images;
with Wasabee.Request;                   use Wasabee.Request;

with Wasabee.Hypertext.Locations ;      use Wasabee.Hypertext.Locations ;

-- with Ada.Strings.Wide_Fixed;
with Ada.Strings.Unbounded ;            use Ada.Strings.Unbounded ;
with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;


with Ada.Characters.Handling ; use Ada.Characters.Handling ;
with Ada.Containers.Vectors ;

-- with Ada.Text_IO;

-- with DOM.Core;

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


package Wasabee.SDL is

   function To_SDL_Color (C : Color_Code) return SDL_Color ;

   package SDL_Fonts_Vector is new Ada.Containers.Vectors(Positive,System.Address,"=") ;

   Fonts : SDL_Fonts_Vector.Vector ;

   Current_Font : System.Address ;

   Current_Color : SDL_Color ;
   Current_BG_Color : SDL_Color ;

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
   overriding procedure Text_at(on: in out SDL_plane; p: Point; text: UTF_16_String);
   overriding procedure Text_size (
                                   on   : in out SDL_plane;
                                   text : in     UTF_16_String;
                                   x,y  :    out Natural
                                  );
   overriding procedure Select_target_fore_color(
                                                 on: in out SDL_plane;
                                                 code: in Color_Code
                                                ) ;

   overriding procedure Select_target_back_color(
                                                 on: in out SDL_plane;
                                                 code: in Color_Code
                                                ) ;

   overriding procedure Rectangle (on: in out SDL_Plane ; coords: Box);

   overriding procedure Full_Rectangle (on: in out SDL_Plane ; coords: Box);

   overriding
   procedure Put_RGB_Bitmap (
     on     : in out SDL_Plane;
     bitmap :        Wasabee.Images.Bitmap_type;
     coords :        Box
   );

   procedure Draw_Point (On : in out SDL_Plane ; P: Point ; Color : Color_Code) ;

   procedure Flush (On : in out SDL_Plane) ;

   procedure Init ;

   procedure Quit ;

   -- Implementation
   --

   -- ( !! Ouch, global variables! )

   Window : SDL_Plane ;

   ret : Int ;

   HTML : Unbounded_String;

   Object : HT_object;

end ;
