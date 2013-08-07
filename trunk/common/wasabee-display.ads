with Wasabee.Hypertext;                 use Wasabee.Hypertext;

with Ada.Finalization;

package Wasabee.Display is

  subtype Color_Code is Natural; -- 24-bit RGB
  Black: constant Color_Code:= 0;
  White: constant Color_Code:= 16#FF_FF_FF#;

  subtype Font_face_name is String(1..32); -- max is 32 on a famous system

  type Font_descriptor is record
    face       : Font_face_name;
    size       : Positive;
    bold       : Boolean;
    italic     : Boolean;
    underlined : Boolean;
  end record;

  ------------------------------------------------
  -- The main object type here: the frame plane --
  ------------------------------------------------

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with null record;

  procedure Draw (on: in out Frame_plane'Class; o: HTML_object);

  --------------------------------------------------------
  -- Abstract methods that are implementation dependent --
  --------------------------------------------------------

  procedure Clear_area (on: in out Frame_plane) is abstract;
  
  ----------
  -- Text --
  ----------

  procedure Select_font(
    on         : in out Frame_plane; 
    descriptor : in     Font_descriptor;
    color      : in     Color_Code
  )
  is abstract;

  procedure Text_XY (
    on   : in out Frame_plane; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  ) 
  is abstract;

  procedure Text_size (
    on   : in out Frame_plane; 
    text : in     UTF_16_String; 
    x,y  :    out Integer
  )
  is abstract;
  
end Wasabee.Display;
