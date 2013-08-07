with Wasabee.Hypertext;                 use Wasabee.Hypertext;

with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Wasabee.Display is

  subtype Color_Code is Natural; -- 24-bit RGB
  Black: constant Color_Code:= 0;
  White: constant Color_Code:= 16#FF_FF_FF#;

  subtype Font_face_name is Unbounded_String;

  type Font_descriptor is record
    face       : Font_face_name;
    size       : Positive;
    bold       : Boolean;
    italic     : Boolean;
    underlined : Boolean;
  end record;

  ----------------------------------------------------------
  -- **** The main object type here: the frame plane **** --
  ----------------------------------------------------------

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with private;

  ------------------------------
  -- ** Class-wide methods ** --
  ------------------------------

  procedure Draw (on: in out Frame_plane'Class; o: HTML_object);

  procedure Select_font(
    on         : in out Frame_plane'Class; 
    descriptor : in     Font_descriptor
  );

  --------------------------------------------------------------
  -- ** Abstract methods that are implementation-dependent ** --
  --------------------------------------------------------------

  procedure Clear_area (on: in out Frame_plane) is abstract;
  
  ----------
  -- Text --
  ----------

  -- Implemetation manages a Vector of fonts, indexed by 1,2,3,...

  procedure Create_target_font(
    on         : in out Frame_plane; 
    descriptor : in     Font_descriptor;
    index      : in     Positive
  )
  is abstract;
  
  procedure Select_target_font(
    on         : in out Frame_plane; 
    index      : in     Positive
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

private

  package Font_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Font_descriptor
  );


  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with record
    current_font   : Font_descriptor;
    bold_level     : Natural; -- 0: not bold, 1: one <b>, 2: two <b> or <h1><b>, etc.
    italic_level   : Natural;
    undeline_level : Natural;
    font_list      : Font_Vectors.Vector;
  end record;

end Wasabee.Display;
