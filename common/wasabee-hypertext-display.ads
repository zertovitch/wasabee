with Ada.Containers.Vectors;
with Ada.Finalization;

package Wasabee.Hypertext.Display is

  ----------------------------------------------------------
  -- **** The main object type here: the frame plane **** --
  ----------------------------------------------------------

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with private;

  ------------------------------
  -- ** Class-wide methods ** --
  ------------------------------

  type Draw_mode is (
    invisible,      -- purpose: find the main bounding box
    all_but_images, 
    images_only,
    full
  );

  procedure Draw (
    on   : in out Frame_plane'Class; 
    o    : in out HT_object;
    mode :        Draw_mode
  );

  procedure Select_main_background (on: in out Frame_plane'Class; o: in out HT_object);

  --------------------------------------------------------------
  -- ** Abstract methods that are target-dependent.        ** --
  -- ** They are like generic parameters.                  ** --
  -- ** NB: actual size of screen coordinates are          ** --
  -- ** implementation-defined: pixels if possible,        ** --
  -- ** characters on a console, ... (0,0) is top left.    ** --
  --------------------------------------------------------------

  -- Erase the entire frame with current background style.
  -- Call Select_main_background before Clear_area to have the correct background.
  procedure Clear_area (on: in out Frame_plane) is abstract;
  
  procedure Area_size (on: Frame_plane; w,h: out Natural) is abstract;
  -- The hypertext page extends vertically. Some implementations
  -- extend the area automatically, some not or not enough.
  -- The following method ask for enough space for a proper display.
  procedure Extend_area_height (on: in out Frame_plane; to: Natural) is abstract;
  
  ------------------
  -- Text display --
  ------------------

  -- Implementation manages a Vector of fonts, indexed by 1,2,3,...
  -- The indices should match the indices passed in parameter
  -- in Create_target_font and Select_target_font.
  -- By each call of Create_target_font, new_index is increased by one.

  procedure Create_target_font(
    on         : in out Frame_plane; 
    descriptor : in     Font_descriptor;
    new_index  : in     Positive -- the new index should match the new index on target
  )
  is abstract;
  
  procedure Select_target_font(
    on         : in out Frame_plane; 
    index      : in     Positive 
  )
  is abstract;

  procedure Destroy_target_fonts(on: in out Frame_plane) is abstract; 

  -- Display text in the current font and color
  procedure Text_at (
    on   : in out Frame_plane; 
    p    : in     Point; 
    text : in     UTF_16_String
  ) 
  is abstract;

  procedure Text_size (
    on   : in out Frame_plane; 
    text : in     UTF_16_String; 
    w,h  :    out Natural
  )
  is abstract;

  -- Color for text and lines
  procedure Select_target_fore_color(on: in out Frame_plane; code: in Color_Code) is abstract;

  -- Color for background
  procedure Select_target_back_color(on: in out Frame_plane; code: in Color_Code) is abstract;

  -- Display a box' frame in the current fore color (and line style, depending on the target system)
  procedure Rectangle (on: in out Frame_plane; coords: Box) is abstract;

  -- Display a box in the current background color
  procedure Full_Rectangle (on: in out Frame_plane; coords: Box) is abstract;

  procedure Put_RGB_Bitmap (
    on     : in out Frame_plane; 
    bitmap :        Bitmap_type;
    coords :        Box
  )
  is abstract;

private

  procedure Select_font(
    on         : in out Frame_plane'Class; 
    descriptor : in     Font_descriptor
  );

  function Get_current_font(on : in Frame_plane'Class) return Font_descriptor;
  
  procedure Select_fore_color(on: in out Frame_plane'Class; code: in Color_Code);

  procedure Select_back_color(on: in out Frame_plane'Class; code: in Color_Code);
  
  procedure Advance_vertically(on: in out Frame_plane'Class; units: Natural);
  pragma Inline(Advance_vertically);

  package Font_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Font_descriptor
  );

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with record
    current_style       : Local_Style;
    font_list           : Font_Vectors.Vector; -- all defined distinct Font_descriptors
    curs: Point;
    new_line_before_writing : Boolean;
    skip_leading_blank      : Boolean;
    show_next_line_break    : Boolean; -- some line breaks are not cumulative
    indentation: Natural;
    indentation_space_width: Positive;
    numbering: Positive;
    marker_width: Natural; -- width of variable-size markers (numbers in an <OL>)
    area_width, area_height: Natural;
    latest_text_height: Natural;
    preformatted_text: Boolean;
  end record;

end Wasabee.Hypertext.Display;
