with Wasabee.Hypertext;                 use Wasabee.Hypertext;

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

  procedure Draw (on: in out Frame_plane'Class; o: HT_object);

  --------------------------------------------------------------
  -- ** Abstract methods that are target-dependent.        ** --
  -- ** They are like generic parameters.                  ** --
  -- ** NB: actual size of screen coordinates are          ** --
  -- ** implementation-defined: pixels if possible,        ** --
  -- ** characters on a console, ... (0,0) is top left.    ** --
  --------------------------------------------------------------

  -- Erase frame with current background style
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

  procedure Text_XY (
    on   : in out Frame_plane; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  ) 
  is abstract;

  procedure Text_size (
    on   : in out Frame_plane; 
    text : in     UTF_16_String; 
    w,h  :    out Natural
  )
  is abstract;

  procedure Select_target_text_color(on: in out Frame_plane; code: in Color_Code) is abstract;
  
private

  procedure Select_font(
    on         : in out Frame_plane'Class; 
    descriptor : in     Font_descriptor
  );

  function Get_current_font(on : in Frame_plane'Class) return Font_descriptor;
  
  procedure Select_text_color(on: in out Frame_plane'Class; code: in Color_Code);

  package Font_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Font_descriptor
  );

  type Font_modifier_level is array(Font_modifier) of Natural;

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with record
    current_style       : Style;
    font_list           : Font_Vectors.Vector; -- all defined distinct Font_descriptors
    -- Font modifiers (actually will activate a new font in some systems)
    modifier_level      : Font_modifier_level; 
    -- ^ 0: not bold, 1: one <b>, 2: two <b> or <h1><b>, etc.
  end record;

end Wasabee.Hypertext.Display;
