------------------------------------------------------------
-- This is the GWindows implementation of Wasabee.Display --
------------------------------------------------------------

with Wasabee;                           use Wasabee;
with Wasabee.Display;                   use Wasabee.Display;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;

-- with GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;

with Ada.Containers.Vectors;

package Wasabee.GWin.Display is

  type p_Font_Type is access Font_Type;

  package GW_Font_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => p_Font_Type
  );

  -- Here we mix the abstract Wasa graphics and the GWindows graphics (phew!)

  type Wasa_GWin_Panel is new Frame_plane with record
    -- GWindows stuff:
    Draw_Control   : Drawing_Panel_Type;
    Drawing_Canvas : Drawing_Canvas_Type;
    gw_font_list   : GW_Font_Vectors.Vector; -- list with same indices as Frame_plane.font_list
  end record;

  overriding
  procedure Clear_area (on: in out Wasa_GWin_Panel);

  overriding
  procedure Area_size (on: Wasa_GWin_Panel; w,h: out Natural);

  overriding
  procedure Extend_area_height (on: in out Wasa_GWin_Panel; to: Natural);

  overriding
  procedure Create_target_font(
    on         : in out Wasa_GWin_Panel;
    descriptor : in     Font_descriptor;
    new_index  : in     Positive
  );

  overriding
  procedure Select_target_font(
    on         : in out Wasa_GWin_Panel;
    index      : in     Positive
  );

  overriding
  procedure Destroy_target_fonts(on: in out Wasa_GWin_Panel);

  overriding
  procedure Text_XY (
    on   : in out Wasa_GWin_Panel;
    x,y  : in     Integer;
    text : in     UTF_16_String
  );

  overriding
  procedure Text_size (
    on   : in out Wasa_GWin_Panel;
    text : in     UTF_16_String;
    x,y  :    out Natural
  );

  overriding
  procedure Select_target_text_color(on: in out Wasa_GWin_Panel; code: in Color_Code);

end Wasabee.GWin.Display;
