------------------------------------------------------------
-- This is the GWindows implementation of Wasabee.Display --
------------------------------------------------------------

with Wasabee;                           use Wasabee;
with Wasabee.Colors;                    use Wasabee.Colors;
with Wasabee.Encoding;                  use Wasabee.Encoding;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Hypertext.Locations;       use Wasabee.Hypertext.Locations;
with Wasabee.Images;

with GWindows.Colors;
with GWindows.Cursors;                  use GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Containers.Vectors;

package Wasabee.GWin.Display is

  type p_Font_Type is access Font_Type;

  package GW_Font_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => p_Font_Type
  );

  type Wasa_drawing_panel is new Drawing_Panel_Type with private;

  overriding
  procedure On_Mouse_Move (Window : in out Wasa_drawing_panel;
                           X      : in     Integer;
                           Y      : in     Integer;
                           Keys   : in     Mouse_Key_States);

  procedure On_Click
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States); -- Keys contains only the mouse button that clicked

  overriding
  procedure On_Left_Mouse_Button_Down
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States);

  overriding
  procedure On_Left_Mouse_Button_Up
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States);

  overriding
  procedure On_Middle_Mouse_Button_Up
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States);

  -- Here we mix the abstract Wasa graphics and the GWindows graphics (phew!)

  type Wasa_GWin_Panel is new Frame_plane with record
    -- GWindows stuff:
    Draw_Control     : Wasa_drawing_panel;
    Drawing_Canvas   : Drawing_Canvas_Type;
    gw_font_list     : GW_Font_Vectors.Vector; -- list with same indices as Frame_plane.font_list
    background_color : GWindows.Colors.Color_Type;
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
  procedure Text_at (
    on   : in out Wasa_GWin_Panel;
    p    : in     Wasabee.Hypertext.Point;
    text : in     UTF_16_String
  );

  overriding
  procedure Text_size (
    on   : in out Wasa_GWin_Panel;
    text : in     UTF_16_String;
    w, h :    out Natural
  );

  overriding
  procedure Select_target_fore_color(on: in out Wasa_GWin_Panel; code: in Color_Code);

  overriding
  procedure Select_target_back_color(on: in out Wasa_GWin_Panel; code: in Color_Code);

  overriding
  procedure Rectangle (on: in out Wasa_GWin_Panel; coords: Box);

  overriding
  procedure Full_Rectangle (on: in out Wasa_GWin_Panel; coords: Box);

  overriding
  procedure Put_RGB_Bitmap (
    on     : in out Wasa_GWin_Panel;
    bitmap :        Wasabee.Images.Bitmap_type;
    coords :        Box
  );

private

  type Wasa_to_gwin_mouse_cursor_type is
    array(Mouse_cursor_style) of Cursor_Type;

  type Wasa_drawing_panel is new Drawing_Panel_Type with record
    wasa_to_gwin_mouse_cursor : Wasa_to_gwin_mouse_cursor_type;
    gwin_cursors_initialized  : Boolean:= False;
    left_button_down_in_panel : Boolean:= False;
  end record;

end Wasabee.GWin.Display;
