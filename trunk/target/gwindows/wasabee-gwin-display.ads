with Wasabee;                           use Wasabee;
with Wasabee.Display;                   use Wasabee.Display;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;

-- with GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
-- with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;

package Wasabee.GWin.Display is

  -- Here we mix the abstract Wasa graphics and the GWindows graphics
  -- (phew!)

  type Wasa_GWin_Panel is new Frame_plane with record
    -- Wasabee stuff
    -- GWindows stuff
    -- !! font vector !!
    Draw_Control   : Drawing_Panel_Type;
    Drawing_Canvas : Drawing_Canvas_Type;
  end record;

  overriding
  procedure Clear_area (on: in out Wasa_GWin_Panel);

  overriding
  procedure Create_target_font(
    on         : in out Wasa_GWin_Panel;
    descriptor : in     Font_descriptor;
    index      : in     Positive
  );

  overriding
  procedure Select_target_font(
    on         : in out Wasa_GWin_Panel;
    index      : in     Positive
  );

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
    x,y  :    out Integer
  );

end Wasabee.GWin.Display;
