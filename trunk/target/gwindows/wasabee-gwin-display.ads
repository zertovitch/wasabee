with Wasabee;                           use Wasabee;
with Wasabee.Display;                   use Wasabee.Display;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Util;                      use Wasabee.Util;

-- with GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
-- with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;

package Wasabee.GWin.Display is

  -- Here we mix the abstract Wasa graphics and the GWindows graphics
  -- (phew!)

  type Wasa_GWin_Panel is new Frame_plane with record
    Draw_Control   : Drawing_Panel_Type;
    Drawing_Canvas : Drawing_Canvas_Type;
  end record;

  overriding
  procedure Clear_area (on: in out Wasa_GWin_Panel);

  overriding
  procedure Text_XY (
    on   : in out Wasa_GWin_Panel;
    x,y  : in     Integer;
    text : in     UTF_16_String
  );

end Wasabee.GWin.Display;
