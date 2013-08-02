with Wasabee;                           use Wasabee;
with Wasabee.Display;                   use Wasabee.Display;
with Wasabee.Util;                      use Wasabee.Util;

with GWindows.Cursors;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;

package Wasabee.GWin.Display is

  -- Here we mix the abstract Wasa graphics and the GWindows graphics
  -- (phew!)

  type Wasa_GWin_Canvas is new Frame_plane with record
    mem_windows_canvas: Memory_Canvas_Type; -- the Windows canvas
  end record;

  overriding
  procedure Text_XY (
    on   : in out Wasa_GWin_Canvas;
    x,y  : in     Integer;
    text : in     UTF_16_String
  );

  procedure Draw_object(
    Canvas : in out Wasa_GWin_Canvas;
    ho     : in out HTML_Object; -- in out: ho.refresh is changed
    width  : Integer;
    height : Integer);

end Wasabee.GWin.Display;
