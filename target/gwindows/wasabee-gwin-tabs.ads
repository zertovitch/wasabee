with Wasabee.Util;                      use Wasabee.Util;

with Wasabee.GWin.Display;              use Wasabee.GWin.Display;

with GWindows.Cursors;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Types;

package Wasabee.GWin.Tabs is

  type HTML_area_type is new Drawing_Panel_Type with record
    HTML_contents: Wasabee.Util.HTML_object;
    --
    --  Windows graphics, including double-buffering
    --
    Drawing_Area : Canvas_Type;
    Saved_Area   : Wasa_GWin_Canvas; -- contains a Memory_Canvas_Type;
    Saved_Bitmap : Bitmap_Type;
    --
    Cursor       : GWindows.Cursors.Cursor_Type;
  end record;

  overriding
  procedure On_Create (Window : in out HTML_area_type);

  overriding
  procedure On_Paint (Window : in out HTML_area_type;
                      Canvas : in out GWindows.Drawing.Canvas_Type;
                      Area   : in     GWindows.Types.Rectangle_Type);

end Wasabee.GWin.Tabs;
