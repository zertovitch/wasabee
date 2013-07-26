with Wasabee_common;                    use Wasabee_common;

with GWindows.Cursors;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;
with GWindows.Types;

package Wasabee_GWin.Tabs is

  type Tab_type is new GWindows.Drawing_Panels.Drawing_Panel_Type with record
    HTML_contents: Wasabee_common.HTML_object;
    --
    --  Windows graphics, inckuding double-buffering
    --
    Drawing_Area : Canvas_Type;
    Saved_Area   : Memory_Canvas_Type;
    Saved_Bitmap : Bitmap_Type;
    Cursor       : GWindows.Cursors.Cursor_Type;
  end record;

  overriding
  procedure On_Paint (Window : in out Tab_type;
                      Canvas : in out GWindows.Drawing.Canvas_Type;
                      Area   : in     GWindows.Types.Rectangle_Type);

end Wasabee_GWin.Tabs;
