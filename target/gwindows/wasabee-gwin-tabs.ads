with Wasabee.Hypertext;                 use Wasabee.Hypertext;

with Wasabee.GWin.Display;              use Wasabee.GWin.Display;

with GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
-- with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
-- with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Scroll_Panels;            use GWindows.Scroll_Panels;
-- with GWindows.Types;

package Wasabee.GWin.Tabs is

  type HTML_area_type is new Scroll_Panel_Type with record
    HTML_contents: Wasabee.Hypertext.HTML_object;
    --
    --  Windows graphics
    --
    Wasa_Panel  : Wasa_GWin_Panel;
    --
    Cursor      : GWindows.Cursors.Cursor_Type;
  end record;

  -- After scroll_panel first resizing(with Dock)
  procedure Finish_creation (Window : in out HTML_area_type);

end Wasabee.GWin.Tabs;
