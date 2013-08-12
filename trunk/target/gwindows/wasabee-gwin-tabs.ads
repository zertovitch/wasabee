with Wasabee.Hypertext;                 use Wasabee.Hypertext;

with Wasabee.GWin.Display;              use Wasabee.GWin.Display;

with GWindows.Cursors;
-- with GWindows.Drawing;                  use GWindows.Drawing;
-- with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
-- with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Scroll_Panels;            use GWindows.Scroll_Panels;
-- with GWindows.Types;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with GWindows.Windows;                  use GWindows.Windows;

package Wasabee.GWin.Tabs is

  type HTML_area_type is new Scroll_Panel_Type with record
    URL          : Unbounded_String;
    HTML_contents: Wasabee.Hypertext.HTML_object;
    --
    --  Windows graphics
    --
    Wasa_Panel   : Wasa_GWin_Panel;
    --
    Cursor       : GWindows.Cursors.Cursor_Type;
  end record;

  overriding
  procedure On_Character_Down(
    Window      : in out HTML_area_type;
    Special_Key : in     Special_Key_Type;
    Value       : in     GCharacter
  );

  overriding
  procedure On_Mouse_Wheel (Window  : in out HTML_area_type;
                            X       : in     Integer;
                            Y       : in     Integer;
                            Keys    : in     Mouse_Key_States;
                            Z_Delta : in     Integer);

  overriding
  procedure On_Focus (Window : in out HTML_area_type);

  overriding
  procedure On_Lost_Focus (Window : in out HTML_area_type);

  overriding
  procedure On_Menu_Select (
        Window : in out HTML_area_type;
        Item   : in     Integer
  );

  -- After scroll_panel first resizing(with Dock)
  procedure Finish_creation (Window : in out HTML_area_type);

end Wasabee.GWin.Tabs;
