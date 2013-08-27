with Wasabee.Hypertext;
with Wasabee.GWin.Display;              use Wasabee.GWin.Display;

-- with GWindows.Drawing;                  use GWindows.Drawing;
-- with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
-- with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Scroll_Panels;            use GWindows.Scroll_Panels;
-- with GWindows.Types;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Wasabee.GWin.Tabs is

  type HT_area_type is new Scroll_Panel_Type with record
    URL          : Unbounded_String;
    HT_contents  : Wasabee.Hypertext.HT_object;
    --
    wasa_panel   : Wasa_GWin_Panel; -- panel sliding within the scroll panel
  end record;

  overriding
  procedure On_Character_Down(
    Window      : in out HT_area_type;
    Special_Key : in     Special_Key_Type;
    Value       : in     GCharacter
  );

  overriding
  procedure On_Mouse_Wheel (Window  : in out HT_area_type;
                            X       : in     Integer;
                            Y       : in     Integer;
                            Keys    : in     Mouse_Key_States;
                            Z_Delta : in     Integer);

  overriding
  procedure On_Focus (Window : in out HT_area_type);

  overriding
  procedure On_Lost_Focus (Window : in out HT_area_type);

  overriding
  procedure On_Menu_Select (
        Window : in out HT_area_type;
        Item   : in     Integer
  );

  procedure Set_minimal_sliding_panel_size (Window : in out HT_area_type);

  -- After scroll_panel first resizing(with Dock)
  procedure Finish_creation (Window : in out HT_area_type);

  procedure Draw_with_resize (Window : in out HT_area_type);

end Wasabee.GWin.Tabs;
