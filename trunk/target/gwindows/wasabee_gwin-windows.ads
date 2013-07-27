with Wasabee_GWin.Tabs;

with Ada.Containers.Vectors;

with GWindows;                          use GWindows;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.Main;             use GWindows.Windows.Main;
with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

package Wasabee_GWin.Windows is

  type Tab_access is access Wasabee_GWin.Tabs.Tab_type;

  package Tabs_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Tab_access
  );

  type Browser_window_type is
    new GWindows.Windows.Window_Type with record
      tabs: Tabs_Vectors.Vector;
      main: Pointer_To_Main_Window_Class;
      hidden_menu: Browser_Menu_Type;
    end record;

  overriding
  procedure On_Create (Window : in out Browser_window_type);

  procedure New_Tab(Window : in out Browser_window_type);

  -- NB: the menu is invisible; only commands and shortcuts used

  overriding
  procedure On_Menu_Select (
        Window : in out Browser_window_type;
        Item   : in     Integer        );

  overriding
  procedure On_Close (Window    : in out Browser_window_type;
                      Can_Close :    out Boolean);

end Wasabee_GWin.Windows;
