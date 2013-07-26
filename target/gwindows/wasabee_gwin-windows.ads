with Wasabee_GWin.Tabs;

with Ada.Containers.Vectors;

with GWindows;                          use GWindows;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.Main;             use GWindows.Windows.Main;

package Wasabee_GWin.Windows is

  type Tab_access is access Wasabee_GWin.Tabs.Tab_type;

  package Tabs_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Tab_access
  );

  type Wasa_Window_Type is
    new GWindows.Windows.Window_Type with record
      tabs: Tabs_Vectors.Vector;
      main: Pointer_To_Main_Window_Class;
    end record;

  overriding
  procedure On_Create (Window : in out Wasa_Window_Type);

  overriding
  procedure On_Close (Window    : in out Wasa_Window_Type;
                      Can_Close :    out Boolean);

end Wasabee_GWin.Windows;
