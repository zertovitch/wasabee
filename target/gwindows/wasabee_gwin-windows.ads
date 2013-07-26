with Wasabee_GWin.Tabs;

with Ada.Containers.Vectors;

with GWindows;                          use GWindows;
with GWindows.Windows;                  use GWindows.Windows;

package Wasabee_GWin.Windows is

  type Tab_access is access Wasabee_GWin.Tabs.Tab_type;

  package Tabs_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Tab_access
  );

  type Window_Type is
    new GWindows.Windows.Window_Type with record
      tabs: Tabs_Vectors.Vector;
    end record;

end Wasabee_GWin.Windows;
