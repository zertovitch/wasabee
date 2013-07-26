with Wasabee_GWin.Windows;
with Wasabee_Resource_GUI;

with Ada.Containers.Vectors;

with GWindows;                          use GWindows;
with GWindows.Windows.Main;
with GWindows.Windows;                  use GWindows.Windows;

package Wasabee_GWin.Main is

  type Window_access is access Wasabee_GWin.Windows.Window_type;

  package Windows_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Window_access
  );

  type Main_Window_Type is
    new GWindows.Windows.Main.Main_Window_Type with record
      windows       : Windows_Vectors.Vector;
      control_frame : Wasabee_Resource_GUI.Main_control_window_Type;
    end record;

  overriding
  procedure On_Create (Window : in out Main_Window_Type);

end Wasabee_GWin.Main;
