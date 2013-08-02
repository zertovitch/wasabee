with Wasabee.Caches;
with Wasabee.GWin.Windows;
with Wasabee_Resource_GUI;

with Ada.Containers.Vectors;

with GWindows.Windows.Main;
with GWindows.Windows;                  use GWindows.Windows;

package Wasabee.GWin.Main is

  type Window_access is access all Wasabee.GWin.Windows.Browser_window_type;

  package Windows_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Window_access
  );

  type Main_Wasa_Window_Type is
    new GWindows.Windows.Main.Main_Window_Type with record
      windows       : Windows_Vectors.Vector;
      control_frame : Wasabee_Resource_GUI.Main_control_window_Type;
      cache         : Wasabee.Caches.Cache_type;
    end record;

  overriding
  procedure On_Create (Window : in out Main_Wasa_Window_Type);

  procedure New_Browser_Window(Window : in out Main_Wasa_Window_Type);

  procedure Update_control_frame(Window : in out Main_Wasa_Window_Type);

end Wasabee.GWin.Main;
