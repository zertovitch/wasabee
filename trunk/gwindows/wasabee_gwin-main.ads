with GWindows;                          use GWindows;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;

package Wasabee_GWin.Main is

  type Main_Window_Type is
    new GWindows.Windows.MDI.MDI_Main_Window_Type with record
      tab_1: Tab_type; -- !! we will have a vector of tabs
    end record;

end Wasabee_GWin.Main;
