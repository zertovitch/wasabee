with Wasabee_GWin.Tabs;

with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
with GWindows.Packing_Boxes;            use GWindows.Packing_Boxes;
with GWindows.Panels;                   use GWindows.Panels;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.Main;             use GWindows.Windows.Main;

with Ada.Containers.Vectors;

package Wasabee_GWin.Windows is

  type Tab_access is access Wasabee_GWin.Tabs.HTML_area_type;

  package Tabs_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Tab_access
  );

  ------------------------------
  -- Control_box_type methods --
  ------------------------------

  type Control_box_type is new Panel_Type with record
    tab_visuals        : Drawing_Panel_Type;
    navigation_and_url : Packing_Box_Type;
    navigation_buttons : Drawing_Panel_Type;
    url_box            : Edit_Box_Type;
  end record;

  overriding
  procedure On_Create (Window : in out Control_box_type);

  type Browser_window_type is new GWindows.Windows.Window_Type with record
    tabs               : Tabs_Vectors.Vector;
    control_box        : Control_Box_Type;
    main               : Pointer_To_Main_Window_Class;
    active_tab         : Positive;
    window_info_string : GString_Unbounded; -- only for monitoring
    -- hidden_menu : Browser_Menu_Type;
  end record;

  ---------------------------------
  -- Browser_window_type methods --
  ---------------------------------

  overriding
  procedure On_Create (Window : in out Browser_window_type);

  overriding
  procedure On_Focus (Window : in out Browser_window_type);

  overriding
  procedure On_Lost_Focus (Window : in out Browser_window_type);

  procedure New_Tab(Window : in out Browser_window_type);
  procedure Next_tab(Window : in out Browser_window_type);
  procedure Close_tab(Window : in out Browser_window_type);

  -- NB: the menu is not created; only commands and shortcuts are used

  overriding
  procedure On_Menu_Select (
        Window : in out Browser_window_type;
        Item   : in     Integer        );

  overriding
  procedure On_Close (Window    : in out Browser_window_type;
                      Can_Close :    out Boolean);

end Wasabee_GWin.Windows;
