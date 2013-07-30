with Wasabee_GWin.Main;                 use Wasabee_GWin.Main;
with Wasabee_GWin.Tabs;                 use Wasabee_GWin.Tabs;

with GWindows.Base;                     use GWindows.Base;

package body Wasabee_GWin.Windows is

  overriding
  procedure On_Create (Window : in out Control_Box_Type) is
  begin
    Window.navigation_and_url.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 20,
      Width      => 1, -- will be extended by docking
      Height     => 1  -- will be extended by docking
    );
    Window.navigation_buttons.Create_As_Control(
      Parent     => Window.navigation_and_url,
      Left       => 0,
      Top        => 0,
      Width      => 50,
      Height     => 1  -- will be extended by docking
    );
    Window.navigation_buttons.Dock(At_Left);
    Window.url_box.Create(
      Parent     => Window.navigation_and_url,
      Left       => 50,
      Top        => 0,
      Width      => 1, -- will be extended by docking
      Height     => 1, -- will be extended by docking
      Text       => "Click here or press Ctrl-L for entering new Web address"
    );
    Window.url_box.Dock(Fill);
    Window.tab_visuals.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 0,
      Width      => 1,  -- will be extended by docking
      Height     => 20
    );
    Window.tab_visuals.Dock(At_Top);
    Window.navigation_and_url.Dock(Fill);
  end On_Create;

  overriding
  procedure On_Create (Window : in out Browser_window_type) is
  begin
    Use_Gui_Font(Window);
    Window.control_box.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 0,
      Width      => 1, -- will be extended by docking
      Height     => 40
    );
    Window.control_box.Dock(At_Top);
    --  Wasabee_Resource_GUI.Create_Full_Menu(Window.hidden_menu);
    --  Window.Menu(Window.hidden_menu.Main);
    Accelerator_Table (Window, "Browser_Window_Shortcuts");
    --
    -- First tab
    --
    Window.New_Tab;
  end On_Create;

  procedure New_Tab(Window : in out Browser_window_type) is
    newcomer: constant Tab_access:= new Tab_type;
    main_window: Main_Wasa_Window_Type
      renames Main_Wasa_Window_Type(Window.main.all);
  begin
    newcomer.Create_As_Control(
      Parent => Window,
      Left   => 0,
      Top    => 20,
      Width  => Window.Client_Area_Width,
      Height => Window.Client_Area_Height
    );
    newcomer.Dock(Fill);
    Window.tabs.Append(newcomer);
    main_window.Update_control_frame;
  end New_Tab;

  overriding
  procedure On_Menu_Select (
        Window : in out Browser_window_type;
        Item   : in     Integer        )
  is
    main_window: Main_Wasa_Window_Type
      renames Main_Wasa_Window_Type(Window.main.all);
  begin
    case Item is
      when ID_New_Browser_Window =>
        main_window.New_Browser_Window;
      when ID_New_Tab =>
        Window.New_Tab;
      when others =>
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  overriding
  procedure On_Close (Window    : in out Browser_window_type;
                      Can_Close :    out Boolean) is
    w_curs: Windows_Vectors.Cursor;
    main_window: Main_Wasa_Window_Type
      renames Main_Wasa_Window_Type(Window.main.all);
  begin
    Can_Close:= True or Window.main/=null; -- !! check "Leave page" "Close tabs" etc.
    if not Can_Close then
      return;
    end if;
    w_curs:= main_window.windows.Find(Window'Unchecked_Access);
    main_window.windows.Delete(w_curs);
    main_window.Update_control_frame;
    if main_window.windows.Is_Empty then
      Close(main_window); -- Close everything if no more browser window is open
    end if;
  end On_Close;

end Wasabee_GWin.Windows;
