with Wasabee_GWin.Main;                 use Wasabee_GWin.Main;
with Wasabee_GWin.Tabs;                 use Wasabee_GWin.Tabs;

package body Wasabee_GWin.Windows is

  overriding
  procedure On_Create (Window : in out Browser_window_type) is
  begin
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
    newcomer.Create(Title => "Another Tab");
    newcomer.Show;
    Window.tabs.Append(newcomer);
    main_window.Update_control_frame;
  end New_Tab;

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
