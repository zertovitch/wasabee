with Wasabee_GWin.Windows;              use Wasabee_GWin.Windows;

package body Wasabee_GWin.Main is

  use Wasabee_Resource_GUI;

  overriding
  procedure On_Create (Window : in out Main_Wasa_Window_Type) is
  begin
    --
    -- Control frame
    --
    Window.control_frame.Create_Child(
      Parent => Window,
      Title  => "Wasa control frame"
    );
    Create_contents(
      Window     => Window.control_frame,
      for_dialog => False,
      resize     => True
    );
    if super_user then
      Window.control_frame.Show;
    end if;
    --
    -- First window
    --
    Window.New_Browser_Window;
  end On_Create;

  procedure New_Browser_Window(Window : in out Main_Wasa_Window_Type) is
    newcomer: Window_access:= new Browser_window_type;
  begin
    newcomer:= new Browser_window_type;
    newcomer.main:= Window'Unchecked_Access;
    Window.windows.Append(newcomer);
    Window.Update_control_frame;
    newcomer.Create(
      Title =>
        "Wasa browser, window opened as #" &
        Integer'Wide_Image(Window.windows.Find_Index(newcomer))
    );
    newcomer.Show;
  end New_Browser_Window;

  procedure Update_control_frame(Window : in out Main_Wasa_Window_Type) is
    w, t: Natural;
    w_curs: Windows_Vectors.Cursor;
    use Windows_Vectors, Tabs_Vectors, Wasabee_common.Caches;
  begin
    if not super_user then
      return;
    end if;
    w:= 0;
    t:= 0;
    w_curs:= Window.windows.First;
    while w_curs /= Windows_Vectors.No_Element loop
      w:= w + 1;
      t:= t + Natural(Length(Element(w_curs).tabs));
      w_curs:= Next(w_curs);
    end loop;
    Window.control_frame.Open_windows_info.Text(Integer'Wide_Image(w));
    Window.control_frame.Open_tabs_info.Text(Integer'Wide_Image(t));
    Window.control_frame.Cached_objects_info.Text(Integer'Wide_Image(Object_count(Window.cache)));
  end Update_control_frame;

end Wasabee_GWin.Main;
