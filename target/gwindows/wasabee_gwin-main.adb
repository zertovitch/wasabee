with Wasabee_GWin.Windows;              use Wasabee_GWin.Windows;

package body Wasabee_GWin.Main is

  use Wasabee_Resource_GUI;

  overriding
  procedure On_Create (Window : in out Main_Window_Type) is
  begin
    --
    -- Control frame
    --
    Window.control_frame.Create_Child(Parent => Window);
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
    Window.New_Wasa_Window;
  end On_Create;

  procedure New_Wasa_Window(Window : in out Main_Window_Type) is
    newcomer: Window_access;
  begin
    newcomer:= new Wasa_Window_type;
    newcomer.Create(Title => "Wasa is there");
    newcomer.Show;
    -- newcomer.main:= Window'Unchecked_Access;
    Window.windows.Append(newcomer);
    Window.Update_control_frame;
  end New_Wasa_Window;

  procedure Update_control_frame(Window : in out Main_Window_Type) is
    w, t: Natural;
  begin
    if not super_user then
      return;
    end if;
    w:= 0;
    t:= 0;
    -- !! iterate windows and tabs
    Window.control_frame.Open_windows_info.Text(Integer'Wide_Image(w));
    Window.control_frame.Open_tabs_info.Text(Integer'Wide_Image(t));
  end Update_control_frame;

end Wasabee_GWin.Main;
