with Wasabee_GWin.Windows;              use Wasabee_GWin.Windows;

package body Wasabee_GWin.Main is

  use Wasabee_Resource_GUI;

  overriding
  procedure On_Create (Window : in out Main_Wasa_Window_Type) is
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

  procedure New_Wasa_Window(Window : in out Main_Wasa_Window_Type) is
    newcomer: Window_access;
  begin
    newcomer:= new Wasa_Window_type;
    newcomer.Create(Title => "Wasa is there");
    newcomer.Show;
    newcomer.main:= Window'Unchecked_Access;
    Window.windows.Append(newcomer);
    Window.Update_control_frame;
  end New_Wasa_Window;

  procedure Update_control_frame(Window : in out Main_Wasa_Window_Type) is
    w, t: Natural;
    w_curs: Windows_Vectors.Cursor;
    t_curs: Tabs_Vectors.Cursor;
    use Windows_Vectors, Tabs_Vectors;
  begin
    if not super_user then
      return;
    end if;
    w:= 0;
    t:= 0;
    w_curs:= Window.windows.First;
    while w_curs /= Windows_Vectors.No_Element loop
      w:= w + 1;
      declare
        ww: Wasa_Window_Type renames Element(w_curs).all;
      begin
        t_curs:= ww.tabs.First;
        while t_curs /= Tabs_Vectors.No_Element loop
          t:= t + 1;
          t_curs:= Next(t_curs);
        end loop;
      end;
      w_curs:= Next(w_curs);
    end loop;
    Window.control_frame.Open_windows_info.Text(Integer'Wide_Image(w));
    Window.control_frame.Open_tabs_info.Text(Integer'Wide_Image(t));
  end Update_control_frame;

end Wasabee_GWin.Main;
