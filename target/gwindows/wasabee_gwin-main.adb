package body Wasabee_GWin.Main is

  use Wasabee_Resource_GUI;

  overriding
  procedure On_Create (Window : in out Main_Window_Type) is
  begin
    Create_contents(
      Window     => Window.control_frame,
      for_dialog => False,
      resize     => False); -- !!
  end On_Create;

end Wasabee_GWin.Main;
