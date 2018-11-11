with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Text_IO ; use Ada.Text_IO ;
with Wasabee.Request ; use Wasabee.Request ;
with Wasabee.Hypertext ; use Wasabee.Hypertext ;
with Wasabee.Hypertext.Parsing ; use Wasabee.Hypertext.Parsing ;
with Wasabee.SDL ; use Wasabee.SDL ;
with Wasabee.Util;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Wasabee_SDL is

begin
   Put_Line("Wasabee version " & Wasabee.Util.Version);
   --
   -- Ouvrir une fenetre
   --
   if Argument_Count = 0 then
      Put_Line(Standard_Error, "Provide an URL as command-line argument");
   else
    Object.Set_own_URL(Argument(1));
    Wasabee.Request.Retrieve_from_URL (Argument(1), HTML);
    Load_frame(Object, To_String(HTML));
    Object.Post_loading_processing;
    Init ;
    Quit ;
  end if;

end Wasabee_SDL;


