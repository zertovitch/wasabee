with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Text_IO ; use Ada.Text_IO ;
with Wasabee.Request ; use Wasabee.Request ;
with Wasabee.Hypertext ; use Wasabee.Hypertext ;
with Wasabee.Sdl ; use Wasabee.Sdl ;
with DOM.Core; use DOM.Core ;

procedure Wasabee_Sdl is

begin
   Put_Line("Wasabee version 0.0.1");
   --
   -- Ouvrir une fenetre
   --
   if Argument_Count = 0 then
      Put_Line(Standard_Error, "Provide an URL as command-line argument");
   else
    Wasabee.Request.Open_Url (Argument(1), Xhtml);
    Load_frame(Object, Xhtml);
    Init ;
    Quit ;
  end if;

end ;


