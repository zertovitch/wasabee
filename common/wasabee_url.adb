--
-- L'idee est de choppeer une URL et d'en faire trois trucs ...
-- Un protocole
-- Un host
-- Eventuellement un port
-- Une ressource a telecharger
--

with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;
with Ada.Text_IO ; use Ada.Text_IO ;

with Ada.Command_Line ; use Ada.Command_Line ;

with Wasabee.URL ; use Wasabee.URL ;

procedure Wasabee_Url is


begin
   if Argument_Count = 1 then
      if Argument(1) = "-test" then
         -- Je pense qu'avaec ca je fais la plupart des cas de figure ...
         General_Test ;
      else
         Test(Argument(1));
      end if ;
   else
      General_Test ;
   end if ;
   -- Put_Line("Argument: " & To_String(Url1));

end;
