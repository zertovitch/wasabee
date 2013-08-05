--
-- Premier but du jeu, mettre tout dans une variable unbounded_String pour le style
--

with Ada.Text_IO ; use Ada.Text_IO ;
with Ada.Command_Line ; use Ada.Command_Line ;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Wasabee.Css ; use Wasabee.Css ;

procedure Wasabee_Css is
begin
   Put_Line ("Wasabee CSS test");
   if Argument_Count = 1 then
      Read_Css_File (Argument(1)) ;
      Put_Line("******************* Element to parse *****************");
      Put_Line(To_String(Css));
      Put_Line("******************  Cleaning         *****************");
      Parse_Information ;
   else
      Put_Line("Usage: wasabee_css <filename>");
      null;
   end if ;
end;
