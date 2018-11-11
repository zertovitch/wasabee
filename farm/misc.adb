--
-- Premier but du jeu, mettre tout dans une variable unbounded_String pour le style
--

with Ada.Text_IO ; use Ada.Text_IO ;
-- with Ada.Command_Line ; use Ada.Command_Line ;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Wasabee.Css ; use Wasabee.Css ;

procedure Misc is
  procedure Test(s:String) is
  begin 
    Put_Line('[' & s & "]  becomes  [" & Simplify_Blanks(s) & ']');
  end;
begin
  Test("  a   b     c  ");
  Test(" a   b     c ");
  Test("a b c ");
  Test(" a b c");
  Test("a b c");
end;
