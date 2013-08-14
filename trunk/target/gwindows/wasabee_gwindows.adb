with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Wasabee.GWin.Main;                 use Wasabee.GWin.Main;

with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Conversions;
use Ada.Strings.UTF_Encoding.Conversions;
with Ada.Text_IO;                       use Ada.Text_IO;

with GNAT.Traceback.Symbolic;

procedure Wasabee_GWindows is

  Top: Wasabee.GWin.Main.Main_Wasa_Window_Type;

  procedure Interactive_crash(
    Window : in out GWindows.Base.Base_Window_Type'Class;
    E: Ada.Exceptions.Exception_Occurrence)
  is
    pragma Unreferenced (Window);
    small_insult: constant String:=
        Ada.Exceptions.Exception_Name (E) & ASCII.LF &
        Ada.Exceptions.Exception_Message (E);
    insult: constant String:=
        small_insult & ASCII.LF &
        GNAT.Traceback.Symbolic.Symbolic_Traceback(E);
  begin
    GWindows.Base.On_Exception_Handler (Handler => null); -- Avoid infinite recursion!
    Message_Box
      ("Crash in Wasabee (GWindows)",
        To_GString_from_String(insult),
        OK_Box
      );
  end Interactive_crash;

begin
    GWindows.Base.On_Exception_Handler (Handler => Interactive_crash'Unrestricted_Access);
    Create (Top, "Wasabee invisible main window");
    -- Open all arguments in separate tabs
    for i in 1..Argument_Count loop
      if i > 1 then
        Top.windows.Element(1).New_Tab;
      end if;
      Top.windows.Element(1).control_box.url_box.Text(
        Convert(Argument(i))
      );
      Top.windows.Element(1).Go_on_URL;
    end loop;
    Message_Loop;
exception
  when E: others =>
    New_Line;
    Put_Line("----[ Unhandled exception before Windows message loop ]----");
    Put_Line(" > Name of exception . . . . .: " &
             Ada.Exceptions.Exception_Name(E) );
    Put_Line(" > Message for exception . . .: " &
             Ada.Exceptions.Exception_Message(E) );
    Put_Line(" > Trace-back of call stack: " );
    Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );
end Wasabee_GWindows;
