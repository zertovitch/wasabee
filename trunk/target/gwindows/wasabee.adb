with GWindows.Application;        use GWindows.Application;
with GWindows.Base;               use GWindows.Base;
with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

with Wasabee_GWin.Main;           use Wasabee_GWin.Main;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

procedure Wasabee is

  Top: Wasabee_GWin.Main.Main_Window_Type;

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
    Create (Top, "Wasabee");
    Message_Loop;
end Wasabee;
