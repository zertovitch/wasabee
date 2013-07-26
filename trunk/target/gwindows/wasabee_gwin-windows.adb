with Wasabee_GWin.Main;                 use Wasabee_GWin.Main;

package body Wasabee_GWin.Windows is

  overriding
  procedure On_Create (Window : in out Wasa_Window_Type) is
  begin
    null; -- create 1st tab
  end On_Create;

  overriding
  procedure On_Close (Window    : in out Wasa_Window_Type;
                      Can_Close :    out Boolean) is
    w_curs: Windows_Vectors.Cursor;
    mw: Main_Wasa_Window_Type renames Main_Wasa_Window_Type(Window.main.all);
  begin
    Can_Close:= True; -- !! check "Leave page" "Close tabs" etc.
    if not Can_Close then
      return;
    end if;
    w_curs:= mw.windows.Find(Window'Unchecked_Access);
    mw.windows.Delete(w_curs);
    mw.Update_control_frame;
    if mw.windows.Is_Empty then
      Close(mw); -- Close everything if no more browser window is open
    end if;
  end On_Close;

end Wasabee_GWin.Windows;
