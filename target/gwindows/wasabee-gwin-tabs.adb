with Wasabee.GWin.Windows;              use Wasabee.GWin.Windows;
-- with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;

-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
-- with GWindows.Application;

package body Wasabee.GWin.Tabs is

  procedure On_Character_Down(
    Window      : in out HT_area_type;
    Special_Key : in     Special_Key_Type;
    Value       : in     GCharacter
  )
  is
    multi: constant:= 5;
  begin
    case Special_Key is
      when Left_Key     =>
        for i in 1..multi loop
          Window.On_Horizontal_Scroll(Previous_Unit, null);
        end loop;
      when Up_Key       =>
        for i in 1..multi loop
          Window.On_Vertical_Scroll(Previous_Unit, null);
        end loop;
      when Right_Key    =>
        for i in 1..multi loop
          Window.On_Horizontal_Scroll(Next_Unit, null);
        end loop;
      when Down_Key     =>
        for i in 1..multi loop
          Window.On_Vertical_Scroll(Next_Unit, null);
        end loop;
      when Page_Up      =>
        Window.On_Vertical_Scroll(Previous_Page, null);
      when Page_Down    =>
        Window.On_Vertical_Scroll(Next_Page, null);
      when None         =>
        if Value = ' ' then
          Window.On_Vertical_Scroll(Next_Page, null);
        end if;
      when others       =>
        null;
    end case;
  end On_Character_Down;

  procedure On_Mouse_Wheel (Window  : in out HT_area_type;
                            X       : in     Integer;
                            Y       : in     Integer;
                            Keys    : in     Mouse_Key_States;
                            Z_Delta : in     Integer)
  is
  pragma Unreferenced (X, Y);
  begin
    if Z_Delta = 0 then
      null;
    elsif Z_Delta > 0 then
      if Keys(Control) then
        null; -- !! increase font size
      else
        Window.On_Vertical_Scroll(Previous_Page, null);
      end if;
    else
      if Keys(Control) then
        null; -- !! decrease font size
      else
        Window.On_Vertical_Scroll(Next_Page, null);
      end if;
    end if;
  end On_Mouse_Wheel;

  procedure On_Focus (Window : in out HT_area_type) is
  begin
    Accelerator_Table (Window, "Browser_Window_Shortcuts");
  end On_Focus;

  procedure On_Lost_Focus (Window : in out HT_area_type) is
  begin
    Accelerator_Table (Window, "nix");
  end On_Lost_Focus;

  procedure On_Menu_Select (
        Window : in out HT_area_type;
        Item   : in     Integer
  )
  is
  begin
    -- "Menu" commands (New Window, New Tab, etc.)
    -- are for the parent browser window.
    Browser_window_type(Window.Parent.all).On_Menu_Select(Item);
  end On_Menu_Select;

  procedure Finish_creation (Window : in out HT_area_type) is
  begin
    -- Preparing the panel that is moved through scrolling (= Window.Panel)
    Panel_Size (Window, 1234, 765); -- !! use maximized window size
    -- Draw control covering all of Window.Panel
    Create_As_Control (Window.Wasa_Panel.Draw_Control, Window.Panel, "", 0,0,0,0);
    Dock (Window.Wasa_Panel.Draw_Control, Fill);
    Dock_Children (Window.Panel);
    -- Associate canvas to draw control
    Get_Canvas (Window.Wasa_Panel.Draw_Control, Window.Wasa_Panel.Drawing_Canvas);
  end Finish_creation;

end Wasabee.GWin.Tabs;
