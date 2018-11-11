with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.GWin.Windows;              use Wasabee.GWin.Windows;
-- with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

with GWindows.Base;                     use GWindows.Base;
-- with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
-- with GWindows.Application;

-- with Ada.Text_IO;                       use Ada.Text_IO;

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
      when Home_Key =>
        Window.On_Vertical_Scroll(First, null);
        Window.On_Vertical_Scroll(Previous_Unit, null);
      when End_Key =>
        Window.On_Vertical_Scroll(Last, null);
        Window.On_Vertical_Scroll(Next_Unit, null);
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
    -- put_line(
    --   Window.Client_Area_Height'img &
    --   " Panel:" & Window.Panel.Client_Area_Height'img &
    --   " top: " & Window.Panel.Top'img
    -- );
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

  procedure Set_minimal_sliding_panel_size (Window : in out HT_area_type) is
  begin
    -- Prepare the panel that is moved through scrolling (= Window.Panel)
    Panel_Size (Window, 1234, 2345); -- !! use maximized window size
  end Set_minimal_sliding_panel_size;

  procedure Finish_creation (Window : in out HT_area_type) is
  begin
    Window.Set_minimal_sliding_panel_size;
    -- Draw control covering all of Window.Panel
    Create_As_Control (Window.Wasa_Panel.Draw_Control, Window.Panel, "", 0,0,0,0);
    Dock (Window.Wasa_Panel.Draw_Control, Fill);
    Dock_Children (Window.Panel);
    -- Associate canvas to draw control
    Get_Canvas (Window.Wasa_Panel.Draw_Control, Window.Wasa_Panel.Drawing_Canvas);
  end Finish_creation;

  procedure Draw_with_resize (Window : in out HT_area_type) is
  begin
    Window.Wasa_Panel.Draw(Window.HT_contents, invisible);
    Window.HT_contents.Fit_bounding_boxes;
    Window.Wasa_Panel.Extend_area_height(Window.HT_contents.Bounding_box.p2.y);
    Window.Wasa_Panel.Select_main_background(Window.HT_contents);
    Window.Wasa_Panel.Clear_area;
    Window.Wasa_Panel.Draw(Window.HT_contents, full);
    -- !! ^ will be a bit more complicated with images...
    Window.Parent.Redraw(Redraw_Now => True);
  end Draw_with_resize;

  procedure Scroll_to_point (Window : in out HT_area_type; p: Point) is
    margin: constant:= 30;
  begin
    if p.y >= Window.Panel.Client_Area_Height then
      Window.On_Vertical_Scroll(Last, null);
      Window.On_Vertical_Scroll(Next_Unit, null);   -- Move to bottom of page
      return;
    end if;
    Window.On_Vertical_Scroll(First, null);         -- Move to top of page to begin with
    Window.On_Vertical_Scroll(Previous_Unit, null);
    Window.Parent.Freeze;
    while Window.Client_Area_Height - Window.Panel.Top + margin < p.y loop
      Window.On_Vertical_Scroll(Next_Unit, null);   -- Scroll down
    end loop;
    Window.Parent.Thaw;
    Window.Parent.Redraw;
  end Scroll_to_point;

end Wasabee.GWin.Tabs;
