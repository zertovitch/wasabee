-- with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Drawing_Panels;           use GWindows.Drawing_Panels;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
-- with GWindows.Application;

package body Wasabee.GWin.Tabs is

  procedure Finish_creation (Window : in out HTML_area_type) is
  begin
    Window.HTML_contents.refresh:= full;
    -- Preparing the panel that is moved through scrolling (= Window.Panel)
    Panel_Size (Window, 1234, 765); -- !! tentative; will depend on HTML page
    -- Draw control covering all of Window.Panel
    Create_As_Control (Window.Wasa_Panel.Draw_Control, Window.Panel, "", 0,0,0,0);
    Dock (Window.Wasa_Panel.Draw_Control, Fill);
    Dock_Children (Window.Panel);
    -- Associate canvas to draw control
    Get_Canvas (Window.Wasa_Panel.Draw_Control, Window.Wasa_Panel.Drawing_Canvas);
  end Finish_creation;

end Wasabee.GWin.Tabs;
