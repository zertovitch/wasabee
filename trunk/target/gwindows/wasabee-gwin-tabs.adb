with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

with GWindows.Application;

package body Wasabee.GWin.Tabs is

  procedure Display_saved_area(
    window : in out HTML_area_type;
    area   :        GWindows.Types.Rectangle_Type)
  is
  begin
    BitBlt( window.Drawing_Area,
            area.Left,
            area.Top,
            area.Right-area.Left,
            area.Bottom-area.top,
            Window.Saved_Area.mem_windows_canvas,
            area.Left,
            area.Top
          );
    --### For monitoring (interesting!): ###
    -- Line(window.Drawing_Area,Area.Left,Area.Top,Area.Right,Area.Bottom);
  end Display_saved_area;

  procedure Update_bitmap(Window : in out HTML_area_type ) is
  begin
    if Window.HTML_contents.refresh /= nothing then
      Wasabee.GWin.Display.Draw_object(
        Window.Saved_Area,
        Window.HTML_contents,
        Width(Window),
        Height(Window)
      );
    end if;
  end Update_bitmap;

  procedure Subtle_redraw (Window : in out HTML_area_type ) is
    initial_refresh: constant Refresh_mode:= Window.HTML_contents.refresh;
  begin
    Update_bitmap(Window);
    if initial_refresh /= nothing then
      Redraw(Window); -- Repaint visible area(s) on screen
                      -- only if bitmap has changed
    end if;
  end Subtle_redraw;

  procedure On_Create (Window : in out HTML_area_type) is
  begin
    Window.HTML_contents.refresh:= full; -- blank page so far
    -- Preparing the canvases
    Window.Drawing_Area.Background_Mode (Transparent);
    Window.Saved_Area.mem_windows_canvas.Background_Mode (Transparent);
    Window.Get_Canvas (Window.Drawing_Area);
    Create_Memory_Canvas (Window.Saved_Area.mem_windows_canvas, Window.Drawing_Area);

    Create_Compatible_Bitmap (
      Window.Drawing_Area,
      Window.Saved_Bitmap,
      GWindows.Application.Desktop_Width,   -- !!
      GWindows.Application.Desktop_Height   -- !!
    );
    Select_Object (Window.Saved_Area.mem_windows_canvas, Window.Saved_Bitmap);
    Subtle_redraw (Window);

    -- Temporary: !!
    Use_GUI_Font(Window);
  end On_Create;

  procedure On_Paint (Window : in out HTML_area_type;
                      Canvas : in out GWindows.Drawing.Canvas_Type;
                      Area   : in     GWindows.Types.Rectangle_Type) is
    pragma Warnings (Off, Canvas); -- Canvas == Window.Drawing_Area
    pragma Warnings (Off, Area);
  begin
    Update_bitmap(Window);
    Display_saved_area(Window,Area);
  end On_Paint;

end Wasabee.GWin.Tabs;
