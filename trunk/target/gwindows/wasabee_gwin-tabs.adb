with Wasabee_GWin.Display;

package body Wasabee_GWin.Tabs is

  overriding
  procedure On_Create (Window : in out Tab_Type) is
  begin
    Window.HTML_contents.refresh:= nothing; -- blank page so far
  end On_Create;

  procedure Display_saved_area(
    window : in out Tab_type;
    area   :        GWindows.Types.Rectangle_Type)
  is
  begin
    BitBlt( window.Drawing_Area,
            area.Left,
            area.Top,
            area.Right-area.Left,
            area.Bottom-area.top,
            Window.Saved_Area,
            area.Left,
            area.Top
          );
    --### For monitoring (interesting!): ###
    -- Line(window.Drawing_Area,Area.Left,Area.Top,Area.Right,Area.Bottom);
  end Display_saved_area;

  procedure Update_bitmap(Window : in out Tab_type ) is
  begin
    if Window.HTML_contents.refresh /= nothing then
      Wasabee_GWin.Display.Draw(
        Window.Saved_Area,
        Window.HTML_contents,
        Width(Window),
        Height(Window)
      );
    end if;
  end Update_bitmap;

  procedure Subtle_redraw (Window : in out Tab_type ) is
    initial_refresh: constant Refresh_mode:= Window.HTML_contents.refresh;
  begin
    Update_bitmap(Window);
    if initial_refresh /= nothing then
      Redraw(Window); -- Repaint visible area(s) on screen
                      -- only if bitmap has changed
    end if;
  end Subtle_redraw;

  procedure On_Paint (Window : in out Tab_type;
                      Canvas : in out GWindows.Drawing.Canvas_Type;
                      Area   : in     GWindows.Types.Rectangle_Type) is
    pragma Warnings (Off, Canvas); -- Canvas == Window.Drawing_Area
    pragma Warnings (Off, Area);
  begin
    Update_bitmap(Window);
    Display_saved_area(Window,Area);
  end On_Paint;

end Wasabee_GWin.Tabs;
