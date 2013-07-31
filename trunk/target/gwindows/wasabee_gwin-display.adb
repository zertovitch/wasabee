with GWindows.Application;
with GWindows.Types;                    use GWindows.Types;
-- with GWindows.Colors;                   use GWindows.Colors;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body Wasabee_GWin.Display is

  procedure Text_XY (
    on   : in out Wasa_GWin_Canvas; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  )
  is
  begin
    on.mem_windows_canvas.Put(x,y,text);
  end Text_XY;

  procedure Draw_object(
    Canvas : in out Wasa_GWin_Canvas;
    ho     : in out HTML_Object; -- in out: ho.refresh is changed
    width  : Integer;
    height : Integer)
  is
    -- Max_Viewport: constant Size_Type:= Viewport_Extents(Canvas);
    Max_Rectangle: constant Rectangle_Type:=
      (0, 0, 
       GWindows.Application.Desktop_Width, -- !! Max_Viewport.Width, 
       GWindows.Application.Desktop_Height -- !! Max_Viewport.Height
      );

    procedure Clear_Area is
    begin
      Fill_Rectangle (
        Canvas => Canvas.mem_windows_canvas,
        Rectangle => Max_Rectangle,
        Color_Const => -1
        -- Integer(GWindows.Colors.White) -- O_o: Integer vs Color_Type (unsigned!)
      );
    end Clear_Area;

  begin
    -- Here we call the overriden Draw with display methods for GWindows 
    Clear_Area; -- !! should be as abstract, called by Draw
    Canvas.Draw(ho);
    Canvas.Text_XY(
        10, 40,
        "Dro! Dro!" & 
        Integer'Wide_Image(Max_Rectangle.Right) & 
        Integer'Wide_Image(Max_Rectangle.Bottom) &
        Integer'Wide_Image(width) & 
        Integer'Wide_Image(height)
        );
  end Draw_object;

end Wasabee_GWin.Display;
