with GWindows.Types;                    use GWindows.Types;
with GWindows.Colors;                   use GWindows.Colors;

package body Wasabee_GWin.Display is

  procedure Draw(
    Canvas : in out GWindows.Drawing.Canvas_Type'Class;
    ho     : in out HTML_Object; -- in out: ho.refresh is changed
    width  : Integer;
    height : Integer)
  is

    procedure Clear_Area is
      Max_Viewport: constant Size_Type:= Viewport_Extents(Canvas);
      Max_Rectangle: constant Rectangle_Type:=
        (0, 0, Max_Viewport.Width, Max_Viewport.Height);
    begin
      Fill_Rectangle (
        Canvas => Canvas,
        Rectangle => Max_Rectangle,
        Color_Const => -1
        -- Integer(GWindows.Colors.White) -- O_o: Integer vs Color_Type (unsigned!)
      );
    end Clear_Area;

  begin
    -- Here we call the overriden Draw with display methods for GWindows 
    Clear_Area; -- !! should be as abstract, called by Draw
  end Draw;

end Wasabee_GWin.Display;
