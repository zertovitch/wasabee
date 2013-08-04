-- with GWindows.Application;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Types;                    use GWindows.Types;
-- with GWindows.Colors;                   use GWindows.Colors;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body Wasabee.GWin.Display is

  procedure Clear_area (on: in out Wasa_GWin_Panel) is
  begin
    Fill_Rectangle (
      Canvas      => on.Drawing_Canvas,
      Rectangle   => (0, 0, on.Draw_Control.Width, on.Draw_Control.Height),
      Color_Const => -1
      -- Integer(GWindows.Colors.White) 
      -- O_o: GWindows bug ? Integer (signed) vs Color_Type (unsigned!)
    );
  end Clear_area;
  
  procedure Text_XY (
    on   : in out Wasa_GWin_Panel; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  )
  is
  begin
    on.Drawing_Canvas.Put(x,y,text);
  end Text_XY;

end Wasabee.GWin.Display;
