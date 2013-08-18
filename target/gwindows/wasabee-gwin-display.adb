with Wasabee.Util;                      use Wasabee.Util;

-- with GWindows.Application;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Types;                    use GWindows.Types;
-- with GWindows.Colors;                   use GWindows.Colors;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Text_IO;                       use Ada.Text_IO;
with GWindows.Colors;                   use GWindows.Colors;

with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;

package body Wasabee.GWin.Display is

  procedure Clear_area (on: in out Wasa_GWin_Panel) is
  begin
    Fill_Rectangle (
      Canvas      => on.Drawing_Canvas,
      Rectangle   => (0, 0, on.Draw_Control.Width, on.Draw_Control.Height),
      Color_Const => -1
      -- Integer(GWindows.Colors.White) 
      -- O_o: GWindows bug ? Integer (signed) vs Color_Type (unsigned!) !!
    );
  end Clear_area;

  procedure Area_size (on: Wasa_GWin_Panel; w,h: out Natural) is
    sz: GWindows.Types.Size_Type;
  begin
    sz:= on.Draw_Control.Client_Area_Size;
    w:= Natural(sz.Width);
    h:= Natural(sz.Height);
    Put_Line("Wasa_GWin_Panel Area_size=" & w'Img & h'Img);
  end Area_size;

  procedure Extend_area_height (on: in out Wasa_GWin_Panel; to: Natural) is
  begin
    null; -- !! 
  end Extend_area_height;

  procedure Create_target_font(
    on         : in out Wasa_GWin_Panel; 
    descriptor : in     Font_descriptor;
    new_index  : in     Positive
  )
  is
    f: constant p_Font_Type:= new Font_Type;
    w: constant array(Boolean) of Font_Weight_Type:= (False => FW_DONTCARE, True => FW_BOLD);
  begin
    f.Create_Font(
      Name       => S2G(S(descriptor.face)),
      Size       => descriptor.size,
      Weight     => w(descriptor.modifier(bold)),
      Italics    => descriptor.modifier(italic),
      Underline  => descriptor.modifier(underlined),
      Strike_Out => descriptor.modifier(strikethrough),
      Angle      => 0,
      Char_Set   => ANSI_CHARSET -- !!
    );
    on.gw_font_list.Append(f);
    if on.gw_font_list.Last_Index /= new_index then
      raise Constraint_Error with "mismatch between font_list and gw_font_list";
    end if;
  end Create_target_font;
  
  procedure Select_target_font(
    on         : in out Wasa_GWin_Panel; 
    index      : in     Positive
  )
  is
  begin
    on.Drawing_Canvas.Select_Object(on.gw_font_list.Element(index).all);
  end Select_target_font;

  procedure Destroy_target_fonts(on: in out Wasa_GWin_Panel) is
    f_curs: GW_Font_Vectors.Cursor:= on.gw_font_list.First;
    procedure Dispose is new Ada.Unchecked_Deallocation(Font_Type, p_Font_Type);
    use GW_Font_Vectors;
    p: p_Font_Type;
  begin
    while f_curs /= GW_Font_Vectors.No_Element loop
      p:= Element(f_curs);
      Dispose(p); -- Will Finalize the font as well
      f_curs:= Next(f_curs);
    end loop;
    on.gw_font_list.Clear;
  end Destroy_target_fonts; 

  procedure Text_XY (
    on   : in out Wasa_GWin_Panel; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  )
  is
  begin
    on.Drawing_Canvas.Put(x,y,text);
  end Text_XY;

  procedure Text_size (
    on   : in out Wasa_GWin_Panel; 
    text : in     UTF_16_String; 
    x,y  :    out Natural
  )
  is
    dims: constant GWindows.Types.Size_Type:=
      on.Drawing_Canvas.Text_Output_Size(text);
  begin
    x:= dims.Width;
    y:= dims.Height;
  end Text_size;

  procedure Select_target_text_color(on: in out Wasa_GWin_Panel; code: in Color_Code) is
    BGR: Unsigned_32;
  begin
    if code /= Default_Color then
      BGR:= Unsigned_32(code);
      on.Drawing_Canvas.Text_Color(
        Color_Type(
          Shift_Left(BGR and 255,16) or 
          (BGR and 16#FF00#) or 
          Shift_Right(BGR,16))
      );
    end if;
  end Select_target_text_color;

end Wasabee.GWin.Display;
