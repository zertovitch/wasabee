with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.GWin.Tabs;                 use Wasabee.GWin.Tabs;
with Wasabee.GWin.Windows;              use Wasabee.GWin.Windows;

-- with GWindows.Application;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Types;                    use GWindows.Types;
with GWindows.Colors;                   use GWindows.Colors;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;

package body Wasabee.GWin.Display is

  procedure On_Mouse_Move (Window : in out Wasa_drawing_panel;
                           X      : in     Integer;
                           Y      : in     Integer;
                           Keys   : in     Mouse_Key_States) is
  pragma Unreferenced (Keys);
  --
    procedure Init_cursors is
      wasa_to_gwin_mouse_cursor_id: constant array(Mouse_cursor_style) of Integer:=
        (arrow => IDC_ARROW, finger => IDC_HAND, I_beam => IDC_IBEAM);
    begin
      for c in Mouse_cursor_style loop
        Window.wasa_to_gwin_mouse_cursor(c):= Load_System_Cursor(wasa_to_gwin_mouse_cursor_id(c));
      end loop;
      Window.gwin_cursors_initialized:= True;
    end Init_cursors;
    --
  begin
    if not Window.gwin_cursors_initialized then
      Init_cursors;
    end if;
    -- put_line(x'Img & y'Img);
    Set_Cursor(Window.wasa_to_gwin_mouse_cursor(
      Mouse_cursor(HT_area_type(Window.Parent.Parent.all).HT_contents, X, Y))
    );
  end On_Mouse_Move;

  procedure On_Click
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States) -- Keys contains only the mouse button that clicked
  is
    HT_area: HT_area_type renames HT_area_type(Window.Parent.Parent.all);
    partial_URL: constant String:= Mouse_partial_URL(HT_area.HT_contents, X, Y);
    bw: Browser_window_type renames Browser_window_type(HT_area.Parent.all);
  begin
    if partial_URL /= "" then
      -- Here we simulate what the user would do by typing and activating the URL:
      if Keys(Middle_Button) then
        bw.New_Tab;
      end if;
      bw.control_box.url_box.Text(S2G(Build_URL(S(HT_area.URL), partial_URL)));
      bw.Go_on_URL;
    end if;
  end On_Click;

  procedure On_Left_Mouse_Button_Up
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
  begin
    Window.On_Click(X,Y,(Left_Button => True, others => False));
  end On_Left_Mouse_Button_Up;

  procedure On_Middle_Mouse_Button_Up
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
  begin
    Window.On_Click(X,Y,(Middle_Button => True, others => False));
  end On_Middle_Mouse_Button_Up;

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
    scroll_panel: HT_area_type
      renames HT_area_type(on.Draw_Control.Parent.Parent.all);
    sz: GWindows.Types.Size_Type;
  begin
    sz:= on.Draw_Control.Client_Area_Size;
    Panel_Size (scroll_panel, sz.Width, Integer'Max(sz.Height,to));
    -- The canvas handle has changed! Arghhh!...
    Get_Canvas (on.Draw_Control, on.Drawing_Canvas);
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
    w, h :    out Natural
  )
  is
    dims: constant GWindows.Types.Size_Type:= on.Drawing_Canvas.Text_Output_Size(text);
  begin
    w:= dims.Width;
    h:= dims.Height;
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

  procedure Rectangle (on: in out Wasa_GWin_Panel; coords: Box) is
    b: Brush_Type; -- !! Provisory (GDI object leak there) !!
  begin
    Create_Stock_Brush(b, Light_Gray_Brush);
    on.Drawing_Canvas.Frame_Rectangle((coords.p1.x, coords.p1.y, coords.p2.x, coords.p2.y), b);
  end Rectangle;

end Wasabee.GWin.Display;
