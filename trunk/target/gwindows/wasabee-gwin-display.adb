with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.GWin.Tabs;                 use Wasabee.GWin.Tabs;
with Wasabee.GWin.Windows;              use Wasabee.GWin.Windows;

-- with GWindows.Application;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing.Extended;
with GWindows.Errors;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Types;                    use GWindows.Types;
with GWindows.Colors;                   use GWindows.Colors;
with GWindows.DIBitmaps;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;
with System;
with System.Storage_Elements;           use System.Storage_Elements;

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
      Mouse_cursor(HT_area_type(Window.Parent.Parent.all).HT_contents, (X, Y)))
    );
  end On_Mouse_Move;

  procedure On_Click
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States) -- Keys contains only the mouse button that clicked
  is
    HT_area: HT_area_type renames HT_area_type(Window.Parent.Parent.all);
    href_URL: constant String:= Mouse_URL(HT_area.HT_contents, (X, Y));
    bw: Browser_window_type renames Browser_window_type(HT_area.Parent.all);
  begin
    if href_URL = "" then
      return; -- didn't click on an hyperlink
    end if;
    -- Here we simulate what the user would do by typing and activating the URL:
    if Keys(Middle_Button) then
      bw.New_Tab;
    end if;
    bw.control_box.url_box.Text(S2G(href_URL));
    bw.Go_on_URL;
  end On_Click;

  overriding
  procedure On_Left_Mouse_Button_Down
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (X, Y, Keys);
  begin
    Window.left_button_down_in_panel:= True;
  end On_Left_Mouse_Button_Down;

  procedure On_Left_Mouse_Button_Up
    (Window : in out Wasa_drawing_panel;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
  begin
    if Window.left_button_down_in_panel then
      -- Real click only if button was pressed inside the area
      -- Solves case where window bar is double-clicked -> window maximized -> some link under
      -- mouse pointer is activated.
      -- !! TBD: separate drag from click
      Window.On_Click(X,Y,(Left_Button => True, others => False));
    end if;
    Window.left_button_down_in_panel:= False;
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

  function To_BGR_color(code: Color_Code) return Color_Type is
    BGR: Unsigned_32;
  begin
    BGR:= Unsigned_32(code);
    return
      Color_Type(
        Shift_Left(BGR and 255,16) or
        (BGR and 16#FF00#) or
        Shift_Right(BGR,16));
  exception
    when Constraint_Error =>
      raise Constraint_Error with "Invalid color value = " & Color_Code'Image(code);
  end To_BGR_color;

  procedure Clear_area (on: in out Wasa_GWin_Panel) is
    b: Brush_Type; -- !! Provisory (GDI object leak there) !!
  begin
    b.Create_Solid_Brush(on.background_color);
    Fill_Rectangle (
      Canvas      => on.Drawing_Canvas,
      Rectangle   => (0, 0, on.Draw_Control.Width, on.Draw_Control.Height),
      Brush       => b
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
    family: Unbounded_String:= descriptor.family;
    spot: Integer:= Index(family, ",");
    medium_size_Windows: constant:= 20;
  begin
    -- We select the first name in the font family and hope it exists in Windows' font list.
    -- Windows is OK with a wrong name!
    if spot > 0 then
      family:= Unbounded_Slice(family, 1, spot-1);
    end if;
    -- Remove quotation signs
    loop
      spot:= Index(family, """");
      exit when spot = 0;
      Delete(family, spot, spot);
    end loop;
    -- Generic-family: "serif", "sans-serif", "cursive", "fantasy", "monospace"
    -- !! Should be configurable
    if family = "serif" then
      family:= U("Calibri");
    elsif family = "sans-serif" then
      family:= U("Times New Roman");
    elsif family = "cursive" then
      family:= U("Monotype Corsiva");
    elsif family = "fantasy" then
      family:= U("Copperplate Gothic");
    elsif family = "monospace" then
      family:= U("Courier New");
    end if;
    f.Create_Font(
      Name       => S2G(S(family)),
      Size       => (descriptor.size * medium_size_Windows) / 100,
      Weight     => w(descriptor.modifier(bold) > 0),
      Italics    => descriptor.modifier(italic) > 0,
      Underline  => descriptor.modifier(underlined) > 0,
      Strike_Out => descriptor.modifier(strikethrough) > 0,
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

  procedure Text_at (
    on   : in out Wasa_GWin_Panel;
    p    : in     Wasabee.Hypertext.Point;
    text : in     UTF_16_String
  )
  is
  begin
    on.Drawing_Canvas.Background_Mode(Transparent);
    on.Drawing_Canvas.Put(p.x, p.y, text);
  end Text_at;

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

  procedure Select_target_fore_color(on: in out Wasa_GWin_Panel; code: in Color_Code) is
  begin
    on.Drawing_Canvas.Text_Color(To_BGR_color(code));
  end Select_target_fore_color;

  procedure Select_target_back_color(on: in out Wasa_GWin_Panel; code: in Color_Code) is
  begin
    on.background_color:= To_BGR_color(code);
  end Select_target_back_color;

  procedure Rectangle (on: in out Wasa_GWin_Panel; coords: Box) is
    b: Brush_Type; -- !! Provisory (GDI object leak there) !!
  begin
    b.Create_Solid_Brush(on.Drawing_Canvas.Text_Color);
    on.Drawing_Canvas.Frame_Rectangle((coords.p1.x, coords.p1.y, coords.p2.x, coords.p2.y), b);
  end Rectangle;

  procedure Full_Rectangle (on: in out Wasa_GWin_Panel; coords: Box) is
    b: Brush_Type; -- !! Provisory (GDI object leak there) !!
  begin
    b.Create_Solid_Brush(on.background_color);
    on.Drawing_Canvas.Fill_Rectangle((coords.p1.x, coords.p1.y, coords.p2.x, coords.p2.y), b);
  end Full_Rectangle;

  procedure Put_RGB_Bitmap (
    on     : in out Wasa_GWin_Panel;
    bitmap :        Wasabee.Images.Bitmap_type;
    coords :        Box
  )
  is
    bkg, bmp: GWindows.Drawing_Objects.Bitmap_Type;
    --
    procedure Slow_display is
      i: Integer:= 0;
    begin
      for y in reverse 1..bitmap.height loop
        for x in 1..bitmap.width loop
          on.Drawing_Canvas.Point(x-1+coords.p1.x, y-1+coords.p1.y,
            Color_Type(bitmap.data(i+2))+
            Color_Type(bitmap.data(i+1))*256+
            Color_Type(bitmap.data(i  ))*65536
           ) ;
          i:= i + 4;
        end loop;
      end loop;
    end Slow_display;
    pragma Unreferenced (Slow_display);
    --
    use Wasabee.Images;
    --
    procedure Manual_blending is
      w: constant Natural:= Width(coords);  -- target width
      h: constant Natural:= Height(coords); -- target height
      use GWindows.DIBitmaps;
      dibh: DIB_Info_Header_Type;
      pxls_bkg, pxls_bmp: System.Address;
      IA_bkg, IA_bmp  : Integer_Address;
      temp_canvas_bkg, temp_canvas_bmp : Memory_Canvas_Type;
      begin
      --  This DIBitmap copy works:
      --
      dibh.Private_Info.Color_Depth:= RGB;
      dibh.Private_Info.Width:= w;
      dibh.Private_Info.Height:= h;
      dibh.Private_Info.Image_Size:= w * h * 4;
      Create_Memory_Canvas (temp_canvas_bkg, on.Drawing_Canvas);
      put_line("Bitmap operation 1: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      Create_DIB_Section(temp_canvas_bkg, dibh, bkg, pxls_bkg);
      put_line("Bitmap operation 2: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      BitBlt (temp_canvas_bkg, 0, 0, w, h, on.Drawing_Canvas, coords.p1.x, coords.p1.y, SRCCOPY);
      put_line("Bitmap operation 3: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      --
      -- Copy image, eventually resized
      --
      Create_Memory_Canvas (temp_canvas_bmp, on.Drawing_Canvas);
      put_line("Bitmap operation 4: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      Create_DIB_Section(temp_canvas_bmp, dibh, bmp, pxls_bmp);
      put_line("Bitmap operation 5: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      temp_canvas_bmp.Paint_Bitmap(
        Bitmap                => bmp,
        X                     => coords.p1.x,
        Y                     => coords.p1.y,
        Width                 => w,
        Height                => h,
        Source_Width          => bitmap.width,
        Source_Height         => bitmap.height
      );
      put_line("Bitmap operation 6: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      --
      -- Blending
      --
      IA_bkg:= To_Integer(pxls_bkg);
      IA_bmp:= To_Integer(pxls_bmp);
      for i in reverse 1..w*h loop
        for fc in Integer_Address'(0)..2 loop -- r,g,b
          declare
            dibmp: Byte;
            for dibmp'Address use To_Address(IA_bmp+fc);
            dibmp_a: Byte; -- alpha value
            for dibmp_a'Address use To_Address(IA_bmp+3);
            dibkg: Byte;
            for dibkg'Address use To_Address(IA_bkg);
          begin
            dibkg:= dibmp; -- !! just a copy so far
          end;
        end loop;
        IA_bkg:= IA_bkg + 4;
        IA_bmp:= IA_bmp + 4;
      end loop;
      --
      --  Copy bkg to main canvas
      --
      BitBlt (on.Drawing_Canvas, coords.p1.x, coords.p1.y, w, h, temp_canvas_bkg, 0, 0, SRCCOPY);
      put_line("Bitmap operation 7: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
    end Manual_blending;
    pragma Unreferenced (Manual_Blending);
    --
    procedure Transparent_display is
      w: constant Natural:= bitmap.width;  -- source width
      h: constant Natural:= bitmap.height; -- source height
      use GWindows.DIBitmaps;
      dibh: DIB_Info_Header_Type;
      pxls_bmp: System.Address;
      IA_bmp  : Integer_Address;
      temp_canvas_bmp : Memory_Canvas_Type;
      use GWindows.Drawing.Extended;
      Blending : constant Blend_Function :=
       (BlendOp             => AC_SRC_OVER,
        BlendFlags          => 0,            -- Must be zero.
        SourceConstantAlpha => 127, --SCA_Opaque,
        AlphaFormat         => 0 --AC_SRC_ALPHA
       );
    begin
      --  This DIBitmap copy works:
      --
      dibh.Private_Info.Color_Depth:= RGB; -- GWindows.DIBitmaps.RGB is actually RGBA (32)
      dibh.Private_Info.Width:= w;
      dibh.Private_Info.Height:= h;
      dibh.Private_Info.Image_Size:= w * h * 4;
      dibh.X_Pixels_Per_Meter:= 1000;
      dibh.Y_Pixels_Per_Meter:= 1000;
      Create_Memory_Canvas (temp_canvas_bmp, on.Drawing_Canvas);
      put_line("Bitmap operation 1: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      Create_DIB_Section(temp_canvas_bmp, dibh, bmp, pxls_bmp);
      put_line("Bitmap operation 2: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      --
      -- Fill the DIB bitmap
      --
      IA_bmp:= To_Integer(pxls_bmp);
      for i in bitmap.data'range loop
        declare
          dibmp: Byte;
          for dibmp'Address use To_Address(IA_bmp);
        begin
          dibmp:= 127; -- 127: gray semi-transp. -- bitmap.data(i);
        end;
        IA_bmp:= IA_bmp + 1;
      end loop;
      -- bmp.Load_Bitmap_From_File("e:\temp\w.bmp");
      Select_Object (temp_canvas_bmp, bmp);
      put_line("Bitmap operation 3: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
      AlphaBlend
       (Canvas_Type (on.Drawing_Canvas), coords.p1.x, coords.p1.y,
         Width(coords), Height(coords), temp_canvas_bmp, 0, 0,
         w, h, Blending );
         -- AlphaBlend   ... , blending  -> failure, err # 87 incorrect param
         -- StretchBlt -> ok (only image reversed)!...
      put_line("Bitmap operation 4: " &
        G2S(GWindows.Errors.Get_Last_Error) &
        Integer'Image(GWindows.Errors.Get_Last_Error));
    end Transparent_display;
    --
    procedure Opaque_display is
    begin
      bmp.Create_Bitmap(
        Width          => bitmap.width,
        Height         => bitmap.height,
        Planes         => 1,
        Bits_Per_Pixel => 32,
        Bits           => bitmap.data(0)'address
      );
      --  NB: This call to Create_Bitmap does nothing if device is not 32 bit per pixel
      --  Use DI (Device Independent) instead if screen device is not 32 bit (not seen yet)
      --
      on.Drawing_Canvas.Paint_Bitmap(
        Bitmap                => bmp,
        X                     => coords.p1.x,
        Y                     => coords.p1.y,
        Width                 => Width(coords),
        Height                => Height(coords),
        Source_Width          => bitmap.width,
        Source_Height         => bitmap.height
      );
    end Opaque_display;
  begin
    -- Slow_display; return; -- !! Snail mode drawing
    if bitmap.data = null then
      return;
    end if;
    if bitmap.transparency AND FALSE then
      Transparent_display;
    else
      Opaque_display;
    end if;
  end Put_RGB_Bitmap;

end Wasabee.GWin.Display;
