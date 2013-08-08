with Wasabee.Util;                      use Wasabee.Util;

-- with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

package body Wasabee.Display is

  default_font: constant Font_descriptor:=
    (face       => U("Calibri"),
     size       => 20,
     bold       => False,
     italic     => False,
     underlined => False
    );

  procedure Draw (on: in out Frame_plane'Class; o: HTML_object) is
  
    curs_x, curs_y: Natural;
  
    procedure New_Line is
      x, y : Natural;
    begin
      on.Text_size("A", x, y);
      curs_y:= curs_y + y;
      curs_x:= 0;
    end New_Line;
  
    procedure Show_text(t: UTF_16_String) is
      x, y : Natural;
    begin
      -- multi-line!!
      on.Text_XY(curs_x, curs_y, t);
      on.Text_size(t, x, y);
      curs_x:= curs_x + x;
    end Show_text;    
  
    procedure Apply_font_modifiers is
      font: Font_descriptor;
    begin
      font:= on.Get_current_font;
      font.bold      := on.bold_level > 0;
      font.italic    := on.italic_level > 0;
      font.underlined:= on.underlined_level > 0;
      on.Select_font(font);
    end Apply_font_modifiers;
  
    procedure Draw_body(bn: p_Body_Node; level: Natural:= 0) is -- Scary name ;-) !
    begin
      if bn = null then
        return;
      end if;
      case bn.kind is
        when text       =>
          Show_text(S(bn.content));
        when b =>
          on.bold_level:= on.bold_level + 1;
          Apply_font_modifiers;
          Draw_body(bn.part, level + 1);
          on.bold_level:= on.bold_level - 1;
          Apply_font_modifiers;
        when i =>
          on.italic_level:= on.italic_level + 1;
          Apply_font_modifiers;
          Draw_body(bn.part, level + 1);
          on.italic_level:= on.italic_level - 1;
          Apply_font_modifiers;
        when u =>
          on.underlined_level:= on.underlined_level + 1;
          Apply_font_modifiers;
          Draw_body(bn.part, level + 1);
          on.underlined_level:= on.underlined_level - 1;
          Apply_font_modifiers;
        when h1|h2|h3| h4|h5|h6   =>
          -- Select style here !!
          New_Line;
          Draw_body(bn.part, level + 1);
          New_Line;
      end case;
      Draw_body(bn.next, level);
    end Draw_body;
    
  begin
    on.Clear_area;
    -- Font lists cleanup
    on.font_list.Clear;
    on.Destroy_target_fonts;
    -- Startup font
    on.Select_font(default_font);
    on.bold_level:= 0;     
    on.italic_level:= 0;  
    on.underlined_level:= 0;  
    --
    curs_x:= 0;
    curs_y:= 0;
    --
    Draw_body(o.the_body);    
  end Draw;
  
  procedure Select_font(
    on         : in out Frame_plane'Class; 
    descriptor : in     Font_descriptor
  )
  is
    index: constant Natural:= on.font_list.Find_Index(descriptor);
  begin
    if index = 0 then -- A font with this face name, size, boldness, etc. doesn't exist yet
      on.font_list.Append(descriptor);
      on.Create_target_font(descriptor, on.font_list.Last_Index);
      on.Select_target_font(on.font_list.Last_Index);
      on.current_font:= on.font_list.Last_Element;
    else
      on.Select_target_font(index);
      on.current_font:= on.font_list.Element(index);
    end if;    
  end Select_font;

  function Get_current_font(on : in Frame_plane'Class) return Font_descriptor is
  begin
    return on.current_font;
  end Get_current_font;

end Wasabee.Display;
