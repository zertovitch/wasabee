with Wasabee.Util;                      use Wasabee.Util;

-- with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

package body Wasabee.Display is

  default_font: constant Font_descriptor:=
    (face          => U("Calibri"),
     size          => 22, -- !! pixel or pt
     bold          => False,
     italic        => False,
     underlined    => False,
     strikethrough => False
    );

  procedure Draw (on: in out Frame_plane'Class; o: HTML_object) is
  
    curs_x, curs_y: Natural;
    skip_leading_blank: Boolean;
    show_next_line_break: Boolean;
    
    procedure Reset_text is
    begin
      on.bold_level:= 0;     
      on.italic_level:= 0;  
      on.underlined_level:= 0;  
      on.strikethrough_level:= 0;
      curs_x:= 0;
      curs_y:= 0;
      skip_leading_blank:= True;
      show_next_line_break:= True;
    end Reset_text;
  
    procedure New_Line is
      x, y : Natural;
    begin
      if show_next_line_break then
        on.Text_size("A", x, y);
        curs_y:= curs_y + y;
        curs_x:= 0;
        skip_leading_blank:= True;
      end if;
      show_next_line_break:= False;
    end New_Line;
  
    procedure Show_text(t: UTF_16_String) is
      x, y : Natural;
    begin
      -- implement multi-line text!!
      if t'Length > 0 then
        if skip_leading_blank and then t(t'First)=' ' then
          Show_text(t(t'First+1 .. t'Last));
        else
          on.Text_XY(curs_x, curs_y, t);
          on.Text_size(t, x, y);
          curs_x:= curs_x + x;
          show_next_line_break:= True; -- there was some text
        end if;
        skip_leading_blank:= False;
      end if;
    end Show_text;    
  
    procedure Apply_font_modifiers is
      font: Font_descriptor:= on.Get_current_font;
    begin
      font.bold          := on.bold_level > 0;
      font.italic        := on.italic_level > 0;
      font.underlined    := on.underlined_level > 0;
      font.strikethrough := on.strikethrough_level > 0;
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
        when b | strong =>
          on.bold_level:= on.bold_level + 1;
          Apply_font_modifiers;
          Draw_body(bn.part, level + 1);
          on.bold_level:= on.bold_level - 1;
          Apply_font_modifiers;
        when i | em | var | dfn =>
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
        when strike =>
          on.strikethrough_level:= on.strikethrough_level + 1;
          Apply_font_modifiers;
          Draw_body(bn.part, level + 1);
          on.strikethrough_level:= on.strikethrough_level - 1;
          Apply_font_modifiers;
        when code | samp | kbd | tt =>
          declare
            mem_font: constant Font_descriptor:= on.Get_current_font;
            monospace_font: Font_descriptor:= mem_font;
          begin
            monospace_font.face:= U("Courier New"); -- !! hardcoded
            on.Select_font(monospace_font);
            Draw_body(bn.part, level + 1);
            on.Select_font(mem_font); -- restore previous font
          end;
        when h1 | h2 | h3 | h4 | h5 | h6   =>
          -- Select style here !!
          New_Line;
          Draw_body(bn.part, level + 1);
          New_Line;
        when br   =>
          New_Line;
        when hr   =>
          -- Draw a nice rule here !!
          New_Line;
        when p =>
          -- paragraph style here !!
          -- * W3 Note: Browsers automatically add some space (margin) before and after each <p>
          --   element. The margins can be modified with CSS (with the margin properties).
          New_Line;
          Draw_body(bn.part, level + 1);
          New_Line;
        when div =>
          -- division style here !!
          -- * W3 Note: By default, browsers always place a line break before and after
          --   the <div> element. However, this can be changed with CSS.
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
    --
    Reset_text;
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
