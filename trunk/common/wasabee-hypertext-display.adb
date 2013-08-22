with Wasabee.Util;                      use Wasabee.Util;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
-- with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

package body Wasabee.Hypertext.Display is

  default_font: constant Font_descriptor:=
    (face          => U("Calibri"),
     size          => 22, -- !! pixel or pt
     modifier      => (others => False)
    );

  default_heading_pct_size: constant array(h1..h6) of Positive:=
    (200, 150, 113, 100, 80, 62);

  procedure Draw (on: in out Frame_plane'Class; o: HT_object) is
    curs_x, curs_y: Natural;
    skip_leading_blank: Boolean;
    show_next_line_break: Boolean;
    indentation: Natural;
    indentation_space_width: Positive;
    type List_marker is (none, numbered, bullets);
    current_marker: List_marker;
    tag_to_marker: constant array(ul..ol) of List_marker:= (ol => numbered, ul => bullets);
    numbering: Positive;
    area_width, area_height: Natural;

    procedure Carriage_Return is
    begin
      curs_x:= indentation_space_width * indentation;
    end Carriage_Return;

    procedure Reset_text is
    begin
      on.modifier_level:= (others => 0);
      curs_x:= 0;
      curs_y:= 0;
      skip_leading_blank:= True;
      show_next_line_break:= True;
      indentation:= 1;
      current_marker:= none;
      numbering:= 1;
      Carriage_Return; -- Get the default indentation (left margin)
    end Reset_text;

    procedure New_Line(with_marker: Boolean:= False);

    procedure Show_text(t: UTF_16_String) is
      w, h, j : Natural;
      blank_first: Boolean;
    begin
      -- implement multi-line text!!
      if t'Length > 0 then
        blank_first:= t(t'First)=' ';
        if skip_leading_blank and then blank_first then
          Show_text(t(t'First+1 .. t'Last));
        else
          skip_leading_blank:= False;
          show_next_line_break:= True; -- there is some text
          -- Message to be displayed is a chunk of blanks and non blanks
          -- NB: several blanks only on pre-formatted text (PRE tag).
          j:= t'First;
          if blank_first then
            for i in t'First+1..t'Last loop
              if t(i)=' ' then
                j:= i;
              else
                exit;
              end if;
            end loop;
            -- t'First .. j is now the largest blank
            on.Text_size(t(t'First .. j), w, h);
            curs_x:= curs_x + w;
            if curs_x > area_width then -- !! not in PRE
              New_Line;
            end if;
          else -- non-blank
            for i in t'First+1..t'Last loop
              if t(i)/=' ' then
                j:= i;
              else
                exit;
              end if;
            end loop;
            -- t'First .. j is now the largest non-blank
            on.Text_size(t(t'First .. j), w, h);
            if curs_x > 0 and then
              -- ^ we give up auto line break if word is larger than the frame
            curs_x + w > area_width
            then
              New_Line;
              skip_leading_blank:= False;
              -- ^ avoid next two words displayed without blank inbetween
            end if;
            on.Text_XY(curs_x, curs_y, t(t'First .. j));
            curs_x:= curs_x + w;
          end if;
          Show_Text(t(j+1..t'Last)); -- show the rest
        end if;
      end if;
    end Show_text;

    procedure New_Line(with_marker: Boolean:= False) is
      x, y : Natural;
    begin
      if show_next_line_break then
        on.Text_size("A", x, y);
        curs_y:= curs_y + y;
      end if;
      if show_next_line_break or with_marker then
        Carriage_Return;
        case current_marker is
          when none =>
            null;
          when numbered =>
            declare
              marker: constant Wide_String:= Positive'Wide_Image(numbering) & ". ";
              -- !! marker is hardcoded
            begin
              if with_marker then
                Show_text(marker);
                numbering:= numbering + 1;
              else
                on.Text_size(marker, x, y);
                curs_x:= curs_x + x;
              end if;
            end;
          when bullets =>
            declare
              marker: constant Wide_String:= Wide_Character'Val(8226) & ' ';
              -- !! marker is hardcoded
            begin
              if with_marker then
                Show_text(marker);
              else
                on.Text_size(marker, x, y);
                curs_x:= curs_x + x;
              end if;
            end;
        end case;
      end if;
      skip_leading_blank:= True;
      show_next_line_break:= False;
    end New_Line;

    procedure Apply_font_modifiers is
      font: Font_descriptor:= on.Get_current_font;
    begin
      for fm in Font_modifier loop
        font.modifier(fm):= on.modifier_level(fm) > 0;
      end loop;
      on.Select_font(font);
    end Apply_font_modifiers;

    procedure Draw_body(bn: p_Body_Node; level: Natural:= 0) is -- Scary name ;-) !
      mem_font: Font_descriptor;
      --
      procedure Draw_children_with_font_modification(fm: Font_modifier) is
      begin
        mem_font:= on.Get_current_font;
        on.modifier_level(fm):= on.modifier_level(fm) + 1;
        Apply_font_modifiers;
        Draw_body(bn.first_child, level + 1);
        on.modifier_level(fm):= on.modifier_level(fm) - 1;
        on.Select_font(mem_font); -- restore font at node's start
      end Draw_children_with_font_modification;
      --
    begin
      if bn = null then
        return;
      end if;
      -- !! tag default style here
      -- !! tag's style attribute - style modifier here e.g. style="color:blue"
      case bn.kind is
        when text       =>
          Show_text(S(bn.content));
        when a =>
          Draw_children_with_font_modification(underlined);
        when b | strong =>
          Draw_children_with_font_modification(bold);
        when i | em | var | dfn | cite =>
          Draw_children_with_font_modification(italic);
        when u | ins =>
          Draw_children_with_font_modification(underlined);
        when strike | del | s =>
          Draw_children_with_font_modification(strikethrough);
        when code | samp | kbd | tt =>
          mem_font:= on.Get_current_font;
          declare
            monospace_font: Font_descriptor:= mem_font;
          begin
            monospace_font.face:= U("Courier New"); -- !! hardcoded
            monospace_font.size:= mem_font.size - 3; -- !! hardcoded
            on.Select_font(monospace_font);
            Draw_body(bn.first_child, level + 1);
          end;
          on.Select_font(mem_font); -- restore font at node's start
        when abbr | acronym =>
          -- !! do something with attribute title
          Draw_body(bn.first_child, level + 1);
        when font =>
          mem_font:= on.Get_current_font;
          declare
            modified_font: Font_descriptor:= mem_font;
            mem_color: constant Color_code:= on.current_color;
          begin
            if bn.face /= "" then
              modified_font.face:= bn.face;
            end if;
            on.Select_font(modified_font);
            on.Select_text_color(bn.color);
            Draw_body(bn.first_child, level + 1);
            on.Select_text_color(mem_color);
          end;
          on.Select_font(mem_font); -- restore font at node's start
        when address =>
          New_Line;
          Draw_children_with_font_modification(italic);
          New_Line;
        when q => -- quote
          Show_text(""""); -- !! hardcoded
          Draw_body(bn.first_child, level + 1);
          Show_text(""""); -- !! hardcoded
        when h1 .. h6   =>
          New_Line;
          mem_font:= on.Get_current_font;
          declare
            font: Font_descriptor:= mem_font;
          begin
            font.size:= (font.size * default_heading_pct_size(bn.kind)) / 100;
            on.Select_font(font);
            Draw_body(bn.first_child, level + 1);
            New_Line;
          end;
          on.Select_font(mem_font); -- restore font at node's start
        when br   =>
          New_Line;
        when hr   =>
          -- Draw a nice rule here !!
          New_Line;
        when p | div | article | aside | dt | nav =>
          -- * W3 Note: Browsers automatically add some space (margin) before and after each <p>
          --   element. The margins can be modified with CSS (with the margin properties).
          -- * W3 Note: By default, browsers always place a line break before and after
          --   the <div> element. However, this can be changed with CSS.
          New_Line;
          Draw_body(bn.first_child, level + 1);
          New_Line;
        when span | dl =>
          -- * W3 Note: The <span> tag provides no visual change by itself.
          Draw_body(bn.first_child, level + 1);
        when blockquote | dd =>
          declare
            mem_indent: constant Natural:= indentation;
          begin
            indentation:= indentation + 2;
            New_Line;
            Carriage_Return;
            -- ^ force new indentation in case New_Line was skipped (e.g. DD after DT)
            Draw_body(bn.first_child, level + 1);
            indentation:= mem_indent;
          end;
          New_Line;
        when ul | ol =>
          declare
            mem_marker   : constant List_marker:= current_marker;
            mem_numbering: constant Positive:= numbering;
            mem_indent   : constant Natural:= indentation;
          begin
            current_marker:= tag_to_marker(bn.kind);
            numbering:= 1;
            indentation:= indentation + 1;
            Draw_body(bn.first_child, level + 1);
            indentation:= mem_indent;
            current_marker:= mem_marker;
            numbering:= mem_numbering;
          end;
          New_Line;
        when li =>
          New_Line(with_marker => True);
          Draw_body(bn.first_child, level + 1);
      end case;
      Draw_body(bn.next, level);
    end Draw_body;

    dummy: Natural;

  begin
    on.Clear_area;
    -- Get dimensions
    on.Area_size(area_width, area_height);
    -- Font lists cleanup
    on.font_list.Clear;
    on.Destroy_target_fonts;
    -- Startup font
    on.Select_font(default_font);
    on.Select_text_color(Black);
    on.Text_size("AA", indentation_space_width, dummy);
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

  procedure Select_text_color(on: in out Frame_plane'Class; code: in Color_Code) is
  begin
    on.Select_target_text_color(code);
    on.current_color:= code;
  end Select_text_color;

end Wasabee.Hypertext.Display;
