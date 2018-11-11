with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.Hypertext.Display.Text;    use Wasabee.Hypertext.Display.Text;

-- with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
-- with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

package body Wasabee.Hypertext.Display is

  visual_debugging: constant Boolean:= False;

  ------------------------------------------------
  --                Draw_element                --
  -- Draw an hypertext element and its children --
  ------------------------------------------------

  procedure Draw_element(
    bn   : in     p_Body_Node;
    on   : in out Frame_plane'Class;
    o    : in out HT_object;
    mode : in     Draw_mode;
    level: in     Natural
  )
  is
    procedure Draw_children is
      p: p_Body_node;
    begin
      if bn.optional_style /= null and then bn.optional_style.hiding_level > 0 then
        return;
      end if;
      --
      -- Draw the children
      --
      Draw_element(bn.first_child, on, o, mode, level + 1);
      --
      -- Compute the common bounding box of all children:
      --
      p:= bn.first_child;
      if p = null then
        return;
      end if;
      bn.bounding_box:= p.bounding_box;  -- Begin with first child
      p:= p.next;
      while p /= null loop  --  Extend the bounding box with all other children
        bn.bounding_box:= Max(bn.bounding_box, p.bounding_box);
        p:= p.next;
      end loop;
    end Draw_children;
    --
    mem_indent, dummy : Natural;
    mem_numbering : Positive;
    mem_pre       : Boolean;
    mem_style     : Local_Style;
  begin
    if bn = null then
      return;
    end if;
    if bn.optional_style /= null then
      if (mode = all_but_images or mode = full) and then
         bn.optional_style.back_color /= on.current_style.back_color
      then
        on.Select_back_color(bn.optional_style.back_color);
        on.Full_Rectangle(bn.bounding_box);
      end if;
      mem_style:= on.current_style;
      on.current_style:= bn.optional_style.all;
      on.Select_fore_color(on.current_style.fore_color);
      on.Select_font(on.current_style.font);
    end if;
    if mode = invisible then
      bn.bounding_box:= (on.curs, on.curs); -- a priori, a pixel... will be extended hereafter.
    end if;
    if bn.optional_style /= null then
      -- !! advance on left-top margins
      if mode = all_but_images or mode = full then
        on.Select_fore_color(bn.optional_style.border_color);
        case bn.optional_style.border_style is
          when solid =>
            for i in 0..bn.optional_style.border_width-1 loop
              on.Rectangle((bn.bounding_box.p1+(i,i), bn.bounding_box.p2-(i,i)));
            end loop;
          when none =>
            null;
          when others =>
            null; -- !!
        end case;
        on.Select_fore_color(bn.optional_style.fore_color);
      end if;
    end if;
    case bn.kind is
      when section =>
        Draw_children;
      when b0dy =>
        Draw_children;
      when body_text   =>
        on.latest_text_height:= 0;
        if mode = invisible then
          bn.bounding_box.p1:= on.curs;
        end if;
        Show_text(on, S(bn.content), mode);
        if mode = invisible then
          bn.bounding_box.p2:= (on.curs.x, on.curs.y + on.latest_text_height);
        end if;
      when img =>
        case mode is
          when invisible =>
            bn.bounding_box.p1:= on.curs;
            if bn.optional_style.dimensions.x <= auto then  --  No width style or attribute, we take width from image
              if bn.optional_style.dimensions.y <= auto then  --  No dimensions by style or attributes, we take dimensions from image
                bn.optional_style.dimensions.x:= bn.bitmap.width;
                bn.optional_style.dimensions.y:= bn.bitmap.height;
              else  --  Height, but no width specified
                if bn.bitmap.height > 0 then
                  bn.optional_style.dimensions.x:= (bn.bitmap.width * bn.optional_style.dimensions.y) / bn.bitmap.height;
                end if;
              end if;
            elsif bn.optional_style.dimensions.y <= auto then  --  Width, but no height specified
              if bn.bitmap.width > 0 then
                bn.optional_style.dimensions.y:= (bn.bitmap.height * bn.optional_style.dimensions.x) / bn.bitmap.width;
              end if;
            else
              null;  --  Both width and height were specified with style or attributes
            end if;
            bn.bounding_box.p2:= on.curs + bn.optional_style.dimensions;
          when all_but_images =>
            null;
          when images_only | full =>
            on.Put_RGB_Bitmap(bn.bitmap, bn.bounding_box);
            -- on.Rectangle(bn.bounding_box);
        end case;
        Max(bn.optional_style.dimensions.x, 0);
        Max(bn.optional_style.dimensions.y, 0);
        Advance_vertically(on, bn.optional_style.dimensions.y);
        New_Line(on, mode);
      when a | u | ins | b | strong | i | em | var | dfn | cite | strike | del | s | font =>
        --  All font modifications have already been set in bn.optional_style
        --  by Post_loading_processing.Apply_styles
        Draw_children;
      when big =>
        Draw_children;
      when small | sup | sub => -- !! missing alignment
        Draw_children;
      when code | samp | kbd | tt | pre =>
        if bn.kind = pre then
          New_Line(on, mode);
          mem_pre:= on.preformatted_text;
          on.preformatted_text:= True;
        end if;
        Draw_children;
        if bn.kind = pre then
          New_Line(on, mode);
          on.preformatted_text:= mem_pre;
        end if;
      when abbr | acronym =>
        Draw_children; -- !! show label with attribute title
      when details | summary =>
        Draw_children;
      when q => -- quote
        Show_text(on, """", mode); -- !! hardcoded
        Draw_children;
        Show_text(on, """", mode); -- !! hardcoded
      when h1 .. h6 | address   =>
        New_Line(on, mode);
        Draw_children;
        New_Line(on, mode);
      when br   =>
        on.show_next_line_break:= True;  --  <br/> are cumulative, cf ex. 1
        New_Line(on, mode);
      when hr   =>
        New_Line(on, mode);
        if mode /= invisible then
          on.Rectangle((on.curs, (on.area_width, on.curs.y + bn.hr_height)));
        end if;
        Advance_vertically(on, bn.hr_height);
      when p | div | article | aside | dt | nav | figcaption =>
        New_Line(on, mode);
        Draw_children;
        New_Line(on, mode);
      when span | dl =>
        Draw_children;
      when blockquote | dd | figure =>
        mem_indent:= on.indentation;
        on.indentation:= on.indentation + 2;
        New_Line(on, mode);
        Carriage_Return(on);
        -- ^ force new indentation in case New_Line was skipped (e.g. DD after DT)
        Draw_children;
        on.indentation:= mem_indent;
        New_Line(on, mode);
      when ul | ol =>
        mem_indent        := on.indentation;
        mem_numbering     := on.numbering;
        on.numbering:= 1;
        on.indentation:= on.indentation + 1;
        if on.current_style.list_style in Ordered_list_styling then
          on.Text_size(
            Integer'Wide_Image(bn.item_count * 10), -- we add a '0' to be sure we have enough
            on.marker_width, dummy
          );
        end if;
        Draw_children;
        on.indentation    := mem_indent;
        on.numbering      := mem_numbering;
        on.new_line_before_writing:= True;
      when li =>
        New_Line(on, mode, with_marker => True);
        Draw_children;
      when area | col | input | param =>
        null;
      when table =>
        New_Line(on, mode);
        if bn.optional_style /= null then
          on.curs:= on.curs + bn.optional_style.border_spacing;
        end if;
        Draw_children;
        if bn.optional_style /= null then
          on.curs:= on.curs + bn.optional_style.border_spacing;
        end if;
        bn.bounding_box:= Max(bn.bounding_box, (on.curs, on.curs));
        New_Line(on, mode);
      when tr =>
        New_Line(on, mode);  -- !! Should not be needed...
        Draw_children;
      when td | th =>
        if mode /= invisible then
          on.curs:= bn.bounding_box.p1; -- Force alignment done by Fit_bounding_boxes
        end if;
        Draw_children;
    end case;
    if visual_debugging and then mode /= invisible then
      on.Select_fore_color(16#000088#);
      on.Rectangle(bn.bounding_box);
      -- Show the bounding box for debugging purposes
    end if;
    if bn.optional_style /= null then
      on.current_style:= mem_style;
      on.Select_fore_color(on.current_style.fore_color);
      on.Select_font(on.current_style.font);
    end if;
    Draw_element(bn.next, on, o, mode, level);
  end Draw_element;

  --------------------------------------------------
  --                   Draw                       --
  -- The main hypertext class-wide drawing method --
  --------------------------------------------------

  procedure Draw (
    on   : in out Frame_plane'Class;
    o    : in out HT_object;
    mode :        Draw_mode
  )
  is
    dummy: Natural;
    p: p_Body_node;
  begin
    -- Get dimensions:
    on.Area_size(on.area_width, on.area_height);
    -- Font lists cleanup:
    on.font_list.Clear;
    on.Destroy_target_fonts;
    -- Startup style:
    on.current_style:= default_style;  -- !! should be configurable
    on.Select_font(on.current_style.font);
    on.Select_fore_color(on.current_style.fore_color);
    on.Select_back_color(on.current_style.back_color);
    on.Text_size("AA", on.indentation_space_width, dummy);
    --
    Reset_text(on);
    o.main_bounding_box:= (on.curs, on.curs); -- One-pixel-page so far...
    --
    Draw_element(o.the_body, on, o, mode, level => 0);
    --
    -- Drawing done, extend the bounding box with all children:
    p:= o.the_body;
    while p /= null loop
      o.main_bounding_box:= Max(o.main_bounding_box, p.bounding_box);
      p:= p.next;
    end loop;
  end Draw;

  procedure Select_main_background (on: in out Frame_plane'Class; o: in out HT_object) is
  begin
    -- Usually the top node is <BODY>.
    -- Background color is set by attribute bgcolor (old way) or by style
    -- background or background-color.
    if o.the_body /= null and then o.the_body.optional_style /= null then
      on.Select_back_color(o.the_body.optional_style.back_color);
    end if;
  end Select_main_background;

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
      -- NB: on MS Windows, font creation succeeds even if the font name is unknown to the system.
      --     So the strategy is to pass the whole font family in the descriptor's face name.
      --     The implementation will do something meaningful with the font family.
      on.Select_target_font(on.font_list.Last_Index);
      on.current_style.font:= on.font_list.Last_Element;
    else
      on.Select_target_font(index);
      on.current_style.font:= on.font_list.Element(index);
    end if;
  end Select_font;

  function Get_current_font(on : in Frame_plane'Class) return Font_descriptor is
  begin
    return on.current_style.font;
  end Get_current_font;

  procedure Select_fore_color(on: in out Frame_plane'Class; code: in Color_Code) is
  begin
    if code > parent_color then
      on.Select_target_fore_color(code);
      on.current_style.fore_color:= code;
    end if;
  end Select_fore_color;

  procedure Select_back_color(on: in out Frame_plane'Class; code: in Color_Code) is
  begin
    if code > parent_color then
      on.Select_target_back_color(code);
      on.current_style.back_color:= code;
    end if;
  end Select_back_color;

  procedure Advance_vertically(on: in out Frame_plane'Class; units: Natural) is
  begin
    on.curs.y:= on.curs.y + units;
  end Advance_vertically;

end Wasabee.Hypertext.Display;
