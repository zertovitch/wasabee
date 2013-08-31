with Wasabee.Util;                      use Wasabee.Util;
with Ada.Calendar ;

with Interfaces ; use Interfaces ;
with Interfaces.C ; use Interfaces.C ;

-- with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       --use Ada.Text_IO;
                                        -- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;
with Ada.Exceptions ; use Ada.Exceptions ;


package body Wasabee.Hypertext.Display is

   default_font: constant Font_descriptor:=
     (face          => U("Calibri"),
      size          => 22, -- !! pixel or pt
      modifier      => (others => False)
     );

   default_heading_pct_size: constant array(h1..h6) of Positive:= (200, 150, 113, 100, 80, 62);


   procedure Draw (
                   on   : in out Frame_plane'Class;
                   o    : in out HT_object;
                   mode :        Draw_mode
                  )
   is
      curs: Point;
      skip_leading_blank: Boolean;
      show_next_line_break: Boolean;
      indentation: Natural;
      indentation_space_width: Positive;
      type List_marker is (none, numbered, bullets);
      current_marker: List_marker;
      tag_to_marker: constant array(ul..ol) of List_marker:= (ol => numbered, ul => bullets);
      numbering: Positive;
      area_width, area_height: Natural;
      latest_text_height: Natural;

      procedure Carriage_Return is
      begin
         curs.x:= indentation_space_width * indentation;
      end Carriage_Return;

      procedure Reset_text is
      begin
         on.modifier_level:= (others => 0);
         curs:= (0, 0);
         skip_leading_blank:= True;
         show_next_line_break:= True;
         indentation:= 1;
         current_marker:= none;
         numbering:= 1;
         Carriage_Return; -- Get the default indentation (left margin)
      end Reset_text;

      procedure New_Line(with_marker: Boolean:= False);

      procedure Show_text(t: UTF_16_String) is
         --
         procedure Text_at_cursor(t: UTF_16_String) is
            pragma Inline(Text_at_cursor);
         begin
            case mode is
               when all_but_images | full =>
                  on.Text_XY(curs.x, curs.y, t);
               when invisible | images_only =>
                  null;
            end case;
         end Text_at_cursor;
         --
         w, h, j : Natural;
         blank_first: Boolean;
      begin
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
                  if curs.x + w > area_width then -- !! not in PRE
                     New_Line;
                  else
                     if on.Get_current_font.modifier(underlined) then
                        Text_at_cursor(t(t'First .. j));
                     end if;
                     curs.x:= curs.x + w;
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
                  if curs.x > 0 and then curs.x + w > area_width then
                     -- ^ we give up auto line break if word is larger than the frame
                     New_Line;
                     skip_leading_blank:= False;
                     -- ^ avoid next two words displayed without blank inbetween
                  end if;
                  Text_at_cursor(t(t'First .. j));
                  curs.x:= curs.x + w;
               end if;
               latest_text_height:= h;
               Show_Text(t(j+1..t'Last)); -- show the rest
            end if;
         end if;
      end Show_text;

      procedure Advance_vertically(units: Natural) is
         pragma Inline(Advance_vertically);
      begin
         curs.y:= curs.y + units;
         -- !! extend canvas here if needed
      end Advance_vertically;

      procedure New_Line(with_marker: Boolean:= False) is
         w, h : Natural;
      begin
         if show_next_line_break then
            on.Text_size("A", w, h);
            Advance_vertically(h);
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
                        on.Text_size(marker, w, h);
                        curs.x:= curs.x + w;
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
                        on.Text_size(marker, w, h);
                        curs.x:= curs.x + w;
                     end if;
                  end;
            end case;
         end if;
         skip_leading_blank:= True;
         show_next_line_break:= False;
      end New_Line;

      procedure Draw_body(bn: p_Body_Node; level: Natural:= 0) is -- Scary name ;-) !
         mem_font: Font_descriptor;
         --
         procedure Draw_children is
            p: p_Body_node;
         begin
            Draw_body(bn.first_child, level + 1);
            p:= bn.first_child;
            while p /= null loop -- Extend the bounding box with all children
               bn.bounding_box:= Max(bn.bounding_box, p.bounding_box);
               p:= p.next;
            end loop;
         end Draw_children;
         --
         procedure Draw_children_with_font_modification(
                                                        change_form   : Boolean;
                                                        fm            : Font_modifier := bold;
                                                        scale_percents: Positive      := 100
                                                       )
         is
            new_font: Font_descriptor:= on.Get_current_font;
            -- ^ we clone the current font before modifying it...
         begin
            mem_font:= new_font; -- remember before modification
            if change_form then
               on.modifier_level(fm):= on.modifier_level(fm) + 1;
               for any_fm in Font_modifier loop
                  new_font.modifier(any_fm):= on.modifier_level(any_fm) > 0;
               end loop;
            end if;
            new_font.size:= (new_font.size * scale_percents) / 100;
            on.Select_font(new_font);
            Draw_children;
            if change_form then
               on.modifier_level(fm):= on.modifier_level(fm) - 1;
            end if;
            on.Select_font(mem_font); -- restore font at node's start
         end Draw_children_with_font_modification;
         --
      begin
         if bn = null then
            return;
         end if;
         -- !! tag default style here
         -- !! tag's style attribute - style modifier here e.g. style="color:blue"
         bn.bounding_box:= (curs, curs); -- a priori, a pixel... will be extended hereafter.
         case bn.kind is
            when Img =>
               New_Line;
               declare
                  P1 : Point := (Curs.X,Curs.Y+10) ;
                  P2 : Point := (Curs.X + Bn.Width,Curs.Y + Bn.Height + 10);
                  B : Box := (P1, P2);
                  posX , posY : Natural ;
                  Color : Color_Code := 16#FF0000# ;
               begin
                  On.Rectangle(B);
                  Advance_vertically(Bn.Height+10);
                  On.Flush ;
                  New_Line ;
               exception
                  when E: others =>
                     Ada.Text_IO.Put_Line (Exception_Name (E));
                     Ada.Text_IO.Put(Exception_Message (E));
               end ;
               New_Line;
            when text       =>
               latest_text_height:= 0;
               bn.bounding_box.p1:= curs;
               Show_text(S(bn.content));
               bn.bounding_box.p2:= (curs.x, curs.y + latest_text_height);
               -- ^ we'll need a multi-rectangle animal within text, too !!
            when a | u | ins =>
               Draw_children_with_font_modification(True, underlined);
            when b | strong =>
               Draw_children_with_font_modification(True, bold);
            when i | em | var | dfn | cite =>
               Draw_children_with_font_modification(True, italic);
            when big =>
               Draw_children_with_font_modification(False, scale_percents => 120);
            when small | sup | sub => -- !! missing alignment
               Draw_children_with_font_modification(False, scale_percents => 80);
            when strike | del | s =>
               Draw_children_with_font_modification(True, strikethrough);
            when code | samp | kbd | tt =>
               mem_font:= on.Get_current_font;
               declare
                  monospace_font: Font_descriptor:= mem_font;
               begin
                  monospace_font.face:= U("Courier New"); -- !! hardcoded
                  monospace_font.size:= mem_font.size - 3; -- !! hardcoded
                  on.Select_font(monospace_font);
                  Draw_children;
               end;
               on.Select_font(mem_font); -- restore font at node's start
            when abbr | acronym =>
               Draw_children; -- !! show label with attribute title
            when details | summary =>
               Draw_children;
            when font =>
               mem_font:= on.Get_current_font;
               declare
                  modified_font: Font_descriptor:= mem_font;
                  mem_color: constant Color_code:= on.current_style.text_color;
               begin
                  if bn.face /= "" then
                     modified_font.face:= bn.face;
                  end if;
                  on.Select_font(modified_font);
                  on.Select_text_color(bn.color);
                  Draw_children;
                  on.Select_text_color(mem_color);
               end;
               on.Select_font(mem_font); -- restore font at node's start
            when address =>
               New_Line;
               Draw_children_with_font_modification(True, italic);
               New_Line;
            when q => -- quote
               Show_text(""""); -- !! hardcoded
               Draw_children;
               Show_text(""""); -- !! hardcoded
            when h1 .. h6   =>
               New_Line;
               mem_font:= on.Get_current_font;
               declare
                  font: Font_descriptor:= mem_font;
               begin
                  font.size:= (font.size * default_heading_pct_size(bn.kind)) / 100;
                  on.Select_font(font);
                  Draw_children;
                  New_Line;
               end;
               on.Select_font(mem_font); -- restore font at node's start
            when br   =>
               New_Line;
            when hr   =>
               New_Line;
               if mode /= invisible then
                  on.Rectangle((curs, (area_width, curs.y + bn.hr_height)));
               end if;
               Advance_vertically(bn.hr_height);
            when p | div | article | aside | dt | nav | figcaption =>
               -- * W3 Note: Browsers automatically add some space (margin) before and after each <p>
               --   element. The margins can be modified with CSS (with the margin properties).
               -- * W3 Note: By default, browsers always place a line break before and after
               --   the <div> element. However, this can be changed with CSS.
               New_Line;
               Draw_children;
               New_Line;
            when span | dl =>
               -- * W3 Note: The <span> tag provides no visual change by itself.
               Draw_children;
            when blockquote | dd | figure =>
               declare
                  mem_indent: constant Natural:= indentation;
               begin
                  indentation:= indentation + 2;
                  New_Line;
                  Carriage_Return;
                  -- ^ force new indentation in case New_Line was skipped (e.g. DD after DT)
                  Draw_children;
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
                  Draw_children;
                  indentation:= mem_indent;
                  current_marker:= mem_marker;
                  numbering:= mem_numbering;
               end;
               New_Line;
            when li =>
               New_Line(with_marker => True);
               Draw_children;
         end case;
         -- if mode /= invisible then
         --   on.Rectangle(bn.bounding_box);
         --   -- Show the bounding box (for debugging purposes)
         -- end if;
         Draw_body(bn.next, level);
      end Draw_body;

      dummy: Natural;
      p: p_Body_node;
   begin
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
      o.main_bounding_box:= (curs, curs); -- one-pixel-page so far...
                                          --
      Draw_body(o.the_body);
      --
      p:= o.the_body;
      while p /= null loop -- Extend the bounding box with all children
         o.main_bounding_box:= Max(o.main_bounding_box, p.bounding_box);
         p:= p.next;
      end loop;
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

   procedure Select_text_color(on: in out Frame_plane'Class; code: in Color_Code) is
   begin
      on.Select_target_text_color(code);
      on.current_style.text_color:= code;
   end Select_text_color;

end Wasabee.Hypertext.Display;
