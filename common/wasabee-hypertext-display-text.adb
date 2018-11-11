-- with Wasabee.Util;                      use Wasabee.Util;

with Ada.Strings.Wide_Fixed;               use Ada.Strings.Wide_Fixed;
-- with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
-- with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

package body Wasabee.Hypertext.Display.Text is

  -- Internal procedures for text display

  procedure Carriage_Return(on: in out Frame_plane'Class) is
  begin
    on.curs.x:= on.indentation_space_width * on.indentation;
  end Carriage_Return;

  procedure Reset_text(on: in out Frame_plane'Class) is
  begin
    on.curs:= (0, 0);
    on.new_line_before_writing := False;
    on.skip_leading_blank             := True;
    on.show_next_line_break           := True;
    on.indentation:= 1;
    on.numbering:= 1;
    on.preformatted_text:= False;
    Carriage_Return(on); -- Get the default indentation (left margin)
  end Reset_text;

  procedure Show_text(on: in out Frame_plane'Class; t: UTF_16_String; mode: Draw_mode) is
    --
    chunk_begin, chunk_end : Natural; -- a chunk is either a series of blanks, or of non-blanks
    phrase_begin: Natural; -- we want to display the most chunks in one go
    phrase_curs: Point;
    --
    procedure Text_flush_before_chunk is
      pragma Inline(Text_flush_before_chunk);
    begin
      case mode is
        when all_but_images | full =>
          on.Text_at(phrase_curs, t(phrase_begin .. chunk_begin - 1));
          phrase_curs:= on.curs; -- on.curs has been pre-advanced
          phrase_begin:= chunk_begin;
        when invisible | images_only =>
          null;
      end case;
    end Text_flush_before_chunk;
    --
    w, h, lf_idx : Natural;
  begin
    if t'Length = 0 then
      return;
    end if;
    if on.new_line_before_writing then
      on.new_line_before_writing:= False;
      on.show_next_line_break:= True;
      New_Line(on, mode);
    end if;
    phrase_curs:= on.curs;
    if on.preformatted_text then
      lf_idx:= Index(t, (1 => Wide_Character'Val(10)));
      if lf_idx > 0 then
        Show_text(on, t(t'First .. lf_idx - 1), mode);  --  First line
        New_Line(on, mode);
        Show_text(on, t(lf_idx + 1 .. t'Last),  mode);  --  All other lines
        return;
      end if;
    end if;
    chunk_begin:= t'First;
    while on.skip_leading_blank and (not on.preformatted_text) and t(chunk_begin)=' ' loop
      chunk_begin:= chunk_begin + 1;
      if chunk_begin > t'Last then
        return; -- t was only a series of blanks to be skipped
      end if;
    end loop;
    on.skip_leading_blank:= False;
    phrase_begin:= chunk_begin;
    loop
      on.show_next_line_break:= True; -- there is some text
      chunk_end:= chunk_begin;
      if t(chunk_begin)=' ' then -- Blanks
        for i in chunk_begin+1..t'Last loop
          if t(i)=' ' then -- One more blank - happens only in PRE mode
            chunk_end:= i;
          else
            exit;
          end if;
        end loop;
        -- chunk_begin .. chunk_end is now the largest blank
        on.Text_size(t(chunk_begin .. chunk_end), w, h);
        if on.curs.x + w > on.area_width and not on.preformatted_text then
          New_Line(on, mode); -- Blank would go into the right margin, we prefer a line break
          Text_flush_before_chunk; -- we flush before the blank
          -- ^ unintuitive after New_Line call even if text is displayed "before",
          --   but Text_flush_before_chunk needs the new cursor position
          phrase_begin:= chunk_end + 1; -- blanks are not to be displayed later!
        else
          on.curs.x:= on.curs.x + w;
        end if;
      else -- Non-blanks
        for i in chunk_begin+1..t'Last loop
          if t(i)=' ' then
            exit;
          else
            chunk_end:= i;
          end if;
        end loop;
        -- chunk_begin .. chunk_end is now the largest non-blank
        on.Text_size(t(chunk_begin .. chunk_end), w, h);
        if on.curs.x > 0 and on.curs.x + w > on.area_width and not on.preformatted_text then
          -- ^ for x = 0 we give up auto line break if word is broader than the frame
          New_Line(on, mode);
          Text_flush_before_chunk; -- flush all but the last word
          -- ^ unintuitive after New_Line call even if text is displayed "before",
          --   but Text_flush_before_chunk needs the new cursor position
          on.skip_leading_blank:= False;
        -- ^ avoid next two words displayed without blank inbetween ( or: in between ;-) )
        end if;
        on.curs.x:= on.curs.x + w;
      end if;
      on.latest_text_height:= h;
      chunk_begin:= chunk_end + 1; -- prepare for next chunk
      exit when chunk_begin > t'Last;
    end loop;
    Text_flush_before_chunk;
  end Show_text;

  marker_symbol: constant array(Geom_list_styling) of Wide_Character:=
    (disc   => Wide_Character'Val(8226),  -- Small (bullet). Medium is: 9679
     circle => Wide_Character'Val(9702),  -- Small. Medium is: 9675
     square => Wide_Character'Val(9642)   -- Small. "Black Medium Square" is: 9724.
    );
  -- http://en.wikipedia.org/wiki/Geometric_Shapes (Unicode)

  procedure New_Line(
    on         : in out Frame_plane'Class;
    mode       :        Draw_mode;
    with_marker:        Boolean:= False
  )
  is
    w, h, x_mem : Natural;
  begin
    if on.show_next_line_break then
      on.Text_size("A", w, h);
      Advance_vertically(on, h);
    end if;
    if on.show_next_line_break or with_marker then
      Carriage_Return(on);
      case on.current_style.list_style is
        when none =>
          null;
        when decimal =>
          declare
            marker: constant Wide_String:= Positive'Wide_Image(on.numbering) & ". ";
            -- !! marker is hardcoded
          begin
            on.skip_leading_blank:= True;
            if with_marker then
              x_mem:= on.curs.x;
              Show_text(on, marker, mode);
              on.curs.x:= x_mem;
              on.numbering:= on.numbering + 1;
            end if;
            on.curs.x:= on.curs.x + on.marker_width; -- align on marker
          end;
        when Geom_list_styling =>
          declare
            marker: constant Wide_String:= marker_symbol(on.current_style.list_style) & ' ';
          begin
            if with_marker then
              Show_text(on, marker, mode);
            else
              Show_text(on, marker, invisible); -- Normal New_Line but aligned on marker
            end if;
          end;
        when initial | inherit =>
          raise Program_Error with "initial | inherit in list_style on display";
      end case;
    end if;
    on.skip_leading_blank:= True;
    on.show_next_line_break:= False;
  end New_Line;

end Wasabee.Hypertext.Display.Text;
