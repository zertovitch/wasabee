with Wasabee.CSS;                       use Wasabee.CSS;
with Wasabee.Request;                   use Wasabee.Request;
with Wasabee.URL;                       use Wasabee.URL;
with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.Entities;                  use Wasabee.Entities;
with Wasabee.Hypertext.Styles;          use Wasabee.Hypertext.Styles;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
-- with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Integer_Wide_Text_IO;          use Ada.Integer_Wide_Text_IO;

package body Wasabee.Hypertext.Parsing is

  -- List of attributes used in parsing HTML element.
  -- This is an absolutely dumb alphabetical list; its purpose
  -- is only to save tedious string comparisons.

  type HTML_attribute is (
    bgcolor,
    charset, class, color, content,
    face,
    height, href,
    id,
    name,
    rel,
    src, style,
    tvpe,
    width,
    zzz_unknown_attribute
  );

  function Get_attribute(s: String) return HTML_attribute is
  begin
    return HTML_attribute'Value(s);
  exception
    when Constraint_Error =>
      if To_Upper(s) = "TYPE" then  --  Ada keyword
        return tvpe;
      else
        return zzz_unknown_attribute;
      end if;
  end Get_attribute;

  type Location_type is (nowhere, in_head, in_body);

  function Identify_tag(s: String) return HTML_kind is
    kind: HTML_kind;
  begin
    begin
      kind:= HTML_kind'Value(s);
    exception
      when Constraint_Error =>
        if s = "!--" then
          kind:= comment;
        elsif To_Upper(s) = "BODY" then  --  Ada keyword
          kind:= b0dy;
        else
          kind:= unknown_tag;
        end if;
    end;
    if Verbosity > 0 then
      Put("Tag: [" & s & ',' & HTML_kind'Image(kind) & ']');
    end if;
    return kind;
  end Identify_tag;

  Alpha_attrib_set: constant array(Character) of Boolean :=
    ('a'..'z' | 'A'..'Z' | '-' => True, others => False);
  --  Example of attribute: data-file-width="627"

  Alphanum_tag_set: constant array(Character) of Boolean :=
    ( 'a'..'z' |
      'A'..'Z' |
      '0'..'9' |
      '!' | '-' |  -- This is for spotting comments: "<!--"
      ':'          -- This is only for <g:plusone> (Google's "+1" button) !
     => True,
     others => False);

  Numeric_narrow_set: constant array(Character) of Boolean :=
    ( '0'..'9' | '-' | '.' => True, others => False);

  Bracketing_tag_set: constant array(HTML_kind) of Boolean :=
    ( Univ_Bracketing_tag |
      Head_bracketing_tag |
      Body_bracketing_tag
     => True,
     others => False);

  function Size_argument(s: String) return Integer is
  begin
    if To_Lower(s) = "auto" then
      return auto;
    end if;
    for i in s'Range loop
      if not Numeric_narrow_set(s(i)) then
        return Integer'Value(s(s'First..i-1));
      end if;
    end loop;
    return Integer'Value(s);
  exception
    when Constraint_Error =>
      raise Constraint_Error with "Size_argument fails on: " & s;
  end Size_argument;

  -- Load an HT object from a string with HTML code (complete or not). HTML is parsed directly.
  procedure Load_frame(ho: in out HT_object; from: String) is

    type p_p_Body_node is access all p_Body_node;
    current_body_pointer: p_p_Body_node:= ho.the_body'Access;
    preformatted_level: Natural:= 0;

    curs: Integer:= from'First;

    function Seek_tag_name return String is
      curs_0: constant Integer:= curs;
    begin
      while
        curs <= from'Last and then
        Alphanum_tag_set(from(curs)) and then
        from(curs_0..curs-1) /= "!--"  -- an HTML comment can be followed by letters without spaces.
      loop
        curs:= curs + 1;
      end loop;
      return from(curs_0..curs-1);
    end Seek_tag_name;

    function Seek_attribute_name return String is
      curs_0: constant Integer:= curs;
    begin
      while curs <= from'Last and then Alpha_attrib_set(from(curs)) loop
        curs:= curs + 1;
      end loop;
      return from(curs_0..curs-1);
    end Seek_attribute_name;

    function Seek_attribute_value return String is
      curs_0:  Integer;
    begin
      if curs > from'Last or else from(curs) /= '=' then
        return "";
      end if;
      curs:= curs + 1;
      if curs > from'Last then
        return "";
      end if;
      if from(curs) = '"' then     --  bracketed with "..." (the correct way)
        curs:= curs + 1;
        curs_0:= curs;
        while curs <= from'Last and then from(curs) /= '"' loop
          curs:= curs + 1;
        end loop;
        return from(curs_0..curs-1);
      elsif from(curs) = ''' then  --  bracketed with '...' (wrong but it happens...)
        curs:= curs + 1;
        curs_0:= curs;
        while curs <= from'Last and then from(curs) /= ''' loop
          curs:= curs + 1;
        end loop;
        return from(curs_0..curs-1);
      else
        curs_0:= curs;
        while curs <= from'Last and then not (from(curs) = '>' or from(curs) = ' ') loop
          curs:= curs + 1;
        end loop;
        curs:= curs - 1; -- Go back to last attribute character
        return from(curs_0..curs);
      end if;
    end Seek_attribute_value;

    truncated, syntax: exception;
    location: Location_type:= nowhere;
    --
    -- Process a tag pair ( <TAG>...</TAG> ) or a singleton tag ( <TAG> or <TAG /> ).
    -- Conventions:
    --   Enter : curs is on the first character after tag name
    --   Exit  : curs is on the '>' of the closing tag
    procedure Process_tag(
      tag_name : String;
      level    : Natural:= 0
    )
    is
      kind: HTML_kind;
      new_node : p_Body_node:= null;
      --
      --
      --   Enter : curs is on the first character after tag name
      --   Exit  : curs is on the '>' of the opening or singleton tag
      procedure Process_tag_attributes is
        current_REL, current_TYPE, current_URL: Unbounded_String;
        data   : Unbounded_String;
        procedure Create_local_style_if_null is
        begin
          if new_node /= null and then new_node.optional_style = null then
            new_node.optional_style:= new Local_Style;
          end if;
        end Create_local_style_if_null;
        --
        procedure Process_body_attribute(attribute: HTML_attribute; attribute_value: String) is
          the_URL: Unbounded_String;
        begin
          case attribute is
            when id =>
              new_node.id:= U(attribute_value);
            when class =>
              new_node.class:= U(Simplify_Blanks(attribute_value));
            when href =>
              the_URL:= U(Simplify_URL(Build_URL(ho, attribute_value)));
              case kind is
                when a =>
                  new_node.URL:= the_URL;
                when others =>
                  null;
              end case;
            when name => -- name is how anchor was defined up to HTML 4.01
              if kind = a then
                new_node.id:= U(attribute_value);
              end if;
            when rel =>
              current_REL:= U(attribute_value);
            when src =>
              if kind = img then
                new_node.src_URL:= U(Simplify_URL(Build_URL(ho, attribute_value)));
              end if;
            when style =>
              Create_local_style_if_null;
              Parse_immediate_style(new_node.optional_style.all, attribute_value);
            when tvpe =>
              current_TYPE:=  U(attribute_value);
              if kind in ul..ol then  --  Pre-CSS styling of lists. Example: <ul type="square">
                Create_local_style_if_null;
                Parse_immediate_style(new_node.optional_style.all, "list-style:" & attribute_value);
              end if;
            when width =>
              if kind = img then
                Create_local_style_if_null;
                new_node.optional_style.dimensions.x  := Size_argument(attribute_value);
              end if;
            when height =>
              if kind = img then
                Create_local_style_if_null;
                new_node.optional_style.dimensions.y := Size_argument(attribute_value);
              end if;
            when face =>
              if kind = font then
                new_node.optional_style.font.family:= U(attribute_value);
              end if;
            when color => --  Usually for a <FONT>
              Create_local_style_if_null;
              new_node.optional_style.fore_color:= Parse_color(attribute_value);
            when bgcolor =>  --  Usually for a <BODY> or <TABLE>
              Create_local_style_if_null;
              new_node.optional_style.back_color:= Parse_color(attribute_value);
            when others =>
              null;
          end case;
        end Process_body_attribute;
        --
        procedure Process_non_body_attribute(attribute: HTML_attribute; attribute_value: String) is
          the_URL: Unbounded_String;
        begin
          case attribute is
            when charset =>
              if kind = meta then
                Parse_encoding("charset=" & attribute_value, ho.encoding);
                --  <meta charset="UTF-8" />  (New in HTML5, used by Wikipedia)
              end if;
            when content =>
              if kind = meta then
                Parse_encoding(attribute_value, ho.encoding);
                --  <meta content="text/html; charset=utf-8" />
              end if;
            when href =>
              the_URL:= U(Simplify_URL(Build_URL(ho, attribute_value)));
              case kind is
                when base =>
                  ho.base_URL:= U(attribute_value);
                when link =>
                  current_URL:= the_URL;
                when others =>
                  null;
              end case;
            when rel =>
              current_REL:= U(attribute_value);
            when tvpe =>
              current_TYPE:= U(attribute_value);
            when others =>
              null;
           end case;
        end Process_non_body_attribute;
        --
      begin  --  Process_tag_attributes
        if kind = font or kind = img then
          Create_local_style_if_null;
        end if;
        loop
          if curs > from'Last then
            raise truncated;
          end if;
          exit when from(curs)='>';
          if Alpha_attrib_set(from(curs)) then
            declare
              attribute_name: constant String:= Seek_attribute_name;
              attribute: constant HTML_attribute:= Get_attribute(attribute_name);
              attribute_value: constant String:= Seek_attribute_value;
            begin
              case location is
                when in_body =>
                  if new_node /= null then
                    Process_body_attribute(attribute, attribute_value);
                  end if;
                when others =>
                  Process_non_body_attribute(attribute, attribute_value);
              end case;
            end;
          end if;
          curs:= curs + 1;
        end loop;
        --  Trigger loading of external resources depending on attributes
        case kind is
          when link =>
            if current_REL = "stylesheet" and then current_TYPE = "text/css" then
              -- !! TEMP - Blocking URL request (but we are happy to have the styles complete)
              -- !! We should go through Wasabee.Caches
              Retrieve_from_URL(S(current_URL), data);
              if Verbosity > 0 then
                Put_Line("Parsing external CSS: " & S(current_URL));
              end if;
              Parse_Information(ho.style_map, data);
            end if;
          when img =>
            -- !! TEMP - Blocking URL request
            -- !! We should go through Wasabee.Caches, queue the request
            --    and let the cache daemon load the image.
            if new_node /= null then
              Get_Full_Image_Blocking(S(new_node.src_URL), new_node.bitmap);
            end if;
          when others =>
            null;
        end case;
      end Process_tag_attributes;
      --
      -- Can be no child at all. We stop when bumping into a closing tag.
      --   Enter : curs is on the '>' of the opening tag
      --   Exit  : curs is on the '<' of any closing tag (i.e. we have a beginning of "</")
      procedure Process_children(take_text: Boolean:= True) is
        curs_0, curs_1: Integer;
        new_text_node : p_Body_node;
        is_closing_tag: Boolean;
      begin
        curs:= curs + 1;
        loop
          if curs > from'Last then
            raise truncated;
          end if;
          curs_0:= curs;
          -- Process any text here:
          while curs <= from'Last and then from(curs) /= '<' loop
            curs:= curs + 1;
          end loop;
          if curs > curs_0 then -- OK, we have a bit of text
            declare
              ze_text: String renames from(curs_0..curs-1);
            begin
              case location is
                when in_head =>
                  case kind is
                    when title =>
                      ho.title:= U(Translate_character_entities(To_UTF_16(ze_text, ho.encoding)));
                    when style =>
                      Parse_Information(ho.style_map, U(ze_text));
                    when others =>
                      null;
                  end case;
                when in_body =>
                  if take_text then
                    -- Put('['& ze_text & ']');
                    new_text_node:= new Body_Node(body_text);
                    new_text_node.content:=
                      U(Filter_blanks(
                          Translate_character_entities(To_UTF_16(ze_text, ho.encoding)),
                          fully => preformatted_level = 0)
                       );
                    current_body_pointer.all:= new_text_node;
                    current_body_pointer:= new_text_node.next'Access; -- ready for next sibling
                  end if;
                when nowhere =>
                  null; -- text neither in HEAD nor BODY region -> trash
              end case;
            end;
          end if;
          --  We have the end of the stream, or the '<' of a tag at this point.
          if curs >= from'Last then
            raise truncated;
          end if;
          is_closing_tag:= from(curs..curs+1)="</";
          if is_closing_tag then
            curs_1:= curs;
            curs:= curs + 2;
          else
            curs:= curs + 1;
          end if;
          declare
            new_tag_name: constant String:= Seek_tag_name;
          begin
            if new_tag_name'Length /= 0 then
              if is_closing_tag then
                if Identify_tag(new_tag_name) in Body_singleton_tag then
                  null;  --  We continue: ill-written singleton tag, e.g. </br> (13-Jan-2016)
                else  --  Start of a closing tag -> end of children list -> we leave the loop
                  curs:= curs_1;
                  exit;
                end if;
              end if;
              --  At this point we have a new sibling: an opening or singleton tag.
              --  The nice recursion happens HERE :
              Process_tag(new_tag_name, level + 1);  --  After Process_tag, we are on a '>'.
              curs:= curs + 1;  --  '>' is skipped now.
            else
              raise syntax; -- No tag ID at all - we must have some UFO...
            end if;
          end;
        end loop;
      end Process_children;
      --
      procedure Process_body_tag is
      begin
        new_node:= new Body_Node(kind);
        current_body_pointer.all:= new_node;
        Process_tag_attributes;
        case kind is
          when pre =>
            preformatted_level:= preformatted_level + 1;
          when script =>
            new_node.optional_style:= new Local_Style; --  !! this doesn't have any effect
            new_node.optional_style.hiding_level:= 1;  --  !! fix: cf. take_text => kind /= script
          when others =>
            null;
        end case;
        if kind in Body_bracketing_tag then
          current_body_pointer:= new_node.first_child'Access; -- ready for first child
          Process_children;
        end if;
        if kind = pre then
          preformatted_level:= preformatted_level - 1;
        end if;
        --
        current_body_pointer:= new_node.next'Access; -- ready for next sibling
      end Process_body_tag;
      --
      procedure Process_closing_tag is
        curs_0: Integer;
      begin
        -- We are on the '<' of the "</TAG>"
        curs:= curs + 2;
        curs_0:= curs;
        declare
          new_tag_name: constant String:= Seek_tag_name;
        begin
          if Verbosity > 0 then
            Put("Closing ");
          end if;
          if Identify_tag(new_tag_name) = kind then -- Closing tag name is correct
            loop
              exit when curs > from'Last or else from(curs) = '>';
              curs:= curs + 1;
            end loop;
          else
            if Verbosity > 0 then
              Put(" (Wrong closing tag (" & new_tag_name & "), should be " & kind'img & ") ");
            end if;
            --  Houston, we have a problem. Typically: "<li>" without "</li>".
            --  Example: "<li>bla</ul>". NB: "<li>bla 1<li>bla 2" is processed by Rectify_tree later.
            curs:= curs_0 - 3;
            --  We go back to the 'x' of "x</TAG>".
            --  If you look where Process_tag is called, it must be a good idea...
            --  Hope: the closing tag is closing one of the open tags in the recursion.
          end if;
          if Verbosity > 0 then
            Put_Line(" @ curs =" & curs'img);
          end if;
        end;
      end Process_closing_tag;
      --
    begin -- Process_tag
      kind:= Identify_tag(tag_name);
      if Verbosity > 0 then
        Put_Line(" @ curs =" & curs'img & " " & kind'img & " " & level'img);
      end if;
      if kind = unknown_tag then
        Process_tag_attributes;
        Process_children;
        Process_closing_tag;
        return;
      elsif kind = comment then
        loop
          if curs > from'Last-2 then
            raise truncated with "End of HTML string reached within a comment";
          end if;
          exit when from(curs..curs+2) = "-->";  --  End of comment
          curs:= curs + 1;
        end loop;
        curs:= curs + 2; -- Skip to the '>' of the "-->".
        return;
      elsif kind = script then
        declare
          target : constant String := "</script>";
        begin
          loop
            if curs > from'Last - target'length then
              raise truncated with "End of HTML string reached within a script";
            end if;
            exit when from(curs .. curs + target'length - 1) = target;
            curs:= curs + 1;
          end loop;
          curs := curs + target'length - 1; -- Skip to the '>' of the closing tag.
          return;
        end;
      end if;
      case level is
        when 0 =>      -- nesting level 0; we know the tag is HTML
          Process_tag_attributes;
          Process_children;
        when 1 =>      -- nesting level 1
          case kind is
            when head =>
              location:= in_head;
              if Verbosity > 0 then
                Put_Line("*** HTML Region is now HEAD");
              end if;
              Process_tag_attributes;
              Process_children;
            when b0dy =>
              location:= in_body;
              if Verbosity > 0 then
                Put_Line("*** HTML Region is now BODY");
              end if;
              Process_body_tag;
            when others =>
              location:= nowhere;
              if Verbosity > 0 then
                Put_Line("*** HTML Region is now NOWHERE");
              end if;
              Process_tag_attributes;
              Process_children;
          end case;
        when others => -- nesting level 2,3,4,...
          case location is
            when in_head =>
              Process_tag_attributes;
              if Bracketing_tag_set(kind) then
                Process_children; -- e.g., text of title, style.
              end if;
            when in_body =>
              if kind in Body_kind then
                Process_body_tag;
              else  --  No body-specific tag
                Process_tag_attributes;
                if Bracketing_tag_set(kind) then
                  Process_children(take_text => kind /= script);
                end if;
              end if;
            when nowhere =>
              null;
          end case;
      end case;
      if Bracketing_tag_set(kind) then
        Process_closing_tag;
      end if;
    end Process_tag;
    --
    html_str: String(1..6);
  begin
    Initialize_styles(ho);
    Delete_body_tree(ho);
    -- Skip until finding "<HTML"
    loop
      if curs + 5 > from'Last then
        return; -- No HTML found in this string.
      end if;
      if from(curs)='<' then
        if from(curs+1..curs+3)="!--" then  --  Comment
          loop
            curs:= curs + 1;
            if curs + 5 > from'Last then
              return; -- No HTML found in this string.
            end if;
            exit when from(curs..curs+2) = "-->";  --  End of comment
          end loop;
        else
          html_str:= To_Upper(from(curs .. curs + 5));
          exit when
            html_str = "<HTML " or else
            html_str = "<HTML" & ASCII.LF or else
            html_str = "<HTML" & ASCII.CR or else
            html_str = "<HTML>";
        end if;
      end if;
      curs:= curs + 1;
    end loop;
    curs:= curs + 5;
    Process_tag("html");
  exception
    when truncated | syntax => null; -- just an incomplete page
  end Load_frame;

end Wasabee.Hypertext.Parsing;
