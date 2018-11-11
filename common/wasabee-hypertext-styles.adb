with Wasabee.CSS;                       use Wasabee.CSS;
with Wasabee.Hypertext.Parsing;
with Wasabee.Util;                      use Wasabee.Util;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Wasabee.Hypertext.Styles is

  --  http://www.w3schools.com/cssref/

  type Property is
    ( background, background_color,
      border_color, border_spacing, border_style, border_width,
      color,
      display,
      font_family, font_size, font_style, font_weight,
      height,
      list_style, list_style_type, list_style_position, list_style_image,
      text_decoration,
      width,
      zzz_unknown_property
    );

  function To_Property is new Wasabee.Util.To_Enum_func(Property, zzz_unknown_property);

  type Value is   --  named values of various CSS properties
    ( none,
      underline, line_through,                                    -- for text-decoration
      larger, smaller,                                            -- for font-size, relative
      xx_small, x_small, small, medium, large, x_large, xx_large, -- for font-size, absolute
      zzz_unknown_value);

  function To_Value is new Wasabee.Util.To_Enum_func(Value, zzz_unknown_value);

  -------------------------------------------------------------------------------
  --  Internal Set_property - property as enumerated type Property, val /= ""  --
  -------------------------------------------------------------------------------

  --  arg is the the argument position of the value.
  --  E.g. for key="list-style", "none" means no marker if arg=1, and no image if arg=3.

  procedure Set_property(sty: in out Local_Style; prop: Property; val: String; arg: Positive) is
    l_val: constant String:= To_Lower(val);
    e_val: constant Value:= To_Value(val);
  begin
    case prop is
      when background =>
        -- !! Can be several properties: background: #00ff00 url('smiley.gif') no-repeat fixed center;
        -- background-color, background-image, background-repeat, background-attachment, and background-position
        case arg is
          when 1 => Set_property(sty, background_color,    val, 1);
          when others => null; -- !!
        end case;
      when background_color =>
        sty.back_color:= Parse_color(val);
      when border_color =>
        sty.border_color:= Parse_color(val);
      when border_spacing =>
        sty.border_spacing:= (2, 2); -- !! Parse 10px [50px]
      when border_style =>
        sty.border_style:= Border_styling'Value(val);
      when border_width =>
        sty.border_width:= 3; -- !! cheat, default value here. Parse 3px
      when color =>
        sty.fore_color:= Parse_color(val);
      when display =>
        if e_val = none then
          sty.hiding_level:= 1;
        end if;
      when font_family =>
        sty.font.family:= To_Unbounded_String(val);
      when font_size =>
        -- http://www.w3schools.com/cssref/pr_font_font-size.asp
        if val(val'Last) = '%' then
          sty.font.size_scale:= Integer'Value(val(val'First..val'Last-1));
        elsif val'Length >= 2 and then l_val(l_val'Last-1..l_val'Last)="em" then
          sty.font.size:= Integer(100.0 * Float'Value(val(val'First..val'Last-2)));
        else
          case e_val is
            -- Relative sizes (size is relative to parent)
            when smaller =>    sty.font.size_scale:= 83;
            when larger =>     sty.font.size_scale:= 120;
            -- Absolute sizes (size is relative to the default font size, like 1em)
            when xx_small =>   sty.font.size :=  58;
            when x_small  =>   sty.font.size :=  69;
            when small    =>   sty.font.size :=  83;
            when medium   =>   sty.font.size := 100;
            when large    =>   sty.font.size := 120;
            when x_large  =>   sty.font.size := 144;
            when xx_large =>   sty.font.size := 173;
            when others   =>   null;
          end case;
        end if;
      when font_style =>
        if l_val = "italic" then
          sty.font.modifier(italic):= 1;
        end if;
      when font_weight =>
        if l_val = "bold" then
          sty.font.modifier(bold):= 1;
        end if;
      when list_style =>  -- list-style: list-style-type list-style-position list-style-image
        case arg is
          when 1 => Set_property(sty, list_style_type,     val, 1);
          when 2 => Set_property(sty, list_style_position, val, 1);
          when 3 => Set_property(sty, list_style_image,    val, 1);
          when others => null;
        end case;
      when list_style_type =>
        sty.list_style:= List_styling'Value(val);
      when list_style_position =>
        null; -- !! inside|outside|initial|inherit
      when list_style_image =>
        null; -- !! none, url, initial, inherit
      when text_decoration =>
        case e_val is
          when underline    =>   sty.font.modifier(underlined):= 1;
          when line_through =>   sty.font.modifier(strikethrough):= 1;
          when others       =>   null;
        end case;
      when width =>
        sty.dimensions.x:= Wasabee.Hypertext.Parsing.Size_argument(val);
      when height =>
        sty.dimensions.y:= Wasabee.Hypertext.Parsing.Size_argument(val);
      when zzz_unknown_property =>
        null;
    end case;
  exception
    when Constraint_Error =>  --  Probably an invalid value on 'Value.
      null;
  end Set_property;
  
  procedure Set_property(sty: in out Local_Style; key, vals: String) is
  begin
    if vals = "" then
      return;
    end if;
    Set_property(sty, To_Property(key), vals, 1); -- !! to do: separate the arguments !!
  end Set_property;

  Blank_set: constant array(Character) of Boolean:=
   (' ' | Character'Val(9) | Character'Val(10) | Character'Val(13) => True,
    others => False);

  procedure Parse_immediate_style(sty: in out Local_Style; v: String) is
    i: Integer:= v'First;
    i0: Integer;
    procedure Skip_blanks is
    begin
      while i <= v'Last and then Blank_set(v(i)) loop
        i:= i + 1;
      end loop;
    end;
  begin
    loop
      exit when i > v'Last;
      Skip_blanks;
      exit when i > v'Last;
      i0:= i;
      while i <= v'Last and then v(i) /= ':' loop
        i:= i + 1;
      end loop;
      exit when i > v'Last;
      declare
        key: constant String := To_Upper(v(i0..i-1));
      begin
        i:= i + 1; -- skip ':'
        i0:= i;
        while i <= v'Last and then v(i) /= ';' loop
          i:= i + 1;
        end loop;
        declare
          value: constant String:= Simplify_Blanks(v(i0..i-1));
        begin
          if Verbosity > 0 then 
            Put_line("--> immediate, local style '" & key & "' = '" & value & ''');
          end if;
          Set_property(sty, key, value);
        end;
        i:= i + 1; -- skip ';'
      end;
    end loop;
  end Parse_immediate_style;

  procedure Dispose is new Ada.Unchecked_Deallocation(Local_Style, p_Local_Style);

  procedure Apply_styles(
    node         : p_Body_Node;
    css_map      : CSS.CSS_Dictionary.Map;
    parent_style : Local_Style
  )
  is
    new_style: p_Local_Style:= null;
    --
    procedure Clone_parent_style_if_null is
    begin
      if new_style = null then
        new_style:= new Local_Style'(parent_style);
        new_style.dimensions:= (unspecified, unspecified);  --  ...no inherited dimensions!
      end if;
    end Clone_parent_style_if_null;
    --
    --  Apply properties for a simple selector
    --  http://www.w3.org/TR/css3-selectors/
    --
    procedure Apply_simple_CSS_selector(tag_s : String) is
    begin
      if Exists_CSS_Element(css_map, tag_s) then
        declare
          PM : constant CSS_Properties_Map_Ptr:=
            CSS_Dictionary.Element(css_map, To_Unbounded_String(tag_s));
          use CSS_Properties;
          prop_cursor: Cursor:= PM.First;
        begin
          Clone_parent_style_if_null;
          if Verbosity > 0 then     
            put_line("Apply_simple_CSS_selector " & tag_s);   
          end if;
          loop
            if Verbosity > 0 then 
              put_line("  - " & S(Key(prop_cursor)) & " := [" & S(Element(prop_cursor)) & ']');
            end if;
            Wasabee.Hypertext.Styles.Set_property
              (new_style.all, S(Key(prop_cursor)), S(Element(prop_cursor)));
            Next(prop_cursor);
            exit when prop_cursor = No_Element;
          end loop;
        end;
      end if;
    end Apply_simple_CSS_selector;
    --
    -- !! Will disappear, see default sheet in Initialize_styles
    procedure Apply_font_modifiers is
    begin
      case node.kind is
        when a =>
          if node.URL /= "" then
            -- !! kept until "a:link:active, a:visited:active" supported
            Clone_parent_style_if_null;
            Inc(new_style.font.modifier(underlined));
          end if;
        when others =>
          null;
      end case;
    end Apply_font_modifiers;
    --
    --  Merge local style (already created at parsing, defined by style='...' attributes
    --  or by specific attributes) with parent style or CSS style.
    --  E.g. <font face="Broadway" color="#880000"> or <h1 style='color:red;'>
    --
    procedure Apply_local_tag_style is
      procedure Take_local_color(new_color: in out Color_Code; local: Color_Code) is
      begin
        if local = parent_color then
          null;  --  The color in new style is already parent's one.
        else
          new_color:= local;
        end if;
      end;
    begin
      if node.optional_style = null then
        return;
      end if;
      Clone_parent_style_if_null;
      Take_local_color(new_style.fore_color, node.optional_style.fore_color);
      Take_local_color(new_style.back_color, node.optional_style.back_color);
      Take_local_color(new_style.border_color, node.optional_style.border_color);
      if node.optional_style.border_style /= inherit then
        new_style.border_style:= node.optional_style.border_style;
      end if;
      if node.optional_style.list_style /= inherit then
        new_style.list_style:= node.optional_style.list_style;
      end if;
      if node.optional_style.dimensions.x /= unspecified then
        new_style.dimensions.x:= node.optional_style.dimensions.x;
      end if;
      if node.optional_style.dimensions.y /= unspecified then
        new_style.dimensions.y:= node.optional_style.dimensions.y;
      end if;
      if node.optional_style.font.family /= "" then
        new_style.font.family:= node.optional_style.font.family;
      end if;
      if node.optional_style.font.size /= parent_size then
        new_style.font.size:= node.optional_style.font.size;
      end if;
      new_style.font.size_scale:= (new_style.font.size_scale * node.optional_style.font.size_scale) / 100;
      for m in Font_modifier loop
        Add(new_style.font.modifier(m), node.optional_style.font.modifier(m));
      end loop;
      Add(new_style.hiding_level, node.optional_style.hiding_level);
    end Apply_local_tag_style;
    --
    procedure Apply_node_style is
      tag   :          String:= Body_tag'Image(node.kind);
      id    : constant String:= To_Upper(To_String(node.id));
      procedure Apply_CSS_classes(class: String) is
        sep: constant Natural:= Ada.Strings.Fixed.Index(class, " ");
      begin
        if sep = 0 then
          Apply_simple_CSS_selector("*." & class);             --  CSS style by *.class
          Apply_simple_CSS_selector(tag & '.' & class);        --  CSS style by tag.class
        else  --  Multi-class
          Apply_CSS_classes(class(class'First..sep - 1));
          Apply_CSS_classes(class(sep+1 .. class'Last));
        end if;
      end Apply_CSS_classes;
      -- !! We need to pass the whole ancestry of Tags, Id's and Classes to the recursive calls
      --    if there are Descendant Selectors (x y), Child Selectors (x>y) or
      --    Adjacent sibling selectors (x+y) and check all that... (that will be funny !!)
      --    Idea: chain in reverse order in dictionary
      --    http://www.w3.org/TR/CSS2/selector.html#descendant-selectors
      --    http://www.w3.org/TR/CSS2/cascade.html#specificity
      -- !! In two steps: find final properties declarations, then apply them
    begin
      if tag = "B0DY" then  --  With 0 (zero)
        tag := "BODY";      --  With O
      end if;
      --
      --  Style merging: Parent -> CSS -> Font modifications -> Immediate as tag attribute
      --
      --
      --  CSS
      --
      Apply_simple_CSS_selector("*");          --  CSS style from universal selector:  *
      Apply_simple_CSS_selector(tag);          --  CSS style by tag
      if node.class /= "" then                 --  CSS style by *.class, then tag.class
        Apply_CSS_classes(To_Upper(To_String(node.class)));
      end if;
      if node.id /= "" then
        Apply_simple_CSS_selector("*#" & id);        --  CSS style by *#id
        Apply_simple_CSS_selector(tag & '#' & id);   --  CSS style by tag#id
      end if;
      --
      --  Font modifiers (obsolescent)
      --  Local tag style
      --
      Apply_font_modifiers;                    --  Font modifications from tag type (<I>)
      Apply_local_tag_style;                   --  Immediate style within tag as attribute
    end Apply_node_style;
  begin
    if node = null then
      return;
    end if;
    if node.kind in Body_tag then -- Not a text
      Apply_node_style;
      if new_style /= null then
        -- Merge font scaling into font size
        new_style.font.size:= (new_style.font.size * new_style.font.size_scale) / 100;
        new_style.font.size_scale:= 100;
        -- Replace style at the node
        Dispose(node.optional_style);
        node.optional_style:= new_style;
      end if;
    end if;
    if node.kind in Body_bracketing_tag then
      if new_style = null then
        Apply_styles(node.first_child, css_map, parent_style);  --  Children with their grand-parent' style
      else
        Apply_styles(node.first_child, css_map, new_style.all);  --  Children with this node's style
      end if;
    end if;
    Apply_styles(node.next, css_map, parent_style);  --  Siblings
  end Apply_styles;

  default_style_sheet: constant String :=
      "a:link, a:visited { color: violet; text-decoration: underline; cursor: auto; }" &
      "a:link:active, a:visited:active { color: red; }" &
      "ins, u { text-decoration: underline; }" &
      "del, strike, s { text-decoration: line-through; }" &
      "b, strong { font-weight: bold; }" &
      "i, em, var, dfn, cite { font-style: italic; }" &
      "address { display: block; font-style: italic; }" &
      "big { font-size : larger; }" &
      "small { font-size : smaller; }" &
      "sup { vertical-align: super; font-size: smaller; }" &
      "sub { vertical-align: sub; font-size: smaller; }" &
      "code, kbd, samp, tt { font-family: monospace; }" &
      "pre { display: block; font-family: monospace; white-space: pre; margin: 1em 0; }" &
      "" &
      "h1 { display: block; font-size: 2em; margin-top: 0.67em; margin-bottom: 0.67em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "h2 { display: block; font-size: 1.5em; margin-top: 0.83em; margin-bottom: 0.83em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "h3 { display: block; font-size: 1.17em; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "h4 { display: block; margin-top: 1.33em; margin-bottom: 1.33em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "h5 { display: block; font-size: .83em; margin-top: 1.67em; margin-bottom: 1.67em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "h6 { display: block; font-size: .67em; margin-top: 2.33em; margin-bottom: 2.33em; margin-left: 0; margin-right: 0; font-weight: bold; }" &
      "" &
      "" &
      "table { display: table; " &
              "border-collapse: separate; border-spacing: 2px;"&
              "border-color: gray; border-width: 3px}" &
      "tr { display: table-row; border_style: none}" &
      "th { display: table-cell; font-weight: bold; text-align: center; border_style: none}" &
      "td { display: table-cell; border_style: none}" &
      "" &
      "ol { display: block; list-style-type: decimal; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0; padding-left: 40px }  " &
      "ul { display: block; list-style-type: disc;    margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0; padding-left: 40px } " &
      "" &
      "";

  procedure Initialize_styles(ho: in out HT_object) is
  begin
    ho.style_map:= CSS_Dictionary.Empty_Map;
    --
    Parse_Information(ho.style_map, To_Unbounded_String(default_style_sheet));
  end Initialize_styles;

end Wasabee.Hypertext.Styles;
