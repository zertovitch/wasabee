-----------------------------------------------------------------------------------
-- This package deals with the in-memory representation and storage of Web pages --
-----------------------------------------------------------------------------------

with Wasabee.Colors;                    use Wasabee.Colors;
with Wasabee.Encoding;                  use Wasabee.Encoding;
with Wasabee.Images;                    use Wasabee.Images;
with Wasabee.CSS;

with Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Wide_Text_IO;

package Wasabee.Hypertext is

  subtype Font_family_name is Unbounded_String;
  -- With quotes, without space outside quotes, comma sparated
  -- Example: "Times New Roman",Times,Serif

  type Font_modifier is (bold, italic, underlined, strikethrough);
  type Font_modifier_level is array(Font_modifier) of Natural;
  -- ^ 0: not bold, 1: one <b>, 2: two <b> or <h1><b>, etc.

  type Font_descriptor is record
    family     : Font_family_name;
    size       : Integer := 100;  --  size in percents (100 is medium font)
    size_scale : Positive:= 100;  --  scaling from parent size, in percents (100 is neutral)
    modifier   : Font_modifier_level:= (others => 0);
  end record;

  default_font: constant Font_descriptor:=
    (family         => To_Unbounded_String("serif"),
     others         => <>
    );

  parent_size: constant:= -1;

  parent_font: constant Font_descriptor:=
    (family         => Null_Unbounded_String,
     size           => parent_size,
     others         => <>
    );

  type Border_styling is
    (none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset, initial, inherit);

  type List_styling is (none, disc, circle, square, decimal, initial, inherit);
  subtype Geom_list_styling is List_styling range disc..square;
  subtype Ordered_list_styling is List_styling range decimal..decimal; -- many others

  type Point is record x, y: Integer; end record;
  function "+"(p1, p2: Point) return Point;
    pragma Inline("+");
  function "-"(p1, p2: Point) return Point;
    pragma Inline("-");

  unspecified: constant:= -1;
  auto       : constant:=  0;  --  Fit with image dimensions

  type Local_Style is record
    fore_color     : Color_Code      := parent_color;
    back_color     : Color_Code      := parent_color;
    border_color   : Color_Code      := parent_color;
    border_style   : Border_styling  := inherit;
    border_spacing : Point           := (0, 0);  --  For tables
    border_width   : Natural         := 1;
    dimensions     : Point           := (unspecified, unspecified);  --  width, height
    font           : Font_Descriptor := parent_font;
    hiding_level   : Natural         := 0;  -- Incremented e.g. through style="display: none"
    list_style     : List_styling    := inherit;
    -- more to come...
  end record;

  type p_Local_Style is access Local_Style;

  default_style: Local_Style:=
    (fore_color   => default_color,
     back_color   => default_bg_color,
     border_color => default_color,
     border_style => none,
     font         => default_font,
     list_style   => none,
     others       => <>);

  type Box is record p1, p2: Point; end record; -- p1.x <= p2.x, p1.y <= p2.y for non-empty boxes
  function "+"(b: Box; p: Point) return Box;
    pragma Inline("+");
  function "-"(b: Box; p: Point) return Box;
    pragma Inline("-");
  function Max(b1, b2: Box) return Box; -- return the smallest box containing boxes b1 and b2
    pragma Inline(Max);
  function Width(b: Box) return Natural;
    pragma Inline(Width);
  function Height(b: Box) return Natural;
    pragma Inline(Height);
  function Dimensions(b: Box) return Point;
    pragma Inline(Dimensions);

  --------------------------------------------------------------
  -- This is the HyperText object. It contains all the useful --
  -- informations for displaying a web page or frame.         --
  -- Its structure is close to the HTML language.             --
  --------------------------------------------------------------

  type HT_object is new Ada.Finalization.Controlled with private;

  -- After loading and before displaying, some post-processing done once in th HT_object's life
  -- * Define absolute local styles from CSS and font modifiers
  -- * Count <OL> list to help display with maximum width
  -- * Rectify tree for unclosed tags: e.g. <li>...<li> (child) becomes <li>...</li><li> (sibling)
  -- What is not done here: finding the bounding boxes and subsequent layout fitting.
  -- This is done by drawing the HT_object in invisible mode with Wasabee.Hypertext.Display.Draw.
  --
  procedure Post_loading_processing(ho: HT_object; top_style: Local_Style:= default_style);

  -- After setting up the leaf object bounding boxes are found by drawing in
  -- invisible mode with Wasabee.Hypertext.Display.Draw, the bounding boxes in the tree
  -- are adjusted to smallest fit. We need to change that in some cases, e.g. on <TABLE> nodes.
  --
  procedure Fit_bounding_boxes(ho: in out HT_object);

  function Title(ho: HT_object) return UTF_16_String;

  function Bounding_box(ho: HT_object) return Box;

  function Get_own_URL(ho: HT_object) return String;

  procedure Set_own_URL(ho: in out HT_object; new_own_URL: String);

  function Build_URL (ho: HT_object; partial_URL: String) return String;

  procedure Set_Encoding(ho: in out HT_object; encoding: Encoding_Choice);

  procedure Dump(ho: HT_object; file: Ada.Wide_Text_IO.File_Type);

  procedure Dump(ho: HT_object; file_name: String);

  ----------
  --  Querying the HT_object

  type HTML_kind is (
    comment,
    html,
    -- Bracketing tags that can be anywhere in the HTML code: <TAG> something... </TAG>
    unknown_tag,
    script,
    -----------------
    -- HEAD region --
    -----------------
    head_text, -- Text, not a tag !
    -- HEAD region - Singleton tags: <TAG> or <TAG /> (XHTML)
    base,
    basefont, -- deprecated, but we need to know it is a singleton
    link,
    -- HEAD region - Bracketing tags: <TAG> something... </TAG>
    head,
    meta,
    style,
    title,
    -----------------
    -- BODY region --
    -----------------
    body_text, -- Text, not a tag !
    -- BODY region - Singleton tags: <TAG> or <TAG /> (XHTML)
    img,
    area, br, col, hr,
    input, param,
    -- BODY region - Bracketing tags: <TAG> something... </TAG>
    a,
    section,
    b0dy,
    b,i,u,strike,s,
    strong, em, dfn, var,
    big, small,
    sup, sub,
    code, samp, kbd, tt,
    del, ins, abbr, acronym, cite, blockquote,
    article, aside, figure, figcaption,
    address, nav, q, dl, dt, dd,
    details, summary,
    font, pre,
    h1, h2, h3, h4, h5, h6,
    p, div, span,
    ul, ol, li,        -- lists
    table, tr, th, td  -- tables
  );

  subtype Univ_Bracketing_tag is HTML_kind range unknown_tag .. script;

  subtype Head_kind is HTML_kind range head_text .. HTML_kind'Pred(body_text);
  subtype Head_tag is Head_kind range Head_kind'Succ(head_text) .. Head_kind'Last; -- all but text
  subtype Head_singleton_tag is Head_kind range Head_kind'Succ(head_text) .. meta;
  subtype Head_bracketing_tag is Head_tag range Head_tag'Succ(Head_singleton_tag'Last) .. Head_tag'Last;

  subtype Body_kind is HTML_kind range body_text .. HTML_kind'Last;
  subtype Body_tag is Body_kind range Body_kind'Succ(body_text) .. Body_kind'Last; -- all but text
  subtype Body_singleton_tag is Body_tag range Body_tag'First .. param;
  subtype Body_text_or_singleton_tag is Body_kind range Body_kind'First .. Body_singleton_tag'Last;
  subtype Body_singleton_tag_no_img is Body_tag range Body_kind'Succ(img) .. Body_singleton_tag'Last;
  subtype Body_bracketing_tag is Body_tag range Body_tag'Succ(Body_singleton_tag'Last) .. Body_tag'Last;
  subtype Body_bracketing_tag_no_a is Body_tag range Body_tag'Succ(a) .. Body_tag'Last;

  -- missing singletons so far: <frame> <isindex>

  type Body_Node(kind: Body_kind) is private;
  type p_Body_node is access Body_node;

  function Root (o : in HT_object) return p_Body_node;
  function Kind (Node : in p_Body_node) return HTML_kind;
  function First_child (Node : in p_Body_node) return p_Body_node;
  function Next_sibling (Node : in p_Body_node) return p_Body_node;
  function Text  (Node : in p_Body_node) return UTF_16_Unbounded_string;
  function Class_Attribute (Node : in p_Body_node) return String;
  function URL (Node : in p_Body_node) return String;

private

  -- !! Body_Node: to do: use a balanced mix of inheritance and use of Body_kind

  type Body_Node(kind: Body_kind) is tagged record
    bounding_box   : Box;
    id, class      : Unbounded_String;
    optional_style : p_Local_Style:= null;        --  If null, the parent style prevails
    next           : aliased p_Body_node:= null;  --  Next sibling
    case kind is
      -- Text or singleton tags
      when body_text  => content: UTF_16_Unbounded_String;
      when area | br | col => null;
      when hr         =>
        hr_height     : Natural:= 2;
      when img =>
        src_URL       : Unbounded_String; -- complete URL
        bitmap        : Bitmap_Type; -- !! will be in some cache object definition
      when input => null;
      when param => null;
      -- Bracketing tags: <TAG> something (the children)... </TAG>
      when Body_bracketing_tag =>
        first_child: aliased p_Body_node:= null;
        case kind is
          when a =>
            URL           : Unbounded_String; -- complete URL
          when ul | ol =>
            item_count    : Natural; -- useful for getting the maximum marker width
          when others =>
            null;
        end case;
    end case;
  end record;

  procedure Move(node: p_Body_Node; by: Point);

  type HT_object is new Ada.Finalization.Controlled with record
    title             : UTF_16_Unbounded_String;
    main_bounding_box : Box;
    style_map         : Wasabee.CSS.CSS_Dictionary.Map;
    the_body          : aliased p_Body_node:= null;
    own_URL           : Unbounded_String; -- NB: URL without anchor
    base_URL          : Unbounded_String; -- given in <base> tag
    encoding          : Encoding_Choice := utf_8;  --  Most common choice
  end record;

  overriding
  procedure Finalize(ho: in out HT_object);

  procedure Delete_body_tree(ho: in out HT_object);

end Wasabee.Hypertext;
