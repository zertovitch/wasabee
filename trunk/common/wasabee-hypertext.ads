with DOM.Core;

with Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;

package Wasabee.Hypertext is

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  subtype Color_Code is Integer range -1 .. 2**24-1;
  -- solid 24-bit RGB color, plus a -1 value for default color
  Default_color: constant Color_Code:= -1;
  Black        : constant Color_Code:= 0;
  White        : constant Color_Code:= 16#FF_FF_FF#;

  subtype Font_face_name is Unbounded_String;

  --------------------------------------------------------------
  -- This is the HyperText object. It contains all the useful --
  -- informations for displaying a web page or frame.         --
  -- Its structure is close to the HTML language.             --
  --------------------------------------------------------------

  type HT_object is new Ada.Finalization.Controlled with private;

  -- Load an HT object from an XML tree (for the XHTML prototype)
  procedure Load_frame(ho: in out HT_object; from: DOM.Core.Node_List);

  -- Load an HT object from any stream
    -- procedure Load_frame(ho: in out HT_object; from: Root_Stream_Type'Class);

  function Title(ho: HT_object) return UTF_16_String;

  procedure Dump(ho: HT_object; file: Ada.Wide_Text_IO.File_Type);

private

  type Body_kind is (
    -- Text or singleton tags
    text,
    hr, br,
    -- Normal tags
    a,
    b,i,u,strike,s,
    strong, em, dfn, var,
    code, samp, kbd, tt,
    del, ins, abbr, acronym, cite, blockquote, article, aside,
    address, nav, q, dl, dt, dd,
    font,
    h1, h2, h3, h4, h5, h6,
    p, div, span,
    ul, ol, li -- lists
  );

  subtype HTML_tag is Body_kind range hr .. Body_kind'Last;
  subtype Singleton_tag is HTML_tag range HTML_tag'First .. br;
  -- missing singletons so far:
  -- <area> <base> <basefont> <col> <frame> <img> <input> <isindex> <link> <meta> <param>

  subtype Text_or_singleton_tag is Body_kind range Body_kind'First .. Singleton_tag'Last;
  subtype Normal_tag is HTML_tag range HTML_tag'Succ(Singleton_tag'Last) .. HTML_tag'Last;
  subtype Normal_tag_no_a is HTML_tag range b .. HTML_tag'Last;

  type Box is record
    x1,y1,x2,y2: Natural;
  end record;

  type Body_node;
  type p_Body_node is access Body_node;

  type Body_Node(kind: Body_kind) is record
    bounding_box: Box;
    next        : aliased p_Body_node:= null; -- Next sibling
    case kind is
      -- Text or singleton tags
      when text       => content: UTF_16_Unbounded_String;
      when hr         => null; -- hr style !!
      when br         => null;
      -- Normal tags
      when Normal_tag =>
        first_child: aliased p_Body_node:= null;
        case kind is
          when a =>
            URL   : Unbounded_String;
          when font =>
            face  : Font_face_name;
            color : Color_Code:= Default_color;
          when others =>
            null;
        end case;
    end case;
  end record;

  type HT_object is new Ada.Finalization.Controlled with record
    title    : UTF_16_Unbounded_String;
    the_body : aliased p_Body_node;
  end record;

  overriding
  procedure Finalize(ho: in out HT_object);

end Wasabee.Hypertext;
