with DOM.Core;

with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;

package Wasabee.Hypertext is

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  type Body_node;

  type p_Body_node is access Body_node;

  type Body_kind is (
    -- Text or singleton tags
    text,
    hr, br,
    -- Normal tags
    b,i,u,strike,
    strong, em, dfn, var,
    code, samp, kbd, tt,
    h1, h2, h3, h4, h5, h6,
    p, div,
    ul, ol, li -- lists
  );

  subtype HTML_tag is Body_kind range hr .. Body_kind'Last;
  subtype Singleton_tag is HTML_tag range HTML_tag'First .. br;
  subtype Text_or_singleton_tag is Body_kind range Body_kind'First .. Singleton_tag'Last;
  subtype Normal_tag is HTML_tag range HTML_tag'Succ(Singleton_tag'Last) .. HTML_tag'Last;
  -- missing:
  -- <area> <base> <basefont> <col> <frame> <img> <input> <isindex> <link> <meta> <param>

  type Body_Node(kind: Body_kind) is record
    x, y, w, h: Natural;
    next      : aliased p_Body_node:= null; -- Next sibling
    case kind is
      -- Text or singleton tags
      when text       => content: UTF_16_Unbounded_String;
      when hr         => null; -- hr style !!
      when br         => null;
      -- Normal tags
      when Normal_tag => first_child: aliased p_Body_node:= null;
    end case;
  end record;

  type HTML_object is new Ada.Finalization.Controlled with record
    title   : UTF_16_Unbounded_String;
    the_body: aliased p_Body_node;
  end record;

  procedure Load_frame(ho: in out HTML_object; from: DOM.Core.Node_List);

  function Title(ho: HTML_object) return UTF_16_String;

  procedure Dump(ho: HTML_object; file: Ada.Wide_Text_IO.File_Type);

private

  overriding
  procedure Finalize(ho: in out HTML_object);

end Wasabee.Hypertext;
