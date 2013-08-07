with DOM.Core;

with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee.Hypertext is

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  type HTML_object is new Ada.Finalization.Controlled with private;

  procedure Load_frame(ho: in out HTML_object; from: DOM.Core.Node_List);

  function Title(ho: HTML_object) return UTF_16_String;

private

  type Body_node;

  type p_Body_node is access Body_node;

  type Body_kind is (text, b,i,u, h1,h2,h3,h4,h5,h6);

  type Body_Node(kind: Body_kind) is record
    x, y, w, h: Natural;
    next      : p_Body_node:= null; -- Next sibling
    case kind is
      when text       => content: UTF_16_Unbounded_String;
      when b|i|u|
           h1|h2|h3|
           h4|h5|h6   => part: p_Body_node:= null;
    end case;
  end record;

  type HTML_object is new Ada.Finalization.Controlled with record
    title   : UTF_16_Unbounded_String;
    the_body: p_Body_node;
  end record;

end Wasabee.Hypertext;
