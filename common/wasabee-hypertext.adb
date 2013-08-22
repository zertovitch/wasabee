with Wasabee.Util;                      use Wasabee.Util;

with Dom.Core.Nodes;                    use Dom.Core, Dom.Core.Nodes;

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.Wide_Fixed;
-- with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Wasabee.Hypertext is

  procedure Load_frame(ho: in out HT_object; from: DOM.Core.Node_List) is

    type Location_type is (nowhere, in_head, in_body);

    location: Location_type:= nowhere;

    function To_UTF_16(s: DOM_String) return UTF_16_String is
    begin
      return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
      -- !! only from utf-8
    end To_UTF_16;

    type p_p_Body_node is access all p_Body_node;

    current_body_pointer: p_p_Body_node:= ho.the_body'Access;

    procedure Process (Nd : Node; Level: Natural:= 0) is
      Children : constant Node_List := Child_Nodes(Nd);
      Attrs    : constant Named_Node_Map := Attributes (Nd);
      Name     : constant DOM_String:= Node_Name(Nd);
      Value    : constant DOM_String:= Node_Value(Nd);
      Tmp_attr : Node;
      kind     : Body_kind;
      new_node : p_Body_node;
      --
      procedure Process_children is
      begin
        for Index in 1 .. Length (Children) loop
          Process(Item(Children, Index-1), Level + 1);
        end loop;
      end Process_children;
      --
      procedure Process_tag is
      begin
        kind:= Body_kind'Value(Name);
        new_node:= new Body_Node(kind);
        current_body_pointer.all:= new_node;
        --
        -- Process some attributes
        --
        case kind is
          when a =>
            for Index in 1 .. Length(Attrs) loop
              Tmp_attr := Item(Attrs, Index-1);
              if Node_Name(Tmp_Attr) = "href" then
                new_node.URL:= U(Node_Value(Tmp_Attr));
              end if;
            end loop;
          when font =>
            for Index in 1 .. Length(Attrs) loop
              Tmp_attr := Item(Attrs, Index-1);
              if Node_Name(Tmp_Attr) = "face" then
                new_node.face:= U(Node_Value(Tmp_Attr));
              elsif Node_Name(Tmp_Attr) = "color" then
                new_node.color:= Get_hex_value(Node_Value(Tmp_Attr));
              end if;
            end loop;
          when others => null;
        end case;
        --
        -- Process children - if any
        --
        if kind not in Text_or_singleton_tag then
          current_body_pointer:= new_node.first_child'Access;
          Process_children;
        end if;
        current_body_pointer:= new_node.next'Access; -- ready for next sibling
      exception
        when Constraint_Error =>
          null; -- unknown tag
      end Process_tag;
      --
    begin
      -- Process the node itself
      case Level is
        when 0 =>
          if Name = "html" then
            Process_children;
          else
            raise Constraint_Error with "Root tag name should be ""html""";
          end if;
        when 1 =>
          if Name = "head" then
            location:= in_head;
            Process_children;
          elsif Name = "body" then
            location:= in_body;
            Process_children;
          else
            location:= nowhere;
          end if;
        when others => -- nesting level 2,3,4,...
          case location is
            when in_head =>
              if Name = "title" and then
                Length (Children) > 0 and then
                Node_Name(Item(Children, 0)) = "#text"
              then
                ho.title:= U("WASABEE v0.0.1 - ") & U(To_UTF_16(Node_Value(Item(Children, 0))));
              end if;
            when in_body =>
              if Name = "#text" then
                new_node:= new Body_Node(text);
                new_node.content:= U(Filter_blanks(To_UTF_16(Value)));
                current_body_pointer.all:= new_node;
                current_body_pointer:= new_node.next'Access; -- ready for next sibling
              else -- try with tags
                Process_tag;
              end if;
            when nowhere =>
              null;
          end case;
      end case;
    end Process;

  begin
    Process(Item(from,0));
  end Load_frame;

  function Title(ho: HT_object) return UTF_16_String is
  begin
    return To_Wide_String(ho.title);
  end Title;

  procedure Dump(ho: HT_object; file: Ada.Wide_Text_IO.File_Type) is
    use Ada.Strings.Wide_Fixed, Ada.Wide_Text_IO;
    --
    procedure Dump_body(bn: p_Body_Node; level: Natural:= 0) is -- Scary name ;-) !
    begin
      if bn = null then
        return;
      end if;
      Put(Level * 3 * ' '); -- show indentation
      case bn.kind is
        when text       =>
          Put_Line(file, "text: [" & S(bn.content) & ']');
        when br | hr    =>
          Put_Line(file, '<' & Body_kind'Wide_Image(bn.kind) & '>');
        when Normal_tag =>
          Put_Line(file, '<' & Body_kind'Wide_Image(bn.kind) & '>');
          Dump_body(bn.first_child, level + 1);
      end case;
      Dump_body(bn.next, level);
    end Dump_body;
  begin
    Put_Line(file, "Title: " & S(ho.title));
    Dump_body(ho.the_body);
  end Dump;

  procedure Delete_body_tree(ho: in out HT_object) is
   --
    procedure Delete_body(bn: in out p_Body_Node) is
      procedure Dispose is new Ada.Unchecked_Deallocation(Body_Node, p_Body_Node);
    begin
      if bn = null then
        return;
      end if;
      case bn.kind is
        when text|hr|br       =>
          null;
        when Normal_tag =>
          Delete_body(bn.first_child);
      end case;
      Delete_body(bn.next);
      Dispose(bn); -- Here memory is freed.
    end Delete_body;
  begin
    Delete_body(ho.the_body);
  end Delete_body_tree;

  procedure Finalize(ho: in out HT_object) is
  begin
    Delete_body_tree(ho);
    Ada.Finalization.Controlled(ho).Finalize;
  end Finalize;

end Wasabee.Hypertext;
