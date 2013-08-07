with Wasabee.Util;                      use Wasabee.Util;

with Dom.Core                     ; use Dom.Core                         ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes                        ;

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Text_IO;                       use Ada.Text_IO;

package body Wasabee.Hypertext is

  procedure Load_frame(ho: in out HTML_object; from: DOM.Core.Node_List) is

    type Location_type is (nowhere, in_head, in_body);

    location: Location_type:= nowhere;

    function To_UTF_16(s: DOM_String) return UTF_16_String is
    begin
      return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
      -- !! only from utf-8
    end To_UTF_16;

    procedure Process (Nd : Node; Level: Natural:= 0) is
      Children : constant Node_List := Child_Nodes(Nd);
      Attrs    : constant Named_Node_Map := Attributes (Nd);
      Name     : constant DOM_String:= Node_Name(Nd);
      Value    : constant DOM_String:= Node_Value(Nd);
      Tmp_attr : Node;
      kind     : Body_kind;
    begin
      -- Process the node itself
      case Level is
        when 0 =>
          null; -- Name should be "html"
        when 1 =>
          if Name = "head" then
            location:= in_head;
          elsif Name = "body" then
            location:= in_body;
          else
            location:= nowhere;
          end if;
        when others =>
          case location is
            when in_head =>
              if Name = "title" and then
                Length (Children) > 0 and then
                Node_Name(Item(Children, 0)) = "#text"
              then
                ho.title:= U(To_UTF_16(Node_Value(Item(Children, 0))));
              end if;
            when in_body =>
              if Name = "#text" then
                put_line(value); --!!
              else
                begin
                  kind:= Body_kind'Value(Name);
                  put_line(kind'img);--!!
                exception
                  when Constraint_Error =>
                    null; -- unknown tag
                end;
              end if;
            when nowhere =>
              null;
          end case;
      end case;
      -- Process the attributes
      for Index in 1 .. Length(Attrs) loop
        Tmp_attr := Item(Attrs, Index-1);
        -- here...
      end loop;
      -- Process the children
      for Index in 1 .. Length (Children) loop
        Process(Item(Children, Index-1), Level + 1);
      end loop;
    end Process;

  begin
    Process(Item(from,0));
  end Load_frame;

  function Title(ho: HTML_object) return UTF_16_String is
  begin
    return To_Wide_String(ho.title);
  end Title;

end Wasabee.Hypertext;
