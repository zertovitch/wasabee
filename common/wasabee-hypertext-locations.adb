with Wasabee.Util;                      use Wasabee.Util;

package body Wasabee.Hypertext.Locations is

  function In_Box(p: Point; b: Box) return Boolean is
  pragma Inline(In_Box);
  begin
    return p.x in b.p1.x .. b.p2.x and p.y in b.p1.y .. b.p2.y;
  end In_Box;

  function Mouse_URL (ho: HT_object; mouse_point: Point) return String is
    candidate, result: Unbounded_String;
    done: exception;
    --
    procedure Traverse (node : p_Body_Node) is
    begin
      if node = null then
        return;
      end if;
      --  Location AND tree pruning happen here.
      if In_Box (mouse_point, node.bounding_box) then
        case node.kind is
          when body_text | img =>
            --  Mouse pointer is within a leaf node's box with something
            --  visible. If hyperlinks were on the way down to here, the one
            --  in the smallest box will be returned.
            result := candidate;
            raise done;  --  Leaf object was found, quit tree traversal.
          when Body_singleton_tag_no_img =>
            null;
          when a =>
            --  Mouse pointer is within an "a" tag's box with perhaps an URL.
            candidate := node.URL;
            Traverse (node.first_child);
            candidate := Null_Unbounded_String;
          when Body_bracketing_tag_no_a =>
            Traverse(node.first_child);
        end case;
      end if;
      Traverse(node.next);  --  Next sibling.
    end Traverse;
  begin
    begin
      Traverse(ho.the_body);
    exception
      when done =>
        null;  --  Found the right node and cancelled search.
    end;
    return S(result);
  end Mouse_URL;

  function Mouse_cursor(ho: HT_object; mouse_point: Point) return Mouse_cursor_style is
    candidate, result: Mouse_cursor_style:= arrow;
    done: exception;
    --
    procedure Traverse(node: p_Body_Node) is
    begin
      if node = null then
        return;
      end if;
      if In_Box(mouse_point,node.bounding_box) then -- Location AND tree pruning for children here!
        case node.kind is
          when body_text | img =>
            if candidate = finger then
              result:= finger;
            else
              result:= I_beam;
            end if;
            raise done;  --  Leaf object was found, quit tree traversal.
          when area | br | col | hr | input | param =>
            null;
          when a =>
            if node.URL /= "" then
              candidate:= finger;
            end if;
            Traverse(node.first_child);
            candidate:= arrow;
          when Body_bracketing_tag_no_a =>
            Traverse(node.first_child);
        end case;
      end if;
      Traverse(node.next);
    end Traverse;
  begin
    begin
      Traverse(ho.the_body);
    exception
      when done =>
        null;  --  Found the right node and cancelled search.
    end;
    return result;
  end Mouse_cursor;

  function Anchor_position(ho: HT_object; anchor: String) return Point is
    result: Point:= (0,0);
    done: exception;
    --
    procedure Traverse(node: p_Body_Node) is
    begin
      if node = null then
        return;
      end if;
      if node.id = anchor then
        result:= node.bounding_box.p1;
        raise done;   --  Anchor was found, quit tree traversal.
      end if;
      if node.kind in Body_bracketing_tag then
        Traverse(node.first_child);
      end if;
      Traverse(node.next);
    end Traverse;
    --
  begin
    begin
      Traverse(ho.the_body);
    exception
      when done =>
        null;  --  Found the right node and cancelled search.
    end;
    return result;
  end Anchor_position;

end Wasabee.Hypertext.Locations;
