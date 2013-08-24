with Wasabee.Util;                      use Wasabee.Util;

package body Wasabee.Hypertext.Locations is

  function In_Box(x,y: Natural; b: Box) return Boolean is
  pragma Inline(In_Box);
  begin
    return x in b.p1.x .. b.p2.y and y in b.p1.y .. b.p2.y;
  end In_Box;

  function URL_Click (ho: HT_object; x,y: Natural) return String is
    rough_result, result: Unbounded_String;
    done: exception;
    --
    procedure Traverse(bn: p_Body_Node) is
    begin
      if bn = null then
        return;
      end if;
      if In_Box(x,y,bn.bounding_box) then -- Location AND tree pruning for children here!
        case bn.kind is
          when text => -- !! img too
            result:= rough_result;
            raise done; -- leaf object found
          when a =>
            rough_result:= bn.URL;
            Traverse(bn.first_child);
            rough_result:= Null_Unbounded_String;
          when Normal_tag_no_a =>
            Traverse(bn.first_child);
          when others =>
            null;
        end case;
      end if;
      Traverse(bn.next);
    end Traverse;
  begin
    begin
      Traverse(ho.the_body);
    exception
      when done =>
        null; -- found the right node and cancelled search
    end;
    return S(result);
  end URL_Click;

  function Mouse_move(ho: HT_object; x,y: Natural) return Mouse_cursor_style is
    rough_result, result: Mouse_cursor_style:= arrow;
    done: exception;
    --
    procedure Traverse(bn: p_Body_Node) is
    begin
      if bn = null then
        return;
      end if;
      if In_Box(x,y,bn.bounding_box) then -- Location AND tree pruning for children here!
        case bn.kind is
          when text => -- !! img too
            if rough_result = finger then
              result:= finger;
            else
              result:= I_beam;
            end if;
            raise done; -- leaf object found
          when a =>
            rough_result:= finger;
            Traverse(bn.first_child);
            rough_result:= arrow;
          when Normal_tag_no_a =>
            Traverse(bn.first_child);
          when others =>
            null;
        end case;
      end if;
      Traverse(bn.next);
    end Traverse;
  begin
    begin
      Traverse(ho.the_body);
    exception
      when done =>
        null; -- found the right node and cancelled search
    end;
    return result;
  end Mouse_move;

end Wasabee.Hypertext.Locations;
