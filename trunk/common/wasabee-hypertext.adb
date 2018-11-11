with Wasabee.CSS;                       use Wasabee.CSS;
with Wasabee.Hypertext.Styles;
with Wasabee.URL;                       use Wasabee.URL;
with Wasabee.Util;                      use Wasabee.Util;

-- with Ada.Characters.Handling;           use Ada.Characters.Handling;
-- with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Wasabee.Hypertext is

  procedure Dispose is new Ada.Unchecked_Deallocation(Local_Style, p_Local_Style);

  procedure Delete_body_tree(ho: in out HT_object) is
   --
    procedure Delete_body(bn: in out p_Body_Node) is
      procedure Dispose is new Ada.Unchecked_Deallocation(Body_Node, p_Body_Node);
    begin
      if bn = null then
        return;
      end if;
      if bn.optional_style /= null then
        Dispose(bn.optional_style);
      end if;
      case bn.kind is
        when Body_text_or_singleton_tag =>
          null;
        when Body_bracketing_tag =>
          Delete_body(bn.first_child);
      end case;
      Delete_body(bn.next);
      Dispose(bn); -- Here memory is freed.
    end Delete_body;
  begin
    Delete_body(ho.the_body);
  end Delete_body_tree;

  function "+"(p1, p2: Point) return Point is
  begin
    return (p1.x + p2.x, p1.y + p2.y);
  end "+";

  function "-"(p1, p2: Point) return Point is
  begin
    return (p1.x - p2.x, p1.y - p2.y);
  end "-";

  function Max(b1, b2: Box) return Box is
  begin
    return
      (p1 => (Integer'Min(b1.p1.x, b2.p1.x), Integer'Min(b1.p1.y, b2.p1.y)),
       p2 => (Integer'Max(b1.p2.x, b2.p2.x), Integer'Max(b1.p2.y, b2.p2.y))
      );
  end Max;

  function "+"(b: Box; p: Point) return Box is
  begin
    return (b.p1+p, b.p2+p);
  end "+";

  function "-"(b: Box; p: Point) return Box is
  begin
    return (b.p1-p, b.p2-p);
  end "-";

  function Width(b: Box) return Natural is
  begin
    return b.p2.x - b.p1.x;
  end Width;

  function Height(b: Box) return Natural is
  begin
    return b.p2.y - b.p1.y;
  end Height;

  function Dimensions(b: Box) return Point is
  begin
    return (Width(b), Height(b));
  end Dimensions;

  procedure Post_loading_processing(ho: HT_object; top_style: Local_Style:= default_style) is

    -- Example of tree rectification: 2nd "<li>" of "<li> ... <li>" is child, should be sibling
    -- See "optional tags" sheet in wasa_work.xls
    -- See example6.html to see the effect of Rectify_tree.
    --
    procedure Rectify_tree(node: p_Body_Node; previous, parent: p_Body_Node) is
      --
      procedure Relink_to_parent is
      begin
        --  1/ Cut link on sibling level
        if previous = null then  --  node is first child
          parent.first_child:= node.next;
        else
          previous.next:= node.next;
        end if;
        --  2/ Insert node on parent level
        node.next:= parent.next;
        parent.next:= node;
      end Relink_to_parent;
    begin
      if node = null then
        return;
      end if;
      Rectify_tree(node.next, previous => node, parent => parent);  --  Siblings
      if parent /= null then
        case node.kind is
          when li | th | td | p =>
            if parent.kind = node.kind then
              Relink_to_parent;
            end if;
          when tr =>
            if parent.kind in tr..td then
              Relink_to_parent;
            end if;
          when dt..dd =>
            if parent.kind in dt..dd then
              Relink_to_parent;
            end if;
          when others =>
            null;
        end case;
      end if;
      if node.kind in Body_bracketing_tag then
        Rectify_tree(node.first_child, previous => null, parent => node);  --  Children
      end if;
    end Rectify_tree;
    --
    procedure Count_lists(node: p_Body_Node) is
      p: p_Body_Node;
    begin
      if node = null then
        return;
      end if;
      case node.kind is
        when ul | ol   =>
          p:= node.first_child;
          node.item_count:= 0;
          while p /= null loop
            if p.kind = li then -- Only count <LI>'s
              node.item_count:= node.item_count + 1;
            end if;
            p:= p.next;
          end loop;
        when others =>
          null;
      end case;
      if node.kind in Body_bracketing_tag then
        Count_lists(node.first_child);  --  Children
      end if;
      Count_lists(node.next);  --  Siblings
    end Count_lists;
    --
  begin
    for i in 1..2 loop  --  We need 2 passes for rectifying <tr><td><tr>.
      Rectify_tree(ho.the_body, previous => null, parent => null);
    end loop;
    Count_lists(ho.the_body);
    Wasabee.Hypertext.Styles.Apply_styles(ho.the_body, ho.style_map, top_style);
  end Post_loading_processing;

  procedure Move(node: p_Body_Node; by: Point) is
  begin
    if node = null then
      return;
    end if;
    node.bounding_box:= node.bounding_box + by;
    if node.kind in Body_bracketing_tag then
      Move(node.first_child, by);  --  Children
    end if;
    Move(node.next, by);  --  Siblings
  end Move;

  procedure Fit_bounding_boxes(ho: in out HT_object) is
    --
    procedure Fit(node: p_Body_Node) is
      --
      procedure Fit_table is
        m, n: Natural;
        --
        procedure Find_dimensions is
          pi, pj: p_Body_Node;
          n_row: Natural;  --  Number of columns for current row
        begin
          m:= 0;
          n:= 0;
          pi:= node.first_child;
          while pi /= null loop
            if pi.kind = tr then
              m:= m + 1;
              n_row:= 0;
              pj:= pi.first_child;
              while pj /= null loop
                if pj.kind = th or pj.kind = td then
                  n_row:= n_row + 1;
                  n:= Integer'Max(n, n_row);
                end if;
                pj:= pj.next;
              end loop;
            end if;
            pi:= pi.next;
          end loop;
        end Find_dimensions;
        --
        procedure Fit_cells is
          max_height : array(1..m) of Natural:= (others => 0);
          max_width  : array(1..n) of Natural:= (others => 0);
          pi, pj: p_Body_Node;
          i, j: Natural;
          cell_O: Point;
          cellspacing: constant:= 2; -- !! hardcoded
          -- Padding already done with the margins of each cell upon invisible Draw
        begin
          --  Pass 1: find widths and heights
          pi:= node.first_child;
          i:= 0;
          while pi /= null loop
            if pi.kind = tr then
              i:= i + 1;
              pj:= pi.first_child;
              j:= 0;
              while pj /= null loop
                if pj.kind = th or pj.kind = td then
                  j:= j + 1;
                  --  Recursion here: a cell may contain elements to be fitted, like tables !
                  Fit(pj);
                  max_height(i):= Integer'Max(max_height(i), Height(pj.bounding_box));
                  max_width(j) := Integer'Max(max_width(j),  Width(pj.bounding_box));
                end if;
                pj:= pj.next;
              end loop;
            end if;
            pi:= pi.next;
          end loop;
          put("Widths"); for w of max_width loop put(w'img); end loop; new_line;
          put("Heights"); for h of max_height loop put(h'img); end loop; new_line;
          --  Pass 2: adjust cells
          pi:= node.first_child;
          i:= 0;
          cell_O.y:= node.first_child.bounding_box.p1.y + cellspacing;
          while pi /= null loop
            if pi.kind = tr then
              i:= i + 1;
              pj:= pi.first_child;
              j:= 0;
              cell_O.x:= node.first_child.bounding_box.p1.x + cellspacing;
              while pj /= null loop
                if pj.kind = th or pj.kind = td then
                  j:= j + 1;
                  put_line(
                    "i="&i'img& " j="&j'img& "   ("&
                    pj.bounding_box.p1.x'img & pj.bounding_box.p1.y'img & ") -> (" &
                    cell_O.x'img & cell_O.y'img &
                    ") w=" & Width(pj.bounding_box)'img & " h=" & Height(pj.bounding_box)'img
                  );
                  Move(pj, cell_O - pj.bounding_box.p1);
                  -- Move the cell as originally "drawn" (in invisible mode) to cell_O + padding
                  cell_O.x:= cell_O.x + max_width(j) + cellspacing;
                end if;
                pj:= pj.next;
              end loop;
              cell_O.y:= cell_O.y + max_height(i) + cellspacing;
            end if;
            pi:= pi.next;
          end loop;
          node.bounding_box.p2:= cell_O;
          if node.optional_style /= null then -- Right-Bottom gap
            node.bounding_box.p2:= node.bounding_box.p2 + node.optional_style.border_spacing;
          end if;
        end Fit_cells;
        --
      begin
        Find_dimensions;
        Fit_cells;
      end Fit_table;
    begin
      if node = null then
        return;
      end if;
      case node.kind is
        when table =>
          Fit_table;
        when others =>
          if node.kind in Body_bracketing_tag then
            Fit(node.first_child);  --  Children of any node but <TABLE>
          end if;
      end case;
      Fit(node.next);  --  Siblings
    end Fit;
  begin
    Fit(ho.the_body);
  end Fit_bounding_boxes;

  function Title(ho: HT_object) return UTF_16_String is
  begin
    return To_Wide_String(ho.title);
  end Title;

  function Bounding_box(ho: HT_object) return Box is
  begin
    return ho.main_bounding_box;
  end Bounding_box;

  function Get_own_URL(ho: HT_object) return String is
  begin
    return S(ho.own_URL);
  end Get_own_URL;

  procedure Set_own_URL(ho: in out HT_object; new_own_URL: String) is
  begin
    ho.own_URL:= U(new_own_URL);
  end Set_own_URL;

  function Build_URL (ho: HT_object; partial_URL: String) return String is
  begin
    if ho.base_URL = "" then
      return Build_URL(S(ho.own_URL), partial_URL);
    else
      return Build_URL(S(ho.base_URL), partial_URL);
    end if;
  end Build_URL;

  procedure Set_Encoding(ho: in out HT_object; encoding: Encoding_Choice) is
  begin
    ho.encoding:= encoding;
  end Set_Encoding;

  procedure Dump(ho: HT_object; file: Ada.Wide_Text_IO.File_Type) is
    use Ada.Strings.Wide_Fixed, Ada.Wide_Text_IO;
    --
    procedure Dump_body(bn: p_Body_Node; level: Natural:= 0) is -- Scary name ;-) !
    begin
      if bn = null then
        return;
      end if;
      Put(file, Level * "|  "); -- show indentation
      case bn.kind is
        when body_text   =>
          Put_Line(file, "text: [" & S(bn.content) & ']');
        when Body_singleton_tag =>
          Put_Line(file, '<' & Body_kind'Wide_Image(bn.kind) & "/>");
        when Body_bracketing_tag =>
          Put_Line(file, '<' & Body_kind'Wide_Image(bn.kind) & '>');
          Dump_body(bn.first_child, level + 1);
      end case;
      Dump_body(bn.next, level);
    end Dump_body;
  begin
    Put_Line(file, "Title: " & S(ho.title));
    Dump_body(ho.the_body);
  end Dump;

  procedure Dump(ho: HT_object; file_name: String) is
    use Ada.Wide_Text_IO;
    file: Ada.Wide_Text_IO.File_Type;
  begin
    Create(file, Out_File, file_name);
    Dump(ho, file);
    Close(file);
  end Dump;

  function Root (o : in HT_object) return p_Body_node
  is begin
    return o.the_body;
  end Root;

  function Kind (Node : in p_Body_node) return HTML_kind
  is begin
    return Node.kind;
  end Kind;

  function First_child (Node : in p_Body_node) return p_Body_node
  is begin
    return Node.First_child;
  end First_child;

  function Next_sibling (Node : in p_Body_node) return p_Body_node
  is begin
    return Node.next;
  end Next_sibling;

  function Text  (Node : in p_Body_node) return UTF_16_Unbounded_string
  is begin
    return node.content;
  end Text;

  function Class_Attribute (Node : in p_Body_node) return String
  is
  begin
    return To_String (Node.class);
  end Class_Attribute;

  function URL (Node : in p_Body_node) return String is
  begin
    return To_String (Node.URL);
  end URL;

  procedure Finalize(ho: in out HT_object) is
  begin
    Delete_body_tree(ho);
    Ada.Finalization.Controlled(ho).Finalize;
  end Finalize;

end Wasabee.Hypertext;
