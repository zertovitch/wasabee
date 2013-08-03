with Ada.Text_IO ; use Ada.Text_IO ;

with Unicode.CES.Utf8        ;

package body Wasabee.Xhtml is

   -- N                : Node ;
   -- Http_Header      : Node ;
   -- Http_Body        : Node ;

   procedure Display_Node (Nd : Node) is
      Attrs : Named_Node_Map ;
      Lgt   : Natural ;
      Tmp   : Node;
   begin
      Put_Line(Node_Name(Nd) & " " & Value(Nd));

      Attrs := Attributes (Nd);
      Lgt := Length(Attrs);
      for Index in 1..Lgt loop
         Tmp := Item(Attrs,Index-1);
         Display_Node(Tmp);
      end loop;

   end;

   procedure Display_All_Node (L : Node_List) is
      Nd : Node ;
   begin
      for Index in 1 .. Length (L) loop
         Nd := Item(L, Index-1);
         Display_Node(Nd);
      end loop;
   end ;

   procedure Display_All_Children (Nd : Node) is
      L : Node_List := Child_Nodes(Nd);
      Tmp : Node ;
   begin
      for Index in 1 .. Length (L) loop
         Tmp := Item(L, Index-1);
         Put(Integer'Image(Index) & " : " );
         Display_Node(Tmp);
         Display_All_Children(Tmp);
      end loop;
   end ;

   procedure Get_Xhtml_Content (Content : String ;
                                List : in out Node_List ;
                                Reader : in out Tree_reader
                               ) is
      Doc              : Document ;
      Input            : Input_Sources.Strings.String_Input ;
   begin
      Input_Sources.Strings.Open (Content, Unicode.CES.Utf8.Utf8_Encoding, Input);
      Parse(Reader, Input) ;
      Close(Input);
      Doc := Get_Tree(Reader);
      List  := Get_Elements_By_Tag_Name(Doc,"html");
   end;

   procedure Extract_Html ( Source : in Unbounded_String ;
                            Target : in out Unbounded_String ) is
      L : Natural ;
      Start_Tag, End_Tag : Natural ;
   begin
      L := Length(Source);
      Start_Tag := Index(Source,"<html",1);
      End_Tag := Index(Source,"</html>",1);
      -- Put_Line("************************* SOURCE ****************************") ;
      -- Put_Line(To_String(Source));
      Target := Unbounded_Slice(Source, Start_Tag , End_Tag+6);
      -- Put_Line("************************* TARGET ****************************") ;
      -- Put_Line(To_String(Target));
   end;



end ;