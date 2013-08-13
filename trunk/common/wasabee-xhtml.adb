with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO ; use Ada.Text_IO ;

with Unicode.CES.Utf8        ;

with Wasabee.Css ; use Wasabee.Css ;

package body Wasabee.Xhtml is

   -- N                : Node ;
   -- Http_Header      : Node ;
   -- Http_Body        : Node ;

   procedure Display_Node (Nd : Node) is
      Attrs : Named_Node_Map ;
      Lgt   : Natural ;
      Tmp   : Node;
      Props : Css_Properties.Vector;
   begin
      Put_Line(Node_Name(Nd) & " -> " & Value(Nd));

      Attrs := Attributes (Nd);
      Lgt := Length(Attrs);
      for Index in 1..Lgt loop
         Tmp := Item(Attrs,Index-1);
         Put(" attribute #" & Integer'Image(Index) & ": ");
         Display_Node(Tmp);
      end loop;
      Get_Style(Nd,Props) ;

   end;

   procedure Display_All_Node (L : Node_List) is
      Nd : Node ;
   begin
      for Index in 1 .. Length (L) loop
         Nd := Item(L, Index-1);
         Display_Node(Nd);
      end loop;
   end ;

   procedure Display_All_Children (Nd : Node; Level: Natural:= 0) is
      L : constant Node_List := Child_Nodes(Nd);
      Tmp : Node ;
   begin
      for Index in 1 .. Length (L) loop
         Tmp := Item(L, Index-1);
         Put(Level * 3 * ' ' & Integer'Image(Index) & " => " );
         Display_Node(Tmp);
         Display_All_Children(Tmp, Level + 1);
      end loop;
   end ;

   procedure Extract_Html ( Source : in Unbounded_String ;
                            Target : in out Unbounded_String ) is
      -- L : Natural ;
      Start_Tag, End_Tag : Natural ;
      Text_trace: constant Boolean:= True;
   begin
      -- L := Length(Source);
      if Text_trace then
         Put_Line("************************* SOURCE ****************************") ;
         -- Put_Line(To_String(Source));
      end if;
      Start_Tag := Index(Source,"<html",1);
      End_Tag := Index(Source,"</html>",1);
      Target := Unbounded_Slice(Source, Start_Tag , End_Tag+6);
      if Text_trace then
	 Put_Line("************************* TARGET ****************************") ;
	 Put_Line(To_String(Target));
      end if;
   end Extract_Html;

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
      Get_Style(Doc) ;
   end Get_Xhtml_Content;

   --
   -- Recupere le style d'un document ... roxe
   --
   procedure Get_Style (Doc : Document) is
      List : Node_List ;
      Style : Node ;
      -- Style_Content : Unbounded_String ;
   begin
      List := Get_Elements_By_Tag_Name(Doc,"style");
      if Length(List) = 1 then
         Style := Item(List,0);
         List := Child_Nodes(Style) ;
         Style := Item(List,0);
         Put_Line("*********************** STYLE ******************************");
         Put_Line(Value(Style));
         Set_Css_Value (Value(Style)) ;
         Parse_Information ;
         Put_Line("*********************** STYLE (FIN) *************************");
      end if;
   end ;

   procedure Get_Style (Nd : in Node ; Props : in out Css_Properties.Vector) is
      A: Attr ;
   begin
      A := Get_Named_Item(Attributes(Nd),"style") ;
      -- Put_Line("Displaying style : " & Value(A)) ;
      Get_Css_Unit_Element(Value(A), Props);
   exception
      when others =>
        null; -- Put_Line("no style");
   end ;




end ;
