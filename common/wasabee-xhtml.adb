with Dom.Core.Documents           ; use Dom.Core.Documents                    ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes                        ;
with Dom.Core.Attrs               ; use Dom.Core.Attrs                        ;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO ; use Ada.Text_IO ;
with input_sources.Strings        ; use input_sources.Strings            ;

with Unicode.CES.Utf8        ;

package body Wasabee.Xhtml is

   -- N                : Node ;
   -- Http_Header      : Node ;
   -- Http_Body        : Node ;

   procedure Display_Node (Nd : Node) is
      Attrs : Named_Node_Map ;
      Lgt   : Natural ;
      Tmp   : Node;
      -- Props : Css_Properties.Vector;
   begin
      Put_Line(Node_Name(Nd) & " -> " & Value(Nd));

      Attrs := Attributes (Nd);
      Lgt := Length(Attrs);
      for Index in 1..Lgt loop
         Tmp := Item(Attrs,Index-1);
         Put(" attribute #" & Integer'Image(Index) & ": ");
         Display_Node(Tmp);
      end loop;
      -- Get_Style(Nd,Props) ;

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
      Text_trace: constant Boolean:= False;
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
      -- Get_Document_Style(Doc) ;
   end Get_Xhtml_Content;

   --
   -- Recupere le style d'un document ... roxe
   --
   procedure Get_Document_Style (Doc : Document ;
                                 Map : in out CSS_Dictionary.Map) is
      List : Node_List ;
      Style : Node ;
      -- Style_Content : Unbounded_String ;
      -- Map : CSS_Dictionary.Map ;
   begin
      List := Get_Elements_By_Tag_Name(Doc,"style");
      if Length(List) = 1 then
         Style := Item(List,0);
         List := Child_Nodes(Style) ;
         Style := Item(List,0);
         Put_Line("*********************** STYLE ******************************");
         Put_Line(Value(Style));
         Parse_Information(Map, To_Unbounded_String(Value(Style))) ;
         Put_Line("*********************** STYLE (FIN) *************************");
      end if;
   end ;

   procedure Get_Node_Style (Nd : in Node) is

   begin
      null;
   end ;


end ;
