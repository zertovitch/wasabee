--  !!  To do:
--  !!    Potential performance issue with Unbounded_String -
--           Try using a sequential parsing and String instead of Index / Unbounded_String
--  !!    Several elements can be defined with same properties: a, b, h1 : { ... }
--  !!    Leak on CSS_Properties_Map_Ptr's if CSS_Dictionary.Map is set as Empty_Map ?

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO ; use Ada.Text_IO ;

package body Wasabee.CSS is

   Blank: constant array(Character) of Boolean:=
     (' ' | Character'Val(9) | Character'Val(10) | Character'Val(13) => True,
     others => False);

   function Clean(Content : in Unbounded_String) return Unbounded_String is
      Tmp : Unbounded_String ;
      C : Character ;
      Quote: Boolean:= False;
   begin
      for I in 1..Length(Content) loop
         C := Element(Content,I) ;
         if C = '"' then
           Quote:= not Quote;
           Append(Tmp, C);
         elsif Quote then  -- Within quotes: let blanks as is.
           Append(Tmp, C);
         elsif Blank(C) then
           -- On ne prend pas les espaces et les tabulations
           null ;
         else
           -- Put_Line(C & ":" & Integer'Image(Character'Pos(C))) ;
           Append(Tmp, C);
         end if;
      end loop;
      return Tmp ;
   end Clean;

   procedure Read_CSS_File (S: String ; Css : in out Unbounded_String) is
      Filename : constant String := S ;
      File : File_Type ;
      Line_Count : Natural := 0 ;
   begin
      Ada.Text_IO.Open(File, In_File, Filename);
      while not End_Of_File (File) loop
         Line_Count := Line_Count + 1 ;
         -- Ada.Text_IO.Put_Line (Get_Line(File));
         Append(Css , Get_Line(File));
      end loop;
      Close(File);
   end Read_CSS_File;

   --  Parse a single element's style properties
   procedure Parse_CSS_Element (Map : in out CSS_Dictionary.Map ;
                                Element : String ;
                                Declarations : Unbounded_String) is
      Key : Unbounded_String ;
      Value : Unbounded_String ;
      -- Count : Natural := 0;
      Base  : Natural := 1;
      Aff_Pos : Natural ;
      Sep_Pos : Natural ;
      --  Selectors ".my_class" and "*.my_class" are equivalent. We store with the '*'.
      --  Same for ID and attribute selectors
      --  http://www.w3.org/TR/CSS2/selector.html#selector-syntax
      function Add_star(s: String) return String is
      begin
        if s'Length > 0 and then (s(s'First) = '.' or s(s'First) = '#' or s(s'First) = '[') then
          return '*' & s;
        else
          return s;
        end if;
      end Add_star;
      --  We need to remove blanks on both ends but not in the middle:
      --  Element can be ".table p.title b".
      Element_Trimmed: constant String:= Add_star(Simplify_Blanks(Element));
   begin
         if Element_Trimmed'Length = 0 then
            return;
         end if;
         -- Put_Line("Element: [" & Element_Trimmed & ']');
         --
         --  Example:  border-right: 1px black solid;
         --          ^base         ^aff_pos         ^sep_pos
         loop
            Aff_Pos := Index (Declarations, ":" , Base);
            exit when Aff_Pos = 0 ;
            Sep_Pos := Index (Declarations, ";" , Aff_Pos+1);

            if Sep_Pos = 0 then  --  No terminal ';' -> we invent it.
               Sep_Pos := Length(Declarations) + 1;
            end if;

            Key := Clean(Unbounded_Slice(Declarations, Base, Aff_Pos - 1));
            Value := Unbounded_Slice(Declarations, Aff_Pos + 1, Sep_Pos - 1) ;
            -- Put_Line("  Key: [" & To_String(Key) & ']');
            -- Put_Line("  Value: [" & To_String(Value) & ']');
            Add_Or_Replace_CSS_Property(
              Map ,
              Element_Trimmed,
              To_Upper(To_String(Key)) ,
              Simplify_Blanks(To_String(Value))
            );
            Base := Sep_Pos + 1 ;
         end loop ;
   end Parse_CSS_Element;

   --  Parse one or more elements' style properties
   --  Elements are separated by commas
   procedure Parse_CSS_Elements (Map : in out CSS_Dictionary.Map ;
                                 Elements : String ;
                                 Declarations : Unbounded_String) is
      Comma : Natural ;
   begin
      Comma  := Index (Elements, ",") ;
      if Comma = 0 then
         Parse_CSS_Element(Map, Elements, Declarations);
      else
         Parse_CSS_Element(Map, Elements(Elements'First .. Comma-1), Declarations);
         Parse_CSS_Elements(Map, Elements(Comma+1..Elements'Last), Declarations);
      end if;
   end Parse_CSS_Elements;

   procedure Parse_Information (Map : in out CSS_Dictionary.Map ;
                                Css : Unbounded_String) is
      Base : Natural := 1;
      Openbracket, Closebracket : Natural ;
      Elem : Unbounded_String ;
      Css_no_comments: Unbounded_String:= Css;
      Css_Content : Unbounded_String ;
   begin
      --  Clean comments first
      loop
         Openbracket  := Index (Css_no_comments, "/*") ;
         exit when Openbracket = 0 ;
         Closebracket := Index(Css_no_comments, "*/") ;
         if Closebracket = 0 then
           Closebracket := Length(Css_no_comments)-1;
         end if;
         if Closebracket >= Openbracket + 2 then
           Delete(Css_no_comments, Openbracket, Closebracket + 1);
         end if;
      end loop;
      --  Loop through the style sheet's elements
      loop
         Openbracket  := Index (Css_no_comments, "{", Base+1) ;
         exit when Openbracket = 0 ;
         Closebracket := Index(Css_no_comments, "}", Base+1) ;
         Elem := Unbounded_Slice(Css_no_comments, Base, Openbracket-1);

         --Put_Line("CSS Element:" & To_String(Elem));
         --Put_Line("Ouverture : " & Integer'Image(Base)) ;
         --Put_Line("Fermeture : " & Integer'Image(Closebracket)) ;
         Css_Content := Unbounded_Slice(Css_no_comments, Openbracket+1, Closebracket-1);
         --Put_Line("CSS Content:" & To_String(Css_Content));
         --  Css_Content := To_Unbounded_String(Simplify_Blanks(To_String(Css_Content)));
         Parse_Css_Elements (Map , To_Upper(To_String(Elem)), Css_Content);
         if Verbosity > 0 then
            New_Line ;
         end if;
         Base := Closebracket+1 ;
      end loop;
   end Parse_Information;

   --
   -- Les deux fonctions phares de Wasabee pour le CSS
   --

   procedure Add_Or_Replace_CSS_Property (Map     : in out CSS_Dictionary.Map ;
                                          Element : String ;
                                          Key     : String ;
                                          Value   : String)
   is
      Ptr : CSS_Properties_Map_Ptr ;
      UKey, UValue: Unbounded_String;
   begin
      if Verbosity > 0 then
         Put_Line("[ADD_OR_REPLACE_CSS] [" & Element & "] : [" & Key & "] => [" & Value & ']');
      end if;
      --
      -- Je cherche si il existe un tableau de propriétés pour cet élément
      --
      if CSS_Dictionary.Contains(Map, To_Unbounded_String(Element)) then
         -- Put_Line("[ADD_OR_REPLACE_CSS] Properties exist ..");
         Ptr := CSS_Dictionary.Element(Map, To_Unbounded_String(Element)) ;
      else
         -- Put_Line("[ADD_OR_REPLACE_CSS] Properties to create ..");
         Ptr := new CSS_Properties.Map;
         CSS_Dictionary.Insert(Map, To_Unbounded_String(Element), Ptr);
      end if ;

      --
      -- Ok on a un container
      --

      UKey:= To_Unbounded_String(Key);
      UValue:= To_Unbounded_String(Value);
      if CSS_Properties.Contains(Ptr.all, UKey) then
         -- Put_Line("  Replacing Element") ;
         CSS_Properties.Replace( Ptr.all, UKey, UValue ) ;
      else
         -- Put_Line("  Adding Element") ;
         CSS_Properties.Insert( Ptr.all, UKey, UValue ) ;
      end if;

   exception
      when others =>
         null ;
   end Add_Or_Replace_CSS_Property;

   function Exists_CSS_Element(Map : CSS_Dictionary.Map ; Element : String) return Boolean is
   begin
     -- Put_Line("[Exists_CSS_Element] Searching element [" & Element & ']') ;
     return CSS_Dictionary.Contains(Map, To_Unbounded_String(Element));
   end Exists_CSS_Element;

   function  Get_CSS_Property (Map     : CSS_Dictionary.Map ;
                               Element : String ;
                               Key     : String) return String
   is
      Ptr : CSS_Properties_Map_Ptr ;
   begin
      --
      --  Looking for a property map for this element
      --
      if Exists_CSS_Element(Map, Element) then
         Ptr := CSS_Dictionary.Element(Map, To_Unbounded_String(Element)) ;
      else
         if Verbosity > 0 then
            Put_Line("[GETTING CSS] Properties for element '" & Element & "' do not exist");
         end if;
         return "" ;
      end if ;

      if CSS_Properties.Contains(Ptr.all , To_Unbounded_String(Key)) then
         declare
           Val: constant String:= To_String(CSS_Properties.Element(Ptr.all, To_Unbounded_String(Key)));
         begin
           if Verbosity > 0 then
              Put_Line("[GETTING CSS] Property '" & Key & "' for element '" & Element & "' is '" & Val &''');
           end if;
           return Val;
         end;
      else
         if Verbosity > 0 then
            Put_Line("[GETTING CSS] Property '" & Key & "' for element '" & Element & "' does not exist ");
         end if;
         return "" ;
      end if ;

   end Get_CSS_Property;

   function Simplify_Blanks(S: String) return String is
     T: String(S'Range);
     j: Integer:= T'First-1;
     in_blank: Boolean:= True;
   begin
     for i in S'Range loop
       if Blank(S(i)) then
         if in_blank then
           null;  --  Skip
         else
           j:= j + 1;
           T(j):= ' ';
         end if;
         in_blank:= True;
       else
         j:= j + 1;
         T(j):= S(i);
         in_blank:= False;
       end if;
     end loop;
     if j >= T'First and then Blank(T(j)) then  --  A last space to trim
       return T(T'First..j-1);
     else
       return T(T'First..j);
     end if;
   end Simplify_Blanks;

end Wasabee.CSS;
