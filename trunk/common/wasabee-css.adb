package body Wasabee.Css is
   function Clean(Content : in Unbounded_String) return Unbounded_String is
      Tmp : Unbounded_String ;
      C : Character ;
   begin

      for I in 1..Length(Content) loop
         C := Element(Content,I) ;
         -- On ne prend pas les espaces
         -- et les tabulations
         if Character'Pos(C) = 32 then
            null ;
         elsif Character'Pos(C) = 9 then
            null ;
         elsif Character'Pos(C) = 10 then
            null ;
         else
            -- Put_Line(C & ":" & Integer'Image(Character'Pos(C))) ;
            Append(Tmp, C);
         end if;
      end loop;
      return Tmp ;
   end;

   function Clean_Key(Content : in Unbounded_String) return Unbounded_String is
      Tmp : Unbounded_String ;
      C : Character ;
   begin

      for I in 1..Length(Content) loop
         C := Element(Content,I) ;
         -- On ne prend pas les espaces
         -- et les tabulations

         if Character'Pos(C) = 9 then
            null ;
         elsif Character'Pos(C) = 10 then
            null ;
         else
            -- Put_Line(C & ":" & Integer'Image(Character'Pos(C))) ;
            Append(Tmp, C);
         end if;

      end loop;
      return Tmp ;
   end;

   procedure Read_CSS_File (S: String) is
      Filename : String := S ;
      File : File_Type ;
      Line_Count : Natural := 0 ;
   begin
      Ada.Text_IO.Open(File, In_File, Filename);
      while not End_Of_File (File) loop
         Line_Count := Line_Count + 1 ;
         -- Put_Line (Natural'Image(Line_Count) & ": " & Line) ;
         Append(Css , Get_Line(File));
      end loop;
      Close(File);
   end;

   procedure Parse_CSS_Element (Element : Unbounded_String) is
      Key : Unbounded_String ;
      Value : Unbounded_String ;
      Count : Natural := 0;
      Base  : Natural := 1;
      Aff_Pos : Natural ;
      Sep_Pos : Natural ;
   begin
      -- Je me fais ca a la sauvage un peu ...
      loop
         Aff_Pos := Index (Element, ":" , Base);
         exit when Aff_Pos = 0 ;
         Sep_Pos := Index (Element, ";" , Aff_Pos+1);
         Key := Unbounded_Slice(Element, Base, Aff_Pos - 1) ;
         Value := Unbounded_Slice(Element, Aff_Pos + 1, Sep_Pos - 1) ;
         Put_Line("Key:" & To_String(Key));
         Put_Line("Value:" & To_String(Value));
         Base := Sep_Pos + 1 ;
      end loop ;
   end ;

   procedure Parse_Information is
      Base : Natural := 1;
      Openbracket, Closebracket : Natural ;
      Key : Unbounded_String ;
      Css_Content : Unbounded_String ;
   begin
      loop
         Openbracket  := Index (Css, "{", Base+1) ;
         exit when Openbracket = 0 ;
         Closebracket := Index(Css, "}", Base+1) ;
         Key := Unbounded_Slice(Css, Base, Openbracket-1);

         Key := Clean_Key(Key);

         -- Put_Line("CSS Element:" & To_String(Key));
         -- Put_Line("Ouverture : " & Integer'Image(Base)) ;
         -- Put_Line("Fermeture : " & Integer'Image(Closebracket)) ;
         Css_Content := Unbounded_Slice(Css, Openbracket+1, Closebracket-1);
         -- Put_Line("CSS Content:" & To_String(Css_Content));
         Css_Content := Clean(Css_Content);
         Parse_Css_Element (Css_Content);
         New_Line ;
         Base := Closebracket+1 ;
      end loop;
   end;

   function Get_CSS_Value (Object : String ; Key : String) return String is
   begin
      return "" ;
   end ;

   procedure Set_CSS_Value ( Content : String) is
   begin
      CSS := To_Unbounded_String(Content) ;
   end;

   procedure Get_Css_Unit_Element (Content : String ;
                                   Props : in out Css_Properties.Vector) is
      Base : Natural := 1;
      Openbracket, Closebracket : Natural ;
      Key : Unbounded_String := To_Unbounded_String(Content) ;
   begin
      Closebracket := Index(Css, "}", Base+1) ;
      Key := Clean_Key(Key);
      -- Put_Line("CSS Element:" & To_String(Key));
      -- Put_Line("Ouverture : " & Integer'Image(Base)) ;
      -- Put_Line("Fermeture : " & Integer'Image(Closebracket)) ;
      -- Put_Line("CSS Content:" & To_String(Key));
      Parse_Css_Element (key);
      New_Line ;
   end ;

end ;
