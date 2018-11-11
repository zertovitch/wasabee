with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO ; use Ada.Text_IO ;

package body Wasabee.URL is

   procedure Display_URL_details (U : in Split_URL) is
   begin
      if Verbosity > 0 then
         Put_Line("Protocole : " & To_String(U.Protocole));
         Put_Line("Host      : " & To_String(U.Host));
         Put_Line("Port      : " & Port_Type'Image(U.Port));
         Put_Line("Ressource : " & To_String(U.Ressource));
      end if;
   end Display_URL_details;

   procedure Decode (Adr : in Unbounded_String ; U : in out Split_URL) is
      -- No_Protocole : Boolean ;
      -- No_Port : Boolean ;

      Pos_Protocole   : Natural ;
      Pos_Port        : Natural ;
      Pos_First_Slash : Natural ;
      
      Pos_SS          : Natural ;
      
      Base : Natural ;
      Last : Natural ;
      
      Cpy_Adr : Unbounded_String ;
   begin
      
      Pos_SS := Index(Adr, "//" , 1);
      if Pos_SS = 1 then
	       Cpy_Adr := To_Unbounded_String("http:") & Adr;
      else
	       Cpy_Adr := Adr ;
      end if ;
      
      Last := Length(Cpy_Adr);

      Pos_Protocole := Index(Cpy_Adr, "://" , 1);
      -- Put_Line("protocole: " & Natural'Image(Pos_Protocole));
      if Pos_Protocole = 0 then
         -- No_Protocole := True ;
         U.Protocole := To_Unbounded_String("http") ;
         Base := 1;
      else
         U.Protocole := Unbounded_Slice (Cpy_Adr, 1, Pos_Protocole-1);
         -- No_Protocole := False ;
         Base := Pos_Protocole + 3 ;
      end if ;

      if U.Protocole = "file" then
         U.host := To_Unbounded_String("-- localhost - unused --") ;
         U.Ressource := Unbounded_Slice (Cpy_Adr, Base, Last);
         U.Port := 0 ;
         return ;
      end if;

      Pos_Port      := Index(Cpy_Adr, ":" , Base);
      -- Put_Line("port     : " & Natural'Image(Pos_Port));

      Pos_First_Slash := Index(Cpy_Adr, "/" , Pos_Protocole+4);
      -- Put_Line("first slash: " & Natural'Image(Pos_First_Slash));

      if 0 < Pos_First_Slash and Pos_First_Slash < Pos_Port then
        Pos_Port:= 0;
        --  First ':' after protocol is after first '/'.
        --  Then that ':' doesn't designate a port, like http://www.gnoga.com:8080/ , but
        --  is part of some query like
        --  http://fonts.googleapis.com/css?family=Lato:400,400italic,700,700italic
      end if;

      if Pos_Port = 0 then
         -- No_Port := True ;
         U.Port := 80 ;
         if Pos_First_Slash = 0 then
            U.Host := Unbounded_Slice ( Cpy_Adr, Base, Last ) ;
            U.Ressource := To_Unbounded_String("/index.html");
         else
            U.Host := Unbounded_Slice ( Cpy_Adr, Base, Pos_First_Slash-1 ) ;
            U.Ressource := Unbounded_Slice ( Cpy_Adr, Pos_First_Slash, Last ) ;
         end if ;
      else
         -- No_Port := False ;
         if Pos_First_Slash = 0 then
            U.Host := Unbounded_Slice ( Cpy_Adr, Base, Pos_Port-1 ) ;
            U.Port := Port_Type'Value ( To_String ( Unbounded_Slice ( Cpy_Adr , Pos_Port+1 , last ) ) ) ;
            U.Ressource := To_Unbounded_String("index.html");
         else
            if Verbosity > 0 then
               Put_Line("W.URL.Decode with : (" & Pos_Port'img & ") and / (" & Pos_First_Slash'img & ") " & To_String(Cpy_Adr));
            end if;
            U.Host := Unbounded_Slice ( Cpy_Adr, Base, Pos_Port-1 ) ;
            U.Port := Port_Type'Value ( To_String ( Unbounded_Slice ( Cpy_Adr , Pos_Port+1 , Pos_First_Slash-1 ) ) ) ;
            U.Ressource := Unbounded_Slice ( Cpy_Adr, Pos_First_Slash, Last ) ;
         end if ;
      end if ;
   end Decode;

  function Build_URL (complete_URL, partial_URL: String) return String is
    first_slash: Integer:= complete_URL'Last + 1;   -- first /
    last_slash : Integer:= complete_URL'First - 1;  -- last /
    protocol   : Integer:= complete_URL'First - 1;  -- //
  begin
    if partial_URL'Length = 0 then
      return complete_URL; -- partial URL is empty: well, use the complete one...
    elsif Index(partial_URL, "://") > 0 then
      return partial_URL;  -- partial URL is actually a complete one: use it!
    end if;
    for i in complete_URL'Range loop
      if complete_URL(i) = '/' then
        if last_slash = i - 1 and protocol < complete_URL'First then
          protocol:= last_slash; -- capture the first //
        end if;
        last_slash:= i;
      end if;
    end loop;
    if protocol < complete_URL'First then
      raise Build_URL_error with
        "In complete_URL, the string ""//"" is missing, something like ""http://"". " &
        "complete_URL=" & complete_URL;
    end if;
    if Index(partial_URL, "//") = partial_URL'First then
      -- case [1]: absolute-with-website path given as the partial URL
      return complete_URL(complete_URL'First..protocol-1) & partial_URL;
    end if;
    for i in reverse protocol + 2 .. complete_URL'Last loop
      if complete_URL(i) = '/' then
        first_slash:= i;
      end if;
    end loop;
    case partial_URL(partial_URL'First) is
      when '/' =>
        -- case [2]: absolute path given as the partial URL
        return complete_URL(complete_URL'First..first_slash-1) & partial_URL;
      when '#' =>
        -- case [3]: partial URL is just an anchor
        return complete_URL & partial_URL;
      when others =>
        -- case [4]: relative path
        return complete_URL(complete_URL'First..last_slash) & partial_URL;
    end case;
  end Build_URL;

  function Simplify_URL(s: String) return String is
    s2: constant String(s'First..s'Last+2):= s & "XX";
    i: Positive:= s'First;
    t: String(s'Range):= s;
    j: Natural:= t'First-1;
  begin
    if s'Length >= 7 and then s(s'First .. s'First + 6) = "file://" then
      return s;
    end if;
    while i <= s'Last loop
      if s2(i .. i + 1) = "./" then
        i:= i + 2; -- skip the "./"
      elsif s2(i .. i + 2) = "../" then
        if j < t'First or else t(j) /= '/' then
          null; -- "../" is misplaced
        else
          j:= j - 1; -- "delete" the '/' in t
          while j >= t'First and then t(j) /= '/' loop
            j:= j - 1;
          end loop;
          i:= i + 3; -- skip the "../"
        end if;
      else
        j:= j + 1;
        t(j):= s(i);
        i:= i + 1;
      end if;
    end loop;
    return t(t'First .. j);
  end Simplify_URL;

  function Remove_anchor(s: String) return String is
  begin
    for i in reverse s'Range loop
      case s(i) is
        when '#' =>
          return s(s'First .. i-1);
        when '/' =>
          return s; -- an eventual # would be part of the address
        when others =>
          null; -- continue back
      end case;
    end loop;
    return s;
  end Remove_anchor;

  function Anchor_only(s: String) return String is
  begin
    for i in reverse s'Range loop
      case s(i) is
        when '#' =>
          return s(i+1 .. s'Last);
        when '/' =>
          return ""; -- an eventual # would be part of the address
        when others =>
          null; -- continue back
      end case;
    end loop;
    return "";
  end Anchor_only;

   procedure Test (U : String) is
      Tmp : Unbounded_String ;
      Trg : Split_URL ;
   begin
      Put_Line("*******************************************************");
      Put_Line("Decoding: " & U);
      Put_Line("*******************************************************");
      Tmp := To_Unbounded_String(U);
      Decode(Tmp, Trg);
      Display_URL_details(Trg);
   end ;

   procedure General_Test is
   begin
      Test("localhost");
      Test("destination");
      Test("destination.com");
      Test("http://destination.com");
      Test("http://www.destination.com");
      Test("https://destination.com");
      Test("http://destination.com:8080");
      Test("http://destination.com:8080/toto.xml");
      Test("http://destination.com:8080/toto/toto.xml");

      Test("file://toto.txt");
      Test("file://c:\tmp\toto.html");

   end;

end ;
