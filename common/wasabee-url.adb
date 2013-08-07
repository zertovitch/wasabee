with Ada.Text_IO ; use Ada.Text_IO ;

package body Wasabee.URL is

   procedure To_String (U : in URL) is
   begin
      Put_Line("protocole : " & To_String(U.Protocole));
      Put_Line("host      : " & To_String(U.Host));
      Put_Line("Port      : " & Natural'Image(U.Port));
      Put_Line("Ressource : " & To_String(U.Ressource));
   end ;

   procedure Decode (Adr : in Unbounded_String ; U : in out URL) is
      No_Protocole : Boolean ;
      No_Port : Boolean ;

      Pos_Protocole   : Natural ;
      Pos_Port        : Natural ;
      Pos_First_Slash : Natural ;

      Base : Natural ;
      Last : Natural ;
   begin

      Last := Length(Adr);

      Pos_Protocole := Index(Adr, "://" , 1);
      -- Put_Line("protocole: " & Natural'Image(Pos_Protocole));
      if Pos_Protocole = 0 then
         No_Protocole := True ;
         U.Protocole := To_Unbounded_String("http") ;
         Base := 1;
      else
         U.Protocole := Unbounded_Slice (Adr, 1, Pos_Protocole-1);
         No_Protocole := False ;
         Base := Pos_Protocole + 3 ;
      end if ;

      if U.Protocole = "file" then
         U.host := To_Unbounded_String("-- localhost - unused --") ;
         U.Ressource := Unbounded_Slice (Adr, Base, Last);
         U.Port := 0 ;
         return ;
      end if;


      Pos_Port      := Index(Adr, ":" , Base);
      -- Put_Line("port     : " & Natural'Image(Pos_Port));

      Pos_First_Slash := Index(Adr, "/" , Pos_Protocole+4);
      -- Put_Line("first slash: " & Natural'Image(Pos_First_Slash));

      if Pos_Port = 0 then
         No_Port := True ;
         U.Port := 80 ;
         if Pos_First_Slash = 0 then
            U.Host := Unbounded_Slice ( Adr, Base, Last ) ;
            U.Ressource := To_Unbounded_String("/index.html");
         else
            U.Host := Unbounded_Slice ( Adr, Base, Pos_First_Slash-1 ) ;
            U.Ressource := Unbounded_Slice ( Adr, Pos_First_Slash, Last ) ;
         end if ;
      else
         No_Port := False ;
         if Pos_First_Slash = 0 then
            U.Host := Unbounded_Slice ( Adr, Base, Pos_Port-1 ) ;
            U.Port := Natural'Value ( To_String ( Unbounded_Slice ( Adr , Pos_Port+1 , last ) ) ) ;
            U.Ressource := To_Unbounded_String("index.html");
         else
            U.Host := Unbounded_Slice ( Adr, Base, Pos_Port-1 ) ;
            U.Port := Natural'Value ( To_String ( Unbounded_Slice ( Adr , Pos_Port+1 , Pos_First_Slash-1 ) ) ) ;
            U.Ressource := Unbounded_Slice ( Adr, Pos_First_Slash, Last ) ;
         end if ;
      end if ;
   end;

   procedure Test (U : String) is
      Tmp : Unbounded_String ;
      Trg : URL ;
   begin
      Put_Line("*******************************************************");
      Put_Line("Decoding: " & U);
      Put_Line("*******************************************************");
      Tmp := To_Unbounded_String(U);
      Decode(Tmp, Trg);
      To_String(Trg);
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