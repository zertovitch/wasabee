
with Ada.Text_IO                      ; use Ada.Text_IO                      ;
with Ada.Command_Line                 ; use Ada.Command_Line                 ;
with Ada.Streams                      ; use Ada.Streams                      ;

package body Wasabee.Net is
   function Is_An_IP_Address (Host : in String) return Boolean is
   begin
      if Host'Length < 7 or Host'Length > 15 Then
         return False;
      else
         for Index in Host'Range loop
            case Host (Index) is
               when '.' | '0' .. '9' =>
                  null;
               when others =>
                  return False;
            end case;
         end loop;
         return True;
      end if;
   end Is_An_IP_Address;

   function To_IP_Address (Host : in String) return Inet_Addr_Type is
   begin
      if Is_An_IP_Address (Host) then
         Put_Line("Address IP");
         return Inet_Addr(Host);
      else
         Put_Line("PAS Address IP");
         return Addresses (Get_Host_By_Name(Host)) ;
      end if;
   end To_IP_Address;

   procedure Get_Url (Base_Url : in out Unbounded_String ;
                      Extension_Url : in out Unbounded_String ;
                      Port : in out Port_Type) is
      package CLI renames Ada.Command_Line;
      Ac : constant Integer := CLI.Argument_Count;
   begin
      if Ac = 0 then
         Base_Url := To_Unbounded_String("127.0.0.1");
         Extension_Url := To_Unbounded_String("/") ;
      elsif Ac = 1 then
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String("/") ;
      elsif Ac = 2 then
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String(Argument(2));
      elsif Ac = 3 then
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String(Argument(2));
         Port := Port_Type'Value( Argument(3) ) ;
      end if;
   end;


   procedure Get_Http_Content (U : String ;
                               Content : in out Unbounded_String) is
      Adr : Unbounded_String := To_Unbounded_String(U);
      The_Url : Wasabee.URL.URL ;
      Port : Port_Type ;
   begin
      Decode(Adr, The_Url);
      if The_Url.Protocole /= "http" then
         Put_Line("Seul le protocole HTTP est supporté pour le moment");
         return ;
      end if;
      Port := Port_Type(The_Url.Port) ;
      Get_Http_Content ( To_String(The_Url.Host),
                         To_String(The_Url.Ressource),
                         Port,
                         content);
   end ;

   procedure Get_Http_Content (Base_Url : in String ;
                              Extension_Url : in String ;
                              Port : in Port_Type ;
                              Content : in out Unbounded_String) is
      Client       : Socket_Type ;
      Address      : Sock_Addr_Type;
      Channel      : Stream_Access ;
      Send         : constant String :=  (1 => ASCII.CR, 2 => ASCII.LF,
                                          3 => ASCII.CR, 4 => ASCII.LF);
      Offset       : Ada.Streams.Stream_Element_Count ;
      Data         : Ada.Streams.Stream_Element_Array (1 .. 1024) ;
   begin
      GNAT.Sockets.Initialize;
      Create_Socket(Client);
      Put_Line("Trying to get IP: " & Base_Url);
      Address.Addr := To_IP_Address(Base_URL) ;
      Address.Port := Port;
      Put_Line("Trying to open " & Base_Url & ", Port " & Port_Type'Image(Port));
      Connect_Socket (Client, Address);
      Channel := Stream(Client);

      Put_Line("Request: " & "GET " & Extension_Url & " HTTP/1.0" & Send) ;
      String'Write (Channel, "GET " & Extension_Url & " HTTP/1.0" & Send) ;
      -- Je ramene tout cela et je stocket dans une variable ...
      loop
         Ada.Streams.Read (Channel.all, Data, Offset) ;
         exit when Offset = 0;
         for I in 1 .. Offset loop
            Append(Content, Character'Val(Data(I)));
         end loop;
      end loop;
   end ;




end;



