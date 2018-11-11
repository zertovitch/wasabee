
with Ada.Text_IO                      ; use Ada.Text_IO                      ;
with Ada.Command_Line                 ; use Ada.Command_Line                 ;
with Wasabee.Url ; use Wasabee.Url ;
with Wasabee.Util;                      use Wasabee.Util;
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
         -- Put_Line("Address IP");
         return Inet_Addr(Host);
      else
         -- Put_Line("PAS Address IP");
         return Addresses (Get_Host_By_Name(Host)) ;
      end if;
   end To_IP_Address;

   procedure Get_URL (Base_Url      : out Unbounded_String ;
                      Extension_Url : out Unbounded_String ;
                      Port          : out Port_Type) is
      package CLI renames Ada.Command_Line;
      Ac : constant Integer := CLI.Argument_Count;
   begin
      case Ac is
        when 0 =>
         Base_Url := To_Unbounded_String("127.0.0.1");
         Extension_Url := To_Unbounded_String("/") ;
        when 1 =>
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String("/") ;
        when 2 =>
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String(Argument(2));
        when others =>
         Base_Url := To_Unbounded_String(Argument(1));
         Extension_Url := To_Unbounded_String(Argument(2));
         Port := Port_Type'Value( Argument(3) ) ;
      end case;
   end Get_URL;


   procedure Get_HTTP_Content (U : String ;
                               Content : out Unbounded_String) is
      Adr : constant Unbounded_String := To_Unbounded_String(U);
      The_Url : Wasabee.URL.Split_URL ;
      Port : Port_Type ;
   begin
      Decode(Adr, The_Url);
      if The_Url.Protocole /= "http" then
         -- Put_Line("Seul le protocole HTTP est supporté pour le moment");
         return ;
      end if;
      Port := The_Url.Port;
      Get_HTTP_Content ( To_String(The_Url.Host),
                         To_String(The_Url.Ressource),
                         Port,
                         content);
   end Get_HTTP_Content;

   procedure Get_HTTP_Content (Host : in String ;
			       Resource : in String ;
			       Port : in Port_Type ;
			       Content : out Unbounded_String) is
      Client       : Socket_Type ;
      Address      : Sock_Addr_Type;
      Channel      : Stream_Access ;
      Send         : constant String :=  (1 => ASCII.CR, 2 => ASCII.LF);
      Offset       : Ada.Streams.Stream_Element_Count ;
      Data_Size    : constant:= 1024;
      Data         : Ada.Streams.Stream_Element_Array (1 .. Data_Size) ;
      Data_String  : String(1 .. Data_Size);
      request      : constant String:= 
                       "GET " & Resource & " HTTP/1.1" & 
                       Send & "Host: " & Host & Send & "User-Agent: Wasabee/" & Version_number &
                       Send & Send;
   begin
      -- GNAT.Sockets.Initialize; -- obsolete
      Create_Socket(Client);
      Put_Line("Trying to get IP: " & Host);
      Address.Addr := To_IP_Address(Host) ;
      Address.Port := Port;
      Put_Line("Trying to open " & Host & ", Port " & Port_Type'Image(Port));
      Connect_Socket (Client, Address);
      Channel := Stream(Client);

      -- Put_Line("Request: " & "GET " & Extension_Url & " HTTP/1.0" & Send & Send) ;
      -- String'Write (Channel, "GET " & Extension_Url & " HTTP/1.0" & Send & Send) ;
      
      Put_Line("Request: " & request);
      String'Write (Channel, request);      
      
      Content:= Null_Unbounded_String;

      -- Je ramene tout cela et je stocket dans une variable ...
      loop
         Put('.');
         Ada.Streams.Read (Channel.all, Data, Offset) ;
         exit when Offset = 0;
         for I in 1 .. Offset loop
            Data_String(Integer(I)):= Character'Val(Data(I));
         end loop;
         Append(Content, Data_String(1 .. Integer(Offset)));
      end loop;    
      Put_Line(" - Done");
 
      Close_Socket(Client);
   end Get_HTTP_Content;

end Wasabee.Net;



