--
-- But du jeu
-- 1- Ramener du bout de code xhtml par HTTP
-- 2- Le passer dans XMLAda et faire en sorte que ca fonctionne
-- Enjoy Wasabee (or not)
--

with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Ada.Streams;

with Ada.Command_Line ; use Ada.Command_Line ; 

use type Ada.Streams.Stream_Element_Count;

procedure Main is
   Client  : Socket_Type;
   Address : Sock_Addr_Type;
   Channel : Stream_Access; 
   CRLF : constant String := ASCII.CR & ASCII.LF;
   Send   : String := (1 => ASCII.CR, 2 => ASCII.LF, 
                       3 => ASCII.CR, 4 => ASCII.LF);
   Data   : Ada.Streams.Stream_Element_Array (1 .. 256);
   Size    : Ada.Streams.Stream_Element_Offset;
   Ret     : Ada.Strings.Unbounded.Unbounded_String;
begin

   GNAT.Sockets.Initialize;  -- initialize a specific package
   Create_Socket (Client);
   Address.Addr := Addresses (Get_Host_By_Name (Argument(1)), 1);
   
   Address.Port := 80;

   Connect_Socket (Client, Address);
   Channel := Stream (Client);

   String'Write (Channel, "GET " & Argument(2) &" HTTP/1.1" & CRLF &
		   "Host: " & Argument(1) & CRLF & CRLF);
   
   loop 
      GNAT.Sockets.Receive_Socket(Client,Data,Size);
      Put_Line(Size'Img);

      for i in 1 .. Size loop
	 Ret := Ret & Character'Val(Data(i));
      end loop;
      exit when Size < 256 ;
   end loop;
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String(Ret));   
   -- Write it out to the console
end Main;
