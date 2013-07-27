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
   CRLF    : constant String := ASCII.CR & ASCII.LF;
   Send    : constant String := CRLF & CRLF;
   Max_Size: constant:= 256;
   Data    : Ada.Streams.Stream_Element_Array (1 .. Max_Size);
   Size    : Ada.Streams.Stream_Element_Offset;
   Data_Str: String(1 .. Max_Size);
   Ret     : Ada.Strings.Unbounded.Unbounded_String;
begin

   GNAT.Sockets.Initialize;  -- initialize a specific package - Obsolescent !!
   Create_Socket (Client);
   Address.Addr := Addresses (Get_Host_By_Name (Argument(1)), 1);
   
   Address.Port := 80;

   Connect_Socket (Client, Address);
   Channel := Stream (Client);

   String'Write (Channel, "GET " & Argument(2) &" HTTP/1.1" & CRLF &
                 "Host: " & Argument(1) & Send);
   
   loop 
      GNAT.Sockets.Receive_Socket(Client,Data,Size);
      Put_Line(Size'Img);

      for i in 1 .. Size loop
	 Data_Str(Integer(i)):= Character'Val(Data(i));
      end loop;
      Ret:= Ret & Data_Str(1..Integer(Size));
      exit when Size < Max_Size ;
   end loop;
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String(Ret));   
   -- Write it out to the console
end Main;
