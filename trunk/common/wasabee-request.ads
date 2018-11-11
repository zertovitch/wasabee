with GNAT.Sockets;

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Wasabee.Request is

   --------------------------------------
   -- Connection type for any protocol --
   --------------------------------------

   type Connection is abstract tagged limited null record;

   type p_Connection is access Connection'Class;

   procedure Close(c: in out Connection) is abstract;

   type File_Connection is new Connection with record
     file: Ada.Streams.Stream_IO.File_Type;
   end record;

   procedure Close(c: in out File_Connection);

   type HTTP_Connection is new Connection with record
     client: GNAT.Sockets.Socket_Type;
   end record;

   procedure Close(c: in out HTTP_Connection);

   -- Blocking retrieval of any data
   procedure Retrieve_from_URL (The_URL : in String ; Data : out Unbounded_String) ;

end ;

