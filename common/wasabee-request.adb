--
-- Programme de test avec les WebSockets et XML Ada
-- But du jeu arriver à ramener du XHTTP via du WebSocket
-- Puis le passer dans du XMLAda pour pouvoir le parser
-- Easy non ?
--

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Streams;             use type Ada.Streams.Stream_Element_Count;
with Ada.Command_Line ;       use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Task_Identification; use Ada.Task_Identification;

with GNAT.Sockets;            use GNAT.Sockets;
with Dom.Readers             ;use Dom.Readers                      ;

with Dom.Core                ;use Dom.Core                         ;
with Dom.Core.Documents     ; use Dom.Core.Documents                    ;
with Dom.Core.Nodes         ; use Dom.Core.Nodes                        ;
with Dom.Core.Attrs         ; use Dom.Core.Attrs                        ;

with Input_Sources.File;      use Input_Sources.File ;

with Wasabee;
with Wasabee.Net ;            use Wasabee.Net ;
with Wasabee.Xhtml ;          use Wasabee.Xhtml ;
with Wasabee.Url   ;             use Wasabee.Url ;

with Sax.Readers ;


package body Wasabee.Request is
   procedure Open_Url (The_Url : in String ; Nl : in out Node_List) is
      U : Wasabee.Url.URL ;
      Xhtml_Content : Unbounded_String ;
      Html_Content : Unbounded_String ;
      Reader : Tree_Reader ;
   begin
      Decode(To_Unbounded_String(The_Url), U) ;
      To_String(U);
      if U.Protocole = "http" then
         begin
            Get_Http_Content (The_Url, Html_Content) ;
            Extract_Html (Html_Content,Xhtml_Content);
            Get_Xhtml_Content (To_String(Xhtml_Content), Nl, Reader);
         exception
            when Error : Sax.Readers.XML_FATAL_ERROR =>
               Put_Line("This document is not an XHTML document");
               Abort_Task (Current_Task);
         end;
      elsif U.Protocole = "file" then
         declare
            Input : File_Input ;
            Reader : Tree_Reader ;
            Doc    : Document ;
         begin
            -- Put_Line("opening");
            Open(To_String(U.Ressource), Input);
            -- Put_Line("parsing");
            Parse(Reader, Input);
            -- Put_Line("closing stream");
            Close(Input);
            -- Put_Line("getting elements");
            Doc := Get_Tree(Reader);
            Nl  := Get_Elements_By_Tag_Name(Doc,"html");
            -- Free(Reader) ;
         exception
            when Error : Sax.Readers.XML_FATAL_ERROR =>
               Put_Line("This document is not an XHTML document");
               Abort_Task (Current_Task);
         end;
      end if ;
   end ;
end Wasabee.Request ;
