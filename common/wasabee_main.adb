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

with GNAT.Sockets;           use GNAT.Sockets;
with Dom.Readers             ; use Dom.Readers                      ;

with Dom.Core                ; use Dom.Core                         ;
with Dom.Core.Documents           ; use Dom.Core.Documents                    ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes                        ;
with Dom.Core.Attrs               ; use Dom.Core.Attrs                        ;

with Wasabee;
with Wasabee.Net ;           use Wasabee.Net ;
with Wasabee.Xhtml ;         use Wasabee.Xhtml ;

with Sax.Readers ;

procedure Wasabee_Main is
   --
   -- Je vais mettre tout ce qui concerne les GNAT Socket Ici pour ranener l'information
   --
   Base_Url : Unbounded_String ;
   Extension_Url : Unbounded_String ;
   Port : Port_Type := 80;
   Html_Content : Unbounded_String ;
   Xhtml_Content : Unbounded_String ;
   --
   -- Et tout ce qui concerne le DOM la
   --
   Xhtml : Node_List ;
   Reader : Tree_Reader ;
   N : Node;

begin
   --
   -- Programme de test
   --
   Put_Line("Wasabee test line version 0.0.1") ;

   if Argument_Count = 1 then
      Get_Http_Content (Argument(1), Html_Content) ;
   else
      Get_Url (Base_Url, Extension_Url , Port) ;
      begin
         Get_Http_Content (To_String(Base_Url),
                           To_String(Extension_Url) ,
                           Port,
                           Html_Content) ;
      exception
         when Error : GNAT.Sockets.Socket_Error =>
            begin
               Put_Line ("Unknown Address");
               Abort_Task (Current_Task);
            end ;
      end;
   end if ;


   -- Put_Line("********************* EXTRACTING *************************");
   -- Put_Line (To_String(html_Content));
   Extract_Html (Html_Content,Xhtml_Content);
   -- Put_Line (To_String(xhtml_Content));

   --
   -- Envoyer cela dans SAX
   --
   begin
      Get_Xhtml_Content (To_String(Xhtml_Content), Xhtml, Reader);
   exception
      when Error : Sax.Readers.XML_FATAL_ERROR =>
         Put_Line("This document is not an XHTML document");
         Abort_Task (Current_Task);
      -- when others =>
         -- Put_Line("Unknown error");
   end;

   --
   -- Maintenant on peut faire joujou avec le XHTML
   --

   --
   -- do something
   --

   N := Item(Xhtml,0);
   Display_Node(N);
   Display_All_Children(N);

   --
   -- Fin du programme de test
   --
   Free(Reader);

end ;
