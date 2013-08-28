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
with Wasabee.Url   ;          use Wasabee.Url ;
with Wasabee.Request   ;      use Wasabee.Request ;
with Wasabee.Hypertext;       use Wasabee.Hypertext;
with Wasabee.CSS ;            use Wasabee.CSS ;

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

   -- ho: HTML_object;
begin
   --
   -- Programme de test
   --
   Put_Line("Wasabee test line version 0.0.1") ;

   if Argument_Count = 0 then
      Open_Url ("http://localhost", xhtml) ;
   else
      Open_Url (Argument(1), Xhtml) ;
   end if ;

   N := Item(Xhtml,0);
   Display_Node(N);
   Display_All_Children(N);

   --
   -- Fin du programme de test
   --
   -- Put_Line("Body color : " & Get_CSS ("body","color")) ;
   -- Put_Line("Body background : " & Get_CSS ("body","background")) ;
   Free(Reader);

end ;
