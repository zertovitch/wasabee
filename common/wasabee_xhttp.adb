--
-- But du jeu. parser un document XHTTP
--

with Wasabee.Xhtml ; use Wasabee.Xhtml ;

with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ; 

with Unicode.CES.Utf8        ;
with unicode.Ces             ; use unicode.Ces                      ;
with unicode.ccs.Iso_8859_1  ; use unicode.ccs.Iso_8859_1           ;
with input_sources.Strings   ; use input_sources.Strings            ;
with Dom.Readers             ; use Dom.Readers                      ;
with Dom.Core                ; use Dom.Core                         ;
with Dom.Core.Documents           ; use Dom.Core.Documents                    ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes                        ;
with Dom.Core.Attrs               ; use Dom.Core.Attrs                        ;

procedure Wasabee_Xhttp is
   Sample_String : String := "<html><header></header><body><p id='para'>Bonjour le monde</p></body></html>" ;
   
   Content : Unbounded_String ;
      
   List             : Node_List ;
   N                : Node ;
   Reader           : Tree_Reader ;
begin
   
   Content := To_Unbounded_String("<html><body><h1>It works!</h1>") ;
   Content := Content & ("<p>This is the default web page for this server.</p>") ;
   Content := Content & ("<p>The web server software is running but no content has been added, yet.</p>") ;
   Content := Content & ("</body></html>") ;
   
   -- Content := To_Unbounded_String("<html></html>");
   
   Get_Xhtml_Content(To_String(Content), List, Reader);


   N := Item(List,0);
   Display_Node(N);
   Display_All_Children(N);
   Free(Reader);
end ;




