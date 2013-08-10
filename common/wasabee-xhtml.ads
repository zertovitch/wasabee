with Ada.Strings.Unbounded        ; use Ada.Strings.Unbounded ;

-- with unicode.Ces                  ; use unicode.Ces                      ;
with unicode.ccs.Iso_8859_1       ; use unicode.ccs.Iso_8859_1           ;

with input_sources.Strings        ; use input_sources.Strings            ;

with Dom.Readers                  ; use Dom.Readers                      ;
with Dom.Core                     ; use Dom.Core                         ;
with Dom.Core.Documents           ; use Dom.Core.Documents                    ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes                        ;
with Dom.Core.Attrs               ; use Dom.Core.Attrs                        ;

package Wasabee.Xhtml is

   procedure Display_Node (Nd : Node) ;

   procedure Display_All_Node (L : Node_List) ;

   procedure Display_All_Children (Nd : Node; Level: Natural:= 0) ;

   procedure Get_Xhtml_Content (Content : String ;
                                List : in out Node_List;
                                Reader : in out Tree_Reader) ;

   procedure Extract_Html ( Source : in Unbounded_String ;
                            Target : in out Unbounded_String ) ;

   procedure Get_Style (Doc : Document) ;

end ;
