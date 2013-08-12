--
--
--


with Ada.Text_IO                  ; use Ada.Text_IO ;
with Ada.Strings.Unbounded        ; use Ada.Strings.Unbounded;

with Dom.Core                     ; use Dom.Core ;
with Dom.Core.Elements            ; use Dom.Core.Elements ;
with Dom.Core.Nodes               ; use Dom.Core.Nodes ; 
with Dom.Core.Attrs               ; use Dom.Core.Attrs ;

with Wasabee.Request   ;      use Wasabee.Request ;
with Wasabee.Xhtml ;          use Wasabee.Xhtml ;
with Wasabee.Url   ;          use Wasabee.Url ;
with Wasabee.Net   ;          use Wasabee.Net ;
with Wasabee.Images ;         use Wasabee.Images ;

with GNAT.Sockets ; use GNAT.Sockets ;
with Interfaces; use Interfaces ;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with Ada.Streams                      ; use Ada.Streams                      ;
with GID ;

procedure Wasabee_Image is
   Xhtml, Nl : Node_List ;
   Xhtml_Content : Unbounded_String ;
   N : Node;
   Img : GID.Image_Descriptor ;
   
begin
   Put_Line("Wasabee, test image programme version 0.0.1");
   Open_Url("file://..\tests\example_img.html",xhtml);
   N := Item(Xhtml,0);
   
   -- Ok essayons de chopper le bon attribut maintenant ...
   
   Nl := Get_Elements_By_Tag_Name(N,"body");
   N  := Item(Nl,0);
   
   Nl := Get_Elements_By_Tag_Name(N,"img");
   N  := Item(Nl,0);
   Display_Node(N);
   Display_All_Children(N);
   
   -- Ok maintenant j'ai le bon attribut
   Get_Image(N, Img);
   -- Getting width
   Put_Line("Width: " & Positive'Image(GID.Pixel_Width(Img)));
   -- Getting height
   Put_Line("Height: " & Positive'Image(GID.Pixel_height(Img)));
   
end ;
