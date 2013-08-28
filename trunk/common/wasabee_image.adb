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

with Interfaces.C ;
with Ada.Calendar ;
procedure Wasabee_Image is
   Xhtml, Nl : Node_List ;
   Xhtml_Content : Unbounded_String ;
   N : Node;
   Img : GID.Image_Descriptor ;



   subtype Primary_Color_Range is Interfaces.C.Unsigned_Short ;

   procedure Set_X_Y (x, y: Natural) is
   begin
      Ada.Text_IO.Put_Line("Set_X_Y " &
                             Natural'Image(X) & ":" &
                             Natural'Image(Y)) ;
   end ;
   procedure Put_Pixel (red, green, blue : Primary_color_range;
                        alpha            : Primary_color_range
                       ) is
   begin

      Ada.Text_IO.Put_Line(Natural'Image(Integer(Red)) &
                             Natural'Image(Integer(Green)) &
                             Natural'Image(Integer(Blue))) ;
   end ;
   procedure Feedback (percents: Natural) is
   begin
      Ada.Text_IO.Put_Line(Natural'Image(Percents) & " %");
   end ;

   procedure Local_Load_Image_Contents
   is new GID.Load_Image_Contents ( Primary_color_range,
                                    Set_X_Y,
                                    Put_Pixel,
                                    Feedback,
                                    GID.fast);
   Dd : Ada.Calendar.Day_Duration ;

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
   Get_Image_Header(N, Img) ;
   -- Getting width
   Put_Line("Width: " & Positive'Image(GID.Pixel_Width(Img)));
   -- Getting height
   Put_Line("Height: " & Positive'Image(GID.Pixel_height(Img)));
   -- Et enfin le chargement de l'image ...
   Local_Load_Image_Contents(Img,dd) ;


end ;
