-- This is a draft and may change much,
-- depending on how the bitmap abstraction is implemented.

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;


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

with GNAT.Sockets ; use GNAT.Sockets ;
with Interfaces; use Interfaces ;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with Ada.Streams                      ; use Ada.Streams                      ;
with GID ;

package Wasabee.Images is

  procedure Decode(image_data: Unbounded_String);
  -- Same for progressive decoding

  function Read_Line (Channel : in Stream_Access) return String ;

  procedure Get_Image_Header (Nd : in Node ; Img : in out GID.Image_Descriptor) ;

  procedure Get_Image_Header (Url : in String ; Desc : in out GID.Image_Descriptor) ;

end Wasabee.Images;
