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

with GNAT.Sockets ; use GNAT.Sockets ;
with Interfaces; use Interfaces ;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;

with GID ;

procedure Wasabee_Image is
   
   
   type Byte_Array is array(Integer range <>) of Unsigned_8;
   type p_Byte_Array is access Byte_Array;
   procedure Dispose is new Ada.Unchecked_Deallocation(Byte_Array, p_Byte_Array);
   
   --
   -- But du jeu, a partir d'un node, obtenir un Handler GID sur une image
   -- Easy non ?
   --
   procedure Get_Image (Nd : in Node ; Img : in out GID.Image_Descriptor) is
      A            : Attr ;
      The_Url      : Wasabee.Url.URL ;      
      Client       : Socket_Type ;
      Address      : Sock_Addr_Type;
      Channel      : Stream_Access ;
      Send         : constant String :=  (1 => ASCII.CR, 2 => ASCII.LF,
                                          3 => ASCII.CR, 4 => ASCII.LF);  
      next_frame, current_frame: Ada.Calendar.Day_Duration:= 0.0;
      
   begin
      A := Get_Named_Item(Attributes(Nd),"src");
      Put_Line(Value(A)) ;
      -- Ok essayons de l'ouvrir en HTTP maintenant ...
      Decode(To_Unbounded_String(Value(A)), The_Url);
      Display_URL_Details(The_Url);      
      
      GNAT.Sockets.Initialize;
      Create_Socket(Client);
      Put_Line("Trying to get IP: " & To_String(The_Url.Host));
      Address.Addr := To_IP_Address(To_String(The_Url.Host));
      Address.Port := The_URL.Port;
      Put_Line("Trying to open " & 
		 To_String(The_Url.Host) & 
		 ", Port " & 
		 Port_Type'Image(The_Url.Port));
      Connect_Socket (Client, Address);
      Channel := Stream(Client);
      GID.Load_image_header(Img,
			    Channel.all);
      
   end ;
   
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
end ;
