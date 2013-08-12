with GID;

package body Wasabee.Images is

   procedure Decode (image_data: Unbounded_String) is
   begin
     null;
   end Decode;
   
   function Read_Line (Channel : in Stream_Access) return String is
      Buffer : String (1 .. 1);
      Result : Unbounded_String;
   begin
      loop
	 String'Read (Channel, Buffer);
	 Append (Result, Buffer);
	 exit when Buffer (1) = ASCII.LF;
      end loop;
      return To_String(Result);
   end Read_Line;
   
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
      -- next_frame, current_frame: Ada.Calendar.Day_Duration:= 0.0;
      Content      : Unbounded_String;      
      S : Unbounded_String ;
   begin
      A := Get_Named_Item(Attributes(Nd),"src");
      -- Put_Line(Value(A)) ;
      -- Ok essayons de l'ouvrir en HTTP maintenant ...
      Decode(To_Unbounded_String(Value(A)), The_Url);
      Display_URL_Details(The_Url);      
      
      GNAT.Sockets.Initialize;
      Create_Socket(Client);
      -- Put_Line("Trying to get IP: " & To_String(The_Url.Host));
      Address.Addr := To_IP_Address(To_String(The_Url.Host));
      Address.Port := The_URL.Port;
      Connect_Socket (Client, Address);
      Channel := Stream(Client);
      -- Put_Line("Trying to get Header ...");
      -- Put_Line("Request: " & "GET " & To_String(The_Url.Ressource) & " HTTP/1.0" & Send) ;
      String'Write (Channel, "GET " & To_String(The_Url.Ressource) & " HTTP/1.0" & Send) ;
      -- Put_Line("RESPONSE:");
      loop
	 S := To_Unbounded_String(Read_Line(Channel)) ;
	 -- Put("Length => " & Integer'Image(Length(S)) & ":" & To_String(S));
	 if Length(S) = 2 then
	    exit;
	 else
	    null ;
	 end if ;
      end loop;
      GID.Load_image_header(Img,Channel.all);            
   end ;
   
   
end Wasabee.Images;
