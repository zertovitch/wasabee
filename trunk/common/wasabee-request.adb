with Ada.Direct_IO;
-- with Ada.Text_IO;             use Ada.Text_IO;
-- with Ada.Streams;                       use Ada.Streams;
-- with Ada.Task_Identification; use Ada.Task_Identification;

-- with GNAT.Sockets;            use GNAT.Sockets;
-- with Dom.Readers             ;use Dom.Readers                      ;

-- with Dom.Core                ;use Dom.Core                         ;
-- with Dom.Core.Documents     ; use Dom.Core.Documents                    ;
-- with Dom.Core.Nodes         ; use Dom.Core.Nodes                        ;
-- with Dom.Core.Attrs         ; use Dom.Core.Attrs                        ;

-- with Input_Sources.File;      use Input_Sources.File ;

with Wasabee;
-- with Wasabee.Xhtml ;          use Wasabee.Xhtml ;
with Wasabee.Url   ;             use Wasabee.Url ;

-- with Sax.Readers ;

---------------------------------------------------------
-- HTTP requests -- choice between Wasabee.Net and AWS --
---------------------------------------------------------
-- with Wasabee.Net;
with AWS.Client, AWS.Response;


package body Wasabee.Request is

   procedure Close(c: in out File_Connection) is
   begin
     Ada.Streams.Stream_IO.Close(c.file);
   end Close;

   procedure Close(c: in out HTTP_Connection) is
   begin
     GNAT.Sockets.Close_Socket(c.client);
   end Close;

   --     procedure Open_Url (The_Url : in String ; Nl : in out Node_List) is
   --        U : Wasabee.Url.Split_URL ;
   --        Xhtml_Content : Unbounded_String ;
   --        Html_Content : Unbounded_String ;
   --        Reader : Tree_Reader ;
   --     begin
   --        Decode(To_Unbounded_String(The_Url), U) ;
   --        Display_URL_details(U);
   --        if U.Protocole = "http" then
   --           begin
   --              Get_HTTP_Content (The_Url, Html_Content) ;
   --              Extract_Html (Html_Content,Xhtml_Content);
   --  	    -- Recuperation du style la dedans ...
   --              Get_Xhtml_Content (To_String(Xhtml_Content), Nl, Reader);
   --           exception
   --              when Sax.Readers.XML_FATAL_ERROR => -- Error :
   --                 Put_Line("This document is not an XHTML document");
   --                 Abort_Task (Current_Task);
   --           end;
   --        elsif U.Protocole = "file" then
   --           declare
   --              Input : File_Input ;
   --              Reader : Tree_Reader ;
   --              Doc    : Document ;
   --           begin
   --              -- Put_Line("opening");
   --              Open(To_String(U.Ressource), Input);
   --              -- Put_Line("parsing");
   --              Parse(Reader, Input);
   --              -- Put_Line("closing stream");
   --              Close(Input);
   --              -- Put_Line("getting elements");
   --              Doc := Get_Tree(Reader);
   --              -- Get_Document_Style(Doc);
   --  	    Nl  := Get_Elements_By_Tag_Name(Doc,"html");
   --              -- Free(Reader) ;
   --           -- exception
   --              -- when Error : Sax.Readers.XML_FATAL_ERROR =>
   --                 -- Put_Line("This document is not an XHTML document");
   --                 -- Abort_Task (Current_Task);
   --           end;
   --        end if ;
   --     end Open_URL;

   procedure Retrieve_from_URL (The_URL : in String ; Data : out Unbounded_String) is
      U : Wasabee.Url.Split_URL ;
      package CIO is new Ada.Direct_IO(Character);
      use CIO;
      f: CIO.File_Type;
   begin
      Decode(To_Unbounded_String(The_Url), U) ;
      Display_URL_details(U);
      if U.Protocole = "http" or U.Protocole = "https" then
        Data:= To_Unbounded_String(AWS.Response.Message_Body(AWS.Client.Get(the_URL)));
        -- Wasabee.Net.Get_HTTP_Content (The_Url, Data) ;
      elsif U.Protocole = "file" then
        Open(f,In_File,To_String(U.Ressource));
        declare
          temp: String(1..Integer(CIO.Size(f)));
        begin
          for i in temp'Range loop -- !! slow, use Stream_Element_Array instead.
            Read(f,temp(i));
          end loop;
          Data:= To_Unbounded_String(temp);
        end;
        Close(f);
      end if ;
   end Retrieve_from_URL;

end Wasabee.Request ;
