with GNAT.Sockets ; use GNAT.Sockets ;

with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

package Wasabee.Net is

   function To_IP_Address (Host : in String) return Inet_Addr_Type;

   --  Get URL from command-line arguments
   procedure Get_URL (Base_Url      : out Unbounded_String ;
                      Extension_Url : out Unbounded_String ;
                      Port          : out Port_Type) ;

   procedure Get_HTTP_Content (U : String ;
                               Content : out Unbounded_String) ;


   procedure Get_HTTP_Content (Host     : in String ;
                               Resource : in String ;
                               Port     : in Port_Type ;
                               Content  : out Unbounded_String) ;



end Wasabee.Net;
