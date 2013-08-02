with Wasabee ; use Wasabee ;

with GNAT.Sockets ; use GNAT.Sockets ;

with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

package Wasabee.Net is

   function To_IP_Address (Host : in String) return Inet_Addr_Type;

   procedure Get_Url (Base_Url : in out Unbounded_String ;
                      Extension_Url : in out Unbounded_String ;
                      Port : in out Port_Type) ;

   procedure Get_Http_Content (Base_Url : in String ;
                              Extension_Url : in String ;
                              Port : in Port_Type ;
                              Content : in out Unbounded_String) ;



end Wasabee.Net;
