with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

package Wasabee.URL is

   type URL is record
      Protocole  : Unbounded_String ; -- http generalement
      Host       : Unbounded_String ;
      Port       : Natural ;  -- 80 generalement
      Ressource  : Unbounded_String ;
   end record;

   procedure Display_URL_details (U : in URL) ;

   procedure Decode (Adr : in Unbounded_String ; U : in out URL) ;

   procedure Test (U : String) ;

   procedure General_Test ;

end ;