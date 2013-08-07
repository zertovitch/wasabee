with Dom ; use Dom ;
with Dom.Core ; use Dom.Core ;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package Wasabee.Request is

   procedure Open_Url (The_Url : in String ; Nl : in out Node_List) ;

   procedure Open_Url (The_Url : in String ; contents : out Unbounded_String) ;

end ;

