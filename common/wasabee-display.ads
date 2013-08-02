package Wasabee_common.Display is

  type Frame_plane is abstract new Ada.Finalization.Limited_Controlled with null record;
  
  procedure Text_XY (
    on   : in out Frame_plane; 
    x,y  : in     Integer; 
    text : in     UTF_16_String
  ) is abstract;
 
  procedure Draw (on: in out Frame_plane'Class; o: HTML_object);
  
end Wasabee_common.Display;
