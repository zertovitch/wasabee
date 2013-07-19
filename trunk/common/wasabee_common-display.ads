package Wasabee_common.Display is

  type Frame_plane is abstract new Ada.Finalization.Controlled with null record;
  procedure Text_XY (on: Frame_plane; x,y: Integer; text: UTF_16_String) is abstract;
 
  procedure Draw (on: Frame_plane'Class; o: HTML_object);
  
end Wasabee_common.Display;
