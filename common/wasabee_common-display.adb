package body Wasabee_common.Display is
  
  procedure Draw (on: Frame_plane'Class; o: HTML_object) is
  begin
    Text_XY(on, 1,1, "o_o");
  end Draw;
  
end Wasabee_common.Display;