package body Wasabee.Display is
  
  procedure Draw (on: in out Frame_plane'Class; o: HTML_object) is
  begin
    on.Text_XY(10, 20, "This is Draw from Wasabee_common.Display");
  end Draw;
  
end Wasabee.Display;
