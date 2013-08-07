package body Wasabee.Display is
  
  procedure Draw (on: in out Frame_plane'Class; o: HTML_object) is
  begin
    on.Clear_area;
    on.Text_XY(10, 20, "This is Draw from Wasabee.Display (portable)");
    -- !!
  end Draw;
  
  procedure Select_font(
    on         : in out Frame_plane'Class; 
    descriptor : in     Font_descriptor
  )
  is
    index: constant Natural:= on.font_list.Find_Index(descriptor);
  begin
    if index = 0 then -- A font with this face name, size, boldness, etc. doesn't exist yet
      on.font_list.Append(descriptor);
      on.Create_target_font(descriptor, on.font_list.Last_Index);
    else
      on.Select_target_font(index);
    end if;    
  end Select_font;
  
end Wasabee.Display;
