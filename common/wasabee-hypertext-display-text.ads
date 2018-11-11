private package Wasabee.Hypertext.Display.Text is

  procedure Carriage_Return(on: in out Frame_plane'Class);
  pragma Inline(Carriage_Return);

  procedure New_Line(
    on         : in out Frame_plane'Class;
    mode       :        Draw_mode;
    with_marker:        Boolean:= False
  );
  pragma Inline(New_Line);

  procedure Show_text(on: in out Frame_plane'Class; t: UTF_16_String; mode: Draw_mode);
  
  procedure Reset_text(on: in out Frame_plane'Class);
  
end Wasabee.Hypertext.Display.Text;
