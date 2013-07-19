-- "Lynx" mode of Wasabee: text only.

with Wasabee_common; use Wasabee_common;
with Wasabee_common.Display;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Traceback.Symbolic;

procedure Wasabee is

  type Text_plane is new Wasabee_common.Display.Frame_plane with null record;

  overriding procedure Text_XY(on: Text_plane; x,y: Integer; text: UTF_16_String);

  procedure Text_XY(on: Text_plane; x,y: Integer; text: UTF_16_String) is
  begin
    Put("lala");
  end Text_XY;

  txt: Text_plane;
  o: HTML_object;

begin
  txt.Draw(o);
end Wasabee;
