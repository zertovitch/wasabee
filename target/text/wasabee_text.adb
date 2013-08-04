-- "Lynx" mode of Wasabee: text only.

with Wasabee;                           use Wasabee;
with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.Display;

-- with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

-- with GNAT.Traceback.Symbolic;

procedure Wasabee_text is

  type Text_plane is new Wasabee.Display.Frame_plane with null record;

  overriding procedure Clear_area(on: in out Text_plane);
  overriding procedure Text_XY(on: in out Text_plane; x,y: Integer; text: UTF_16_String);

  procedure Clear_area(on: in out Text_plane) is null;
  procedure Text_XY(on: in out Text_plane; x,y: Integer; text: UTF_16_String) is
  begin
    Put("lala");
  end Text_XY;

  txt: Text_plane;
  o: HTML_object;

begin
  txt.Draw(o);
end Wasabee_text;
