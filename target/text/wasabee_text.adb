-- "Lynx" mode of Wasabee: text only.
-- Actually we may have two text modes - see Text_mode below

with Wasabee;                           use Wasabee;
with Wasabee.Colors;                    use Wasabee.Colors;
with Wasabee.Encoding;                  use Wasabee.Encoding;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Hypertext.Parsing;         use Wasabee.Hypertext.Parsing;
with Wasabee.Images;                    use Wasabee.Images;
with Wasabee.Request;
with Wasabee.Util;                      use Wasabee.Util;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
-- with Ada.Exceptions;
-- with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
-- with GNAT.Traceback.Symbolic;

procedure Wasabee_text is

  type Text_mode is (
    pure_text, -- only characters and line feeds
    terminal   -- with escape sequences to position cursor, set colours, etc.
  );

  mode: constant Text_mode:= pure_Text;

  type Text_plane is new Frame_plane with record
    x, y: Positive;
  end record;

  overriding procedure Clear_area(on: in out Text_plane);
  overriding procedure Area_size (on: Text_plane; w,h: out Natural);
  overriding procedure Extend_area_height (on: in out Text_plane; to: Natural) is null;

  overriding procedure Create_target_font(
    on         : in out Text_plane;
    descriptor : in     Font_descriptor;
    new_index  : in     Positive
  ) is null;
  overriding procedure Select_target_font(
    on         : in out Text_plane;
    index      : in     Positive
  ) is null;
  overriding procedure Destroy_target_fonts(on: in out Text_plane) is null;
  overriding procedure Text_at(on: in out Text_plane; p: Point; text: UTF_16_String);
  overriding procedure Text_size (
    on   : in out Text_plane;
    text : in     UTF_16_String;
    x,y  :    out Natural
  );
  overriding procedure Select_target_fore_color(
    on: in out Text_plane;
    code: in Color_Code
  ) is null;
  overriding procedure Select_target_back_color(
    on: in out Text_plane;
    code: in Color_Code
  ) is null;


  overriding procedure Rectangle (on: in out Text_plane; coords: Box) is null;
  overriding procedure Full_Rectangle (on: in out Text_plane; coords: Box) is null;

  overriding procedure Put_RGB_Bitmap (
    on     : in out Text_plane;
    bitmap :        Bitmap_type;
    coords :        Box
  )
  is null;

  ---------------------------
  -- Body - implementation --
  ---------------------------

  procedure Clear_area(on: in out Text_plane) is
  begin
    on.x:= 1;
    on.y:= 1;
    case mode is
      when pure_text =>
        null;
      when terminal =>
        null; -- !!
    end case;
  end Clear_area;

  procedure Area_size (on: Text_plane; w,h: out Natural) is
  pragma Unreferenced (on);
  begin
    case mode is
      when pure_text =>
        w:= 80;
        h:= Integer'Last;
      when terminal =>
        w:= 80;
        h:= 50;
    end case;
  end Area_size;

  procedure Set_XY(on: in out Text_plane; x,y: Positive) is
    use Ada.Strings.Wide_Fixed;
  begin
    case mode is
      when pure_text =>
        if y /= on.y then
          New_Line;
          Put((x-1)*' ');
        elsif x > on.x then
          Put((x-on.x)*' ');
        end if;
      when terminal =>
        null; -- !!
    end case;
    on.x:= x;
    on.y:= y;
  end Set_XY;

  procedure Text_at(on: in out Text_plane; p: Point; text: UTF_16_String) is
  begin
    Set_XY(on, p.x+1, p.y+1); -- We are 1-based.
    Put(text);
    on.x:= on.x + text'Length;
  end Text_at;

  procedure Text_size (
    on   : in out Text_plane;
    text : in     UTF_16_String;
    x,y  :    out Natural
  )
  is
  pragma Unreferenced (on);
  begin
    x:= text'Length;
    y:= 1;
  end Text_size;

  txt: Text_plane;
  HTML : Unbounded_String;
  o: HT_object;

begin
  if Argument_Count = 0 then
    case mode is
      when pure_text =>
        Put_Line(Standard_Error, "Wasabee, target pure text console, version " & To_Wide_String(Version));
        Put_Line(Standard_Error, "Provide an URL as command-line argument");
        Put_Line(Standard_Error, "Syntax: wasabee_text url [verbosity level]");
        Put_Line(Standard_Error, "Verbosity level = 0, 1, ...");
      when terminal =>
        null; -- !! some interaction (Lynx-style)
    end case;
  else
    Wasabee.Request.Retrieve_from_URL (Argument(1), HTML);
    if argument_count > 1 then
      Wasabee.Verbosity := Integer'Value (Argument(2));
    end if;
    o.Set_own_URL(Argument(1));
    Load_frame(o, To_String(HTML));
    o.Post_loading_processing;
    txt.Clear_area;
    txt.Draw(o, full);
  end if;
end Wasabee_text;
