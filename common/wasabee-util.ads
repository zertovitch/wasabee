with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee.Util is

  function S (Source : Unbounded_String) return String renames To_String;
  function S (Source : Unbounded_Wide_String) return Wide_String renames To_Wide_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;
  function U (Source : Wide_String) return Unbounded_Wide_String renames To_Unbounded_Wide_String;

  -- Remove multiple blanks and replace tabs, carriage returns, line feed, and so on by one blank
  function Filter_blanks (s : Wide_String) return Wide_String;

  function Get_hex_value(s: String) return Natural; -- NB: up to 31 bit; usually 24 bit.

end Wasabee.Util;
