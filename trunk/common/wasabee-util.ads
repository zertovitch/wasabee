with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee.Util is

  -----------------------------
  -- Wasabee version strings --
  -----------------------------

  function Version return String;        -- full
  function Version_number return String; -- number only, like 0.0.1

  --------------------------
  -- Manipulation of URLs --
  --------------------------

  function Build_URL (complete_URL, partial_URL: String) return String;

  -- Examples:
  -- ========
  -- [1] http://en.wikipedia.org/wiki/UNIX     ,  //fr.wikipedia.org/wiki/Unix -> (guess what ?)
  -- [2] http://en.wikipedia.org/wiki/OpenVMS  ,  /wiki/Itanium                -> (guess what ?)

  Build_URL_error: exception;

  ---------------------------------------
  -- Shortcuts for string manipulation --
  ---------------------------------------

  function S (Source : Unbounded_String) return String renames To_String;
  function S (Source : Unbounded_Wide_String) return Wide_String renames To_Wide_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;
  function U (Source : Wide_String) return Unbounded_Wide_String renames To_Unbounded_Wide_String;

  -- Remove multiple blanks and replace tabs, carriage returns, line feed, and so on by one blank
  function Filter_blanks (s : Wide_String) return Wide_String;

  function Get_hex_value(s: String) return Natural; -- NB: up to 31 bit; usually 24 bit.

end Wasabee.Util;
