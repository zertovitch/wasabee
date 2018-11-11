with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee.Util is

  -----------------------------
  -- Wasabee version strings --
  -----------------------------

  function Version return String;        -- full
  function Version_number return String; -- number only, like 0.0.1

  ---------------------------------------
  -- Shortcuts for string manipulation --
  ---------------------------------------

  function S (Source : Unbounded_String) return String renames To_String;
  function S (Source : Unbounded_Wide_String) return Wide_String renames To_Wide_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;
  function U (Source : Wide_String) return Unbounded_Wide_String renames To_Unbounded_Wide_String;

  -- Replace multiple blanks (space, tabs, carriage returns, line feeds, and so on), by one space.
  -- For Unicode text.
  -- If fully = False, all is preserved (incl. line feeds) except tabs, carriage returns.
  -- This is for the <PRE> mode.
  function Filter_blanks (s : Wide_String; fully: Boolean) return Wide_String;

  function Get_hex_value(s: String) return Natural; -- NB: up to 31 bit; usually 24 bit.

  -- To_Enum replaces '-' by '_' and tries to match a value in the Enum type.
  generic
    type Enum is (<>); default: Enum;
  function To_Enum_func(key: String) return Enum;

  generic
    type Enum is (<>);
  procedure To_Enum_proc(key: String; variable: in out Enum);

  ----------
  -- Misc --
  ----------

  procedure Dump_string(file_name, s: String);

  procedure Inc(x: in out Integer);
  pragma Inline(Inc);
  procedure Dec(x: in out Integer);
  pragma Inline(Dec);

  procedure Add(x: in out Integer; y: Integer);
  pragma Inline(Add);
  procedure Sub(x: in out Integer; y: Integer);
  pragma Inline(Sub);

  procedure Min(x: in out Integer; y: Integer);
  pragma Inline(Min);
  procedure Max(x: in out Integer; y: Integer);
  pragma Inline(Max);

end Wasabee.Util;
