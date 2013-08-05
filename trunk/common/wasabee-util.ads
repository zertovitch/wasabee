with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee.Util is

  function S (Source : Unbounded_String) return String renames To_String;
  function S (Source : Unbounded_Wide_String) return Wide_String renames To_Wide_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;
  function U (Source : Wide_String) return Unbounded_Wide_String renames To_Unbounded_Wide_String;

end Wasabee.Util;
