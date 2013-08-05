with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Wasabee.Util is

  function S (Source : Unbounded_String) return String renames To_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;

end Wasabee.Util;
