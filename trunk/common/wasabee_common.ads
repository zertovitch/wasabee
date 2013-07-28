with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee_common is

  type Refresh_mode is
    (nothing,
     full);

  type HTML_object is new Ada.Finalization.Controlled with record
    refresh: Refresh_mode;
  end record;

  procedure Load_frame(object: in out HTML_object; URL: String);

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  function S (Source : Unbounded_String) return String renames To_String;
  function U (Source : String) return Unbounded_String renames To_Unbounded_String;

end Wasabee_common;
