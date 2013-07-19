with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package Wasabee_common is

  type HTML_object is new Ada.Finalization.Controlled with null record;

  procedure Load_frame(object: in out HTML_object; URL: String);

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;
  
end Wasabee_common;
