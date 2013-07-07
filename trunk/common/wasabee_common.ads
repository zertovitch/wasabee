with Ada.Finalization;

package Wasabee_common is

  type HTML_object is new Ada.Finalization.Controlled with null record;

  procedure Load_frame(object: in out HTML_object; URL: String);
  
end Wasabee_common;
