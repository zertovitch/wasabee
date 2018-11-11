-----------------------------------------------------------------------------
-- Navigation log - history held for use with "Back" and "Forward" buttons --
-----------------------------------------------------------------------------

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Wasabee.Navigation is

  type Navigation_log is tagged private;
  
  -- [1] If new_URL is exactly the URL at current position, do nothing;
  -- [2] if new_URL is exactly the URL at next position, just do "Forward";
  -- [3] otherwise, erase any log from next position and register new_URL as new last URL.

  procedure Register(nav: in out Navigation_log; new_URL: String);
  
  procedure Back(nav: in out Navigation_log);

  procedure Forward(nav: in out Navigation_log);

  function Current_URL(nav: Navigation_log) return String;  -- returns "" if log is empty

private

  package Navigation_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Unbounded_String
  );

  type Navigation_log is tagged record
    index: Positive;
    log  : Navigation_Vectors.Vector;
  end record;

end Wasabee.Navigation;
