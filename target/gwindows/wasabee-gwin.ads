-- Here is the hierachical scheme from Wasabee application to visual contents:

-- Main window (hidden, except in super-user mode)
-- Windows
-- (in each window) Tabs
-- (in each tab)    Frameset
-- (in each frame)  Frame's contents

with GWindows;                          use GWindows;
with GWindows.GStrings;                 use GWindows.GStrings;

package Wasabee.GWin is

  -- Almost everything with children so far :-)

  -----------------------------------------------
  -- A few shortcut function names for strings --
  -----------------------------------------------

  function S2G (Value : String) return GString renames To_GString_From_String;
  function G2S (Value : GString) return String renames To_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

private

  super_user: constant Boolean:= True;

end Wasabee.GWin;
