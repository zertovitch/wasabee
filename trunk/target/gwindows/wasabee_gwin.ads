-- Here is the hierachical scheme from Wasabee application to visual contents:

-- Main window (hidden, except in super-user mode)
-- Windows
-- (in each window) Tabs
-- (in each tab)    Frameset
-- (in each frame)  Frame's contents


package Wasabee_GWin is

  -- Almost everything with children so far :-)

private

  super_user: constant Boolean:= True;

end Wasabee_GWin;
