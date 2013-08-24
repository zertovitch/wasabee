package Wasabee.Hypertext.Locations is

  -- If an URL is under the (x,y) point, return it, "" otherwise.
  function Mouse_URL(ho: HT_object; x,y: Natural) return String;

  type Mouse_cursor_style is (arrow, finger, I_beam);

  -- Upon a normal mouse move (no selection), this function returns
  -- the appropriate mouse cursor.
  function Mouse_cursor(ho: HT_object; x, y: Natural) return Mouse_cursor_style;

end Wasabee.Hypertext.Locations;
