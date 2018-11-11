package Wasabee.Hypertext.Locations is

  -- If an hyperlink is under mouse_point, return the URL; return "" otherwise.
  function Mouse_URL(ho: HT_object; mouse_point: Point) return String;

  type Mouse_cursor_style is (arrow, finger, I_beam);

  -- Upon a normal mouse move (no selection), this function returns
  -- the appropriate mouse cursor.
  function Mouse_cursor(ho: HT_object; mouse_point: Point) return Mouse_cursor_style;

  -- Given an anchor name, return the element position
  function Anchor_position(ho: HT_object; anchor: String) return Point;

end Wasabee.Hypertext.Locations;
