package Wasabee.Hypertext.Parsing is

  -- Load an HT object from a string with HTML code (complete or not). HTML is parsed directly.
  procedure Load_frame(ho: in out HT_object; from: String);

  -- Load an HT object from any stream
    -- procedure Load_frame(ho: in out HT_object; from: Root_Stream_Type'Class);

  -----------------------------------------------------------
  -- Some parsing helpers, also useful for style arguments --
  -----------------------------------------------------------

  --  Returns a size in pixels, e.g. for width="80px"; parses:
  --  "80px", "80", "auto"

  function Size_argument(s: String) return Integer;

end Wasabee.Hypertext.Parsing;
