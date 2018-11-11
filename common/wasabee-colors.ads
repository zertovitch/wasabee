package Wasabee.Colors is

  subtype Color_Code is Integer range -1 .. 2**24-1;
  -- Solid 24-bit RGB color, plus -1 for designating parent color

  Black_code       : constant Color_Code:= 0;
  White_code       : constant Color_Code:= 16#FF_FF_FF#;

  default_color    : constant Color_Code:= Black_code;
  default_bg_color : constant Color_Code:= White_code;

  parent_color : constant Color_Code:= -1;

  function Parse_color(s: String) return Color_Code;

end Wasabee.Colors;

