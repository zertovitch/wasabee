-- This is a draft and may change much,
-- depending on how the bitmap abstraction is implemented.

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Wasabee.Images is

  procedure Decode(image_data: Unbounded_String);
  -- Same for progressive decoding

end Wasabee.Images;
