-- This is a draft and may change much,
-- depending on how the bitmap abstraction is implemented.

with Ada.Finalization;

package Wasabee.Images is

  type Byte is mod 2**8;
  type Byte_Array is array(Integer range <>) of aliased Byte;
  type p_Byte_Array is access all Byte_Array;

  type Bitmap_type is new Ada.Finalization.Controlled with record
    width, height: Natural:= 0;           -- Image's own dimensions
    data         : p_Byte_Array:= null;   -- RGB, 32 bit per pixel
    transparency : Boolean;
  end record;

  overriding procedure Finalize (Object : in out Bitmap_type);

  -- Blocking reading of a bitmap
  procedure Get_Full_Image_Blocking (URL: String; bitmap: out Bitmap_type);

end Wasabee.Images;
