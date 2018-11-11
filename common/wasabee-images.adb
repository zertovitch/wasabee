with Wasabee.Request;                   use Wasabee.Request;

with GID;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO ;
with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO;             use Ada.Streams;
with Ada.Calendar;

package body Wasabee.Images is

  overriding procedure Finalize (Object : in out Bitmap_type) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Byte_Array, p_Byte_Array);
  begin
    Dispose(Object.data);
  end Finalize;

  procedure Load_image_body(Object : in out Bitmap_type; Descr: in out GID.Image_descriptor) is

    generic
      correct_orientation: GID.Orientation;
    -- Load image into a 32-bit truecolor BGRA (or RGBA ?) raw bitmap
    procedure Load_raw_image(
      image : in out GID.Image_descriptor;
      buffer: in out p_Byte_Array;
      next_frame: out Ada.Calendar.Day_Duration
    );
    --
    bkg_buf: p_Byte_Array:= null; -- unused so far
    pragma Warnings(Off, bkg_buf);
    bkg: GID.Image_descriptor;    -- unused so far
    pragma Warnings(Off, bkg);
    --
    procedure Load_raw_image(
      image : in out GID.Image_descriptor;
      buffer: in out p_Byte_Array;
      next_frame: out Ada.Calendar.Day_Duration
    )
    is
      subtype Primary_color_range is Byte;
      type U16 is mod 2**16;
      -- Width/height as stored, before any orientation (e.g. EXIF)
      image_width: constant Positive:= GID.Pixel_Width(image);
      image_height: constant Positive:= GID.Pixel_Height(image);
      padded_line_size_x: constant Positive:= 4 * image_width;
      padded_line_size_y: constant Positive:= 4 * image_height;
      -- (in bytes)
      idx: Integer;
      mem_x, mem_y: Natural;
      bkg_padded_line_size: Positive;
      bkg_width, bkg_height: Natural;
      --
      procedure Set_X_Y (x, y: Natural) is
      pragma Inline(Set_X_Y);
        use GID;
        rev_x: constant Natural:= image_width - (x+1);
        rev_y: constant Natural:= image_height - (y+1);
      begin
        case correct_orientation is
          when Unchanged =>
            idx:= 4 * x + padded_line_size_x * rev_y;
          when Rotation_90 =>
            idx:= 4 * rev_y + padded_line_size_y * rev_x;
          when Rotation_180 =>
            idx:= 4 * rev_x + padded_line_size_x * y;
          when Rotation_270 =>
            idx:= 4 * y + padded_line_size_y * x;
        end case;
        mem_x:= x;
        mem_y:= y;
      end Set_X_Y;
      --
      -- No background version of Put_Pixel
      --
      procedure Put_Pixel_without_bkg (
        red, green, blue : Primary_color_range;
        alpha            : Primary_color_range
      )
      is
      pragma Inline(Put_Pixel_without_bkg);
        use GID;
      begin
        buffer(idx..idx+3):= (blue, green, red, alpha);
        -- GID requires us to look to next pixel for next time:
        case correct_orientation is
          when Unchanged =>
            idx:= idx + 4;
          when Rotation_90 =>
            idx:= idx - padded_line_size_y;
          when Rotation_180 =>
            idx:= idx - 4;
          when Rotation_270 =>
            idx:= idx + padded_line_size_y;
        end case;
      end Put_Pixel_without_bkg;
      --
      -- Background image version of Put_Pixel
      --
      procedure Put_Pixel_with_image_bkg (
        red, green, blue : Primary_color_range;
        alpha            : Primary_color_range
      )
      is
      pragma Inline(Put_Pixel_with_image_bkg);
        b_red,
        b_green,
        b_blue : Primary_color_range;
        bkg_idx: Natural;
      begin
        if alpha = 255 then
          buffer(idx..idx+2):= (blue, green, red);
        else -- blend with background image
          bkg_idx:= 3 * (mem_x mod bkg_width) + bkg_padded_line_size * (mem_y mod bkg_height);
          b_blue := bkg_buf(bkg_idx);
          b_green:= bkg_buf(bkg_idx+1);
          b_red  := bkg_buf(bkg_idx+2);
          buffer(idx)  := Primary_color_range((U16(alpha) * U16(blue)  + U16(255-alpha) * U16(b_blue) )/255);
          buffer(idx+1):= Primary_color_range((U16(alpha) * U16(green) + U16(255-alpha) * U16(b_green))/255);
          buffer(idx+2):= Primary_color_range((U16(alpha) * U16(red)   + U16(255-alpha) * U16(b_red)  )/255);
        end if;
        idx:= idx + 3;
        -- ^ GID requires us to look to next pixel on the right for next time.
        mem_x:= mem_x + 1;
      end Put_Pixel_with_image_bkg;

      stars: Natural:= 0;
      procedure Feedback(percents: Natural) is
        so_far: constant Natural:= percents / 5;
      begin
        for i in stars+1..so_far loop
          Put('*');
        end loop;
        stars:= so_far;
      end Feedback;

      procedure BMP32_Load_without_bkg is
        new GID.Load_image_contents(
          Primary_color_range,
          Set_X_Y,
          Put_Pixel_without_bkg,
          Feedback,
          GID.fast
        );

      procedure BMP32_Load_with_image_bkg is
        new GID.Load_image_contents(
          Primary_color_range,
          Set_X_Y,
          Put_Pixel_with_image_bkg,
          Feedback,
          GID.fast
        );

    begin
      case correct_orientation is
        when GID.Unchanged | GID.Rotation_180 =>
          buffer:= new Byte_Array(0..padded_line_size_x * GID.Pixel_height(image) - 1);
        when GID.Rotation_90 | GID.Rotation_270 =>
          buffer:= new Byte_Array(0..padded_line_size_y * GID.Pixel_width(image) - 1);
      end case;
      if GID.Expect_transparency(image) then
        if bkg_buf = null then
          BMP32_Load_without_bkg(image, next_frame);
        else
          bkg_width:= GID.Pixel_width(bkg);
          bkg_height:= GID.Pixel_height(bkg);
          bkg_padded_line_size:=
            4 * Integer(Float'Ceiling(Float(bkg_width) * 4.0 / 4.0));
          BMP32_Load_with_image_bkg(image, next_frame);
        end if;
      else
        BMP32_Load_without_bkg(image, next_frame);
      end if;
    exception
      when others =>
        next_frame:= 0.0; -- forgive errors
        put_line("Error in image!");
    end Load_raw_image;

    procedure Load_raw_image_0 is new Load_raw_image(GID.Unchanged);
    procedure Load_raw_image_90 is new Load_raw_image(GID.Rotation_90);
    procedure Load_raw_image_180 is new Load_raw_image(GID.Rotation_180);
    procedure Load_raw_image_270 is new Load_raw_image(GID.Rotation_270);

    next_frame: Ada.Calendar.Day_Duration:= 0.0;

  begin  --  Load_image_body
      case GID.Display_orientation(Descr) is
        when GID.Unchanged =>
          Load_raw_image_0(Descr, Object.data, next_frame);
        when GID.Rotation_90 =>
          Load_raw_image_90(Descr, Object.data, next_frame);
        when GID.Rotation_180 =>
          Load_raw_image_180(Descr, Object.data, next_frame);
        when GID.Rotation_270 =>
          Load_raw_image_270(Descr, Object.data, next_frame);
      end case;
  end Load_image_body;
  --
  -- Definition of an input stream for reading a String via streams
  -- Excerpt of Zip_Streams

  type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with null record;
  -- ROM_stream spec
  type ROM_stream is new Root_Zipstream_Type with
     record
        Unb : Unbounded_String;
        Loc : Integer := 1;
     end record;
  -- Read dataimg_buf from the stream.
  procedure Read
    (Stream : in out ROM_stream;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset);
   -- write data to the stream, starting from the current index.
   -- Data will be overwritten from index is already available.
  procedure Write
    (Stream : in out ROM_stream;
     Item   : Stream_Element_Array) is null;

  procedure Read
    (Stream : in out ROM_stream;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset) is
  begin
    -- Item is read from the stream. If (and only if) the stream is
    -- exhausted, Last will be < Item'Last. In that case, T'Read will
    -- raise an End_Error exception.
    --
    -- Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
    -- explanations by Tucker Taft
    --
    Last:= Item'First - 1;
    -- if Item is empty, the following loop is skipped; if Stream.Loc
    -- is already indexing out of Stream.Unb, that value is also appropriate
    for i in Item'Range loop
      Item(i) := Character'Pos (Element(Stream.Unb, Stream.Loc));
      Stream.Loc := Stream.Loc + 1;
      Last := i;
    end loop;
  exception
    when Ada.Strings.Index_Error =>
       null; -- what could be read has been read; T'Read will raise End_Error
  end Read;

  procedure Set (Str : in out ROM_stream; Unb : Unbounded_String) is
  begin
    Str.Unb := Null_Unbounded_String; -- clear the content of the stream
    Str.Unb := Unb;
    Str.Loc := 1;
  end Set;

  procedure Dump_as_PPM(name: String; i: GID.Image_descriptor; img_buf: Byte_Array) is
    use Ada.Streams.Stream_IO;
    f: Ada.Streams.Stream_IO.File_Type;
  begin
    Create(f, Out_File, name & ".ppm");
    -- PPM Header:
    String'Write(
      Stream(f),
      "P7 " & ASCII.LF &
      "WIDTH " & Integer'Image(GID.Pixel_width(i)) & ASCII.LF &
      "HEIGHT " & Integer'Image(GID.Pixel_height(i)) & ASCII.LF &
      "MAXVAL 255" & ASCII.LF &
      "TUPLTYPE RGB_ALPHA" & ASCII.LF &
      "ENDHDR" & ASCII.LF
    );
    -- PPM raw BGR image (so red and blue inverted):
    Byte_Array'Write(Stream(f), img_buf);
    -- ^ slow on some Ada systems, see to_bmp to have a faster version
    Close(f);
  end Dump_as_PPM;
  pragma Unreferenced (Dump_as_PPM);

  -- ppms: natural:= 0; -- !! temp

  -- Blocking reading of a bitmap
  procedure Get_Full_Image_Blocking (URL: String; bitmap: out Bitmap_type) is
    compr_data: Unbounded_String;
    stream: ROM_stream;
    descriptor: GID.Image_Descriptor;
  begin
    Retrieve_from_URL(URL, compr_data);
    Set(stream, compr_data);
    begin
      GID.Load_image_header(descriptor,stream);
      bitmap.transparency:= GID.Expect_transparency(descriptor);
      case GID.Display_orientation(descriptor) is
        when GID.Unchanged | GID.Rotation_180 =>
          bitmap.width := GID.Pixel_width(descriptor);
          bitmap.height:= GID.Pixel_height(descriptor);
        when GID.Rotation_90 | GID.Rotation_270 =>
          bitmap.width := GID.Pixel_height(descriptor);
          bitmap.height:= GID.Pixel_width(descriptor);
      end case;
      Load_image_body(bitmap, descriptor);
      -- ppms:=ppms+1;
      -- Dump_as_PPM("image" & ppms'img, descriptor, bitmap.data.all);
    exception
      when End_Error =>
        null;  --  fake image or connection interrupted
      when GID.unknown_image_format | GID.unsupported_image_subformat =>
        bitmap.width:= 0;
        bitmap.height:= 0;
    end;
  end Get_Full_Image_Blocking;

end Wasabee.Images;
