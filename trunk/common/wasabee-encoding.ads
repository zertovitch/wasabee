with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Strings.UTF_Encoding;

package Wasabee.Encoding is

  type Encoding_choice is (
    utf_8,
    iso_8859_1,
    iso_8859_2,
    windows_1252
  );
  -- <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

   procedure Parse_encoding
     (content_str: String; encoding: in out Encoding_choice);

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  function To_UTF_16(s: String; e: Encoding_choice) return UTF_16_String;

end Wasabee.Encoding;
