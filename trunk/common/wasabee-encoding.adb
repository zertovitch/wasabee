--  Conversions found @
--    http://www.unicode.org/Public/MAPPINGS/ISO8859/8859-1.TXT
--    http://www.unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT

with Wasabee.Util;

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

package body Wasabee.Encoding is

  procedure To_Encoding is new Wasabee.Util.To_Enum_proc(Encoding_choice);

  procedure Parse_encoding(content_str: String; encoding: in out Encoding_choice) is
    n: constant Natural:= Index(content_str, "charset=");
  begin
    if n > 0 then
      To_Encoding(content_str(n+8..content_str'Last), encoding);
    end if;
  end Parse_encoding;

  ---------------
  --  Internal --
  ---------------

  function ISO_8859_1_to_UTF_16(s: String) return UTF_16_String is
    --  This conversion is a trivial 8-bit to 16-bit copy.
    r: UTF_16_String(s'Range);
  begin
    for i in s'Range loop
      r(i):= Wide_Character'Val(Character'Pos(s(i)));
    end loop;
    return r;
  end ISO_8859_1_to_UTF_16;

  ISO_8859_2_Conv: constant array(16#A1#..16#FF#) of Integer :=
  (
    16#A1# => 16#0104#,
    16#A2# => 16#02D8#,
    16#A3# => 16#0141#,
    16#A4# => 16#00A4#,
    16#A5# => 16#013D#,
    16#A6# => 16#015A#,
    16#A7# => 16#00A7#,
    16#A8# => 16#00A8#,
    16#A9# => 16#0160#,
    16#AA# => 16#015E#,
    16#AB# => 16#0164#,
    16#AC# => 16#0179#,
    16#AD# => 16#00AD#,
    16#AE# => 16#017D#,
    16#AF# => 16#017B#,
    16#B0# => 16#00B0#,
    16#B1# => 16#0105#,
    16#B2# => 16#02DB#,
    16#B3# => 16#0142#,
    16#B4# => 16#00B4#,
    16#B5# => 16#013E#,
    16#B6# => 16#015B#,
    16#B7# => 16#02C7#,
    16#B8# => 16#00B8#,
    16#B9# => 16#0161#,
    16#BA# => 16#015F#,
    16#BB# => 16#0165#,
    16#BC# => 16#017A#,
    16#BD# => 16#02DD#,
    16#BE# => 16#017E#,
    16#BF# => 16#017C#,
    16#C0# => 16#0154#,
    16#C1# => 16#00C1#,
    16#C2# => 16#00C2#,
    16#C3# => 16#0102#,
    16#C4# => 16#00C4#,
    16#C5# => 16#0139#,
    16#C6# => 16#0106#,
    16#C7# => 16#00C7#,
    16#C8# => 16#010C#,
    16#C9# => 16#00C9#,
    16#CA# => 16#0118#,
    16#CB# => 16#00CB#,
    16#CC# => 16#011A#,
    16#CD# => 16#00CD#,
    16#CE# => 16#00CE#,
    16#CF# => 16#010E#,
    16#D0# => 16#0110#,
    16#D1# => 16#0143#,
    16#D2# => 16#0147#,
    16#D3# => 16#00D3#,
    16#D4# => 16#00D4#,
    16#D5# => 16#0150#,
    16#D6# => 16#00D6#,
    16#D7# => 16#00D7#,
    16#D8# => 16#0158#,
    16#D9# => 16#016E#,
    16#DA# => 16#00DA#,
    16#DB# => 16#0170#,
    16#DC# => 16#00DC#,
    16#DD# => 16#00DD#,
    16#DE# => 16#0162#,
    16#DF# => 16#00DF#,
    16#E0# => 16#0155#,
    16#E1# => 16#00E1#,
    16#E2# => 16#00E2#,
    16#E3# => 16#0103#,
    16#E4# => 16#00E4#,
    16#E5# => 16#013A#,
    16#E6# => 16#0107#,
    16#E7# => 16#00E7#,
    16#E8# => 16#010D#,
    16#E9# => 16#00E9#,
    16#EA# => 16#0119#,
    16#EB# => 16#00EB#,
    16#EC# => 16#011B#,
    16#ED# => 16#00ED#,
    16#EE# => 16#00EE#,
    16#EF# => 16#010F#,
    16#F0# => 16#0111#,
    16#F1# => 16#0144#,
    16#F2# => 16#0148#,
    16#F3# => 16#00F3#,
    16#F4# => 16#00F4#,
    16#F5# => 16#0151#,
    16#F6# => 16#00F6#,
    16#F7# => 16#00F7#,
    16#F8# => 16#0159#,
    16#F9# => 16#016F#,
    16#FA# => 16#00FA#,
    16#FB# => 16#0171#,
    16#FC# => 16#00FC#,
    16#FD# => 16#00FD#,
    16#FE# => 16#0163#,
    16#FF# => 16#02D9#
  );

  function ISO_8859_2_to_UTF_16(s: String) return UTF_16_String is
    r: UTF_16_String(s'Range);
  begin
    for i in s'Range loop
      case Character'Pos(s(i)) is
        when 16#00# .. 16#A0# =>
          r(i):= Wide_Character'Val(Character'Pos(s(i)));  --  trivial 8-bit to 16-bit copy
        when others =>
          r(i):= Wide_Character'Val(ISO_8859_2_Conv(Character'Pos(s(i))));
      end case;
    end loop;
    return r;
  end ISO_8859_2_to_UTF_16;

  --  http://en.wikipedia.org/wiki/Windows-1252
  function Windows_1252_to_UTF_16(s: String) return UTF_16_String is
    r: UTF_16_String(s'Range);
  begin
    for i in s'Range loop
      case Character'Pos(s(i)) is
        when 16#80# => r(i):= Wide_Character'Val(16#20AC#);
        when 16#82# => r(i):= Wide_Character'Val(16#201A#);
        when 16#83# => r(i):= Wide_Character'Val(16#0192#);
        when 16#84# => r(i):= Wide_Character'Val(16#201E#);
        when 16#85# => r(i):= Wide_Character'Val(16#2026#);
        when 16#86# => r(i):= Wide_Character'Val(16#2020#);
        when 16#87# => r(i):= Wide_Character'Val(16#2021#);
        when 16#88# => r(i):= Wide_Character'Val(16#02C6#);
        when 16#89# => r(i):= Wide_Character'Val(16#2030#);
        when 16#8A# => r(i):= Wide_Character'Val(16#0160#);
        when 16#8B# => r(i):= Wide_Character'Val(16#2039#);
        when 16#8C# => r(i):= Wide_Character'Val(16#0152#);
        when 16#8E# => r(i):= Wide_Character'Val(16#017D#);
        when 16#91# => r(i):= Wide_Character'Val(16#2018#);
        when 16#92# => r(i):= Wide_Character'Val(16#2019#);
        when 16#93# => r(i):= Wide_Character'Val(16#201C#);
        when 16#94# => r(i):= Wide_Character'Val(16#201D#);
        when 16#95# => r(i):= Wide_Character'Val(16#2022#);
        when 16#96# => r(i):= Wide_Character'Val(16#2013#);
        when 16#97# => r(i):= Wide_Character'Val(16#2014#);
        when 16#98# => r(i):= Wide_Character'Val(16#02DC#);
        when 16#99# => r(i):= Wide_Character'Val(16#2122#);
        when 16#9A# => r(i):= Wide_Character'Val(16#0161#);
        when 16#9B# => r(i):= Wide_Character'Val(16#203A#);
        when 16#9C# => r(i):= Wide_Character'Val(16#0153#);
        when 16#9E# => r(i):= Wide_Character'Val(16#017E#);
        when 16#9F# => r(i):= Wide_Character'Val(16#0178#);
        when others =>
          r(i):= Wide_Character'Val(Character'Pos(s(i)));    --  trivial 8-bit to 16-bit copy
      end case;
    end loop;
    return r;
  end Windows_1252_to_UTF_16;

  function To_UTF_16(s: String; e: Encoding_choice) return UTF_16_String is
  begin
    case e is
      when utf_8 =>
        return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
      when iso_8859_1 =>
        return ISO_8859_1_to_UTF_16(s);
      when iso_8859_2 =>
        return ISO_8859_2_to_UTF_16(s);
      when windows_1252 =>
        return Windows_1252_to_UTF_16(s);
    end case;
  end To_UTF_16;

end Wasabee.Encoding;

