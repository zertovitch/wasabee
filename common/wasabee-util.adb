with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

package body Wasabee.Util is

  function Version return String is
  begin
    return "WASABEE v" & Version_number;
  end Version;

  function Version_number return String is
  begin
    return "0.0.1";
  end Version_number;

  function Build_URL (complete_URL, partial_URL: String) return String is
    first_slash: Integer:= complete_URL'First - 1;  -- first /
    last_slash : Integer:= complete_URL'Last + 1;   -- last /
    protocol   : Integer:= complete_URL'First - 1;  -- //
  begin
    if partial_URL'Length = 0 then
      return complete_URL; -- partial URL is empty: well, use the complete one...
    elsif Index(partial_URL, "//") > 0 then
      return partial_URL;  -- partial URL is actually a complete one: use it!
    end if;
    for i in complete_URL'Range loop
      if complete_URL(i) = '/' then
        if last_slash = i - 1 and protocol < complete_URL'First then
          protocol:= last_slash; -- capture the first //
        end if;
        last_slash:= i;
      end if;
    end loop;
    if protocol < complete_URL'First then
      raise Build_URL_error with "In complete_URL, ""//"" is missing, like ""http://""";
    end if;
    for i in reverse protocol + 2 .. complete_URL'Last loop
      if complete_URL(i) = '/' then
        first_slash:= i;
      end if;
    end loop;
    if partial_URL(partial_URL'First) = '/' then -- absolute path given as the partial URL
      return complete_URL(complete_URL'First..first_slash-1) & partial_URL;
    end if;
    -- !! relative paths
    return complete_URL; -- !! wrong !! lazy !!
  end Build_URL;

  function Filter_blanks (s : Wide_String) return Wide_String is
    t: Wide_String(s'Range);
    j: Integer:= t'First-1;
    previous_blank: Boolean:= False;
  begin
    for i in s'Range loop
      case s(i) is
        when ' ' | Wide_Character'Val(9) | Wide_Character'Val(10) | Wide_Character'Val(13) =>
          if previous_blank then
            null; -- do not add anything
          else
            j:= j + 1;
            t(j):= ' ';
          end if;
          previous_blank:= True;
        when others =>
          j:= j + 1;
          t(j):= s(i);
          previous_blank:= False;
       end case;
    end loop;
    return t(t'First .. j);
  end Filter_blanks;

  function Get_hex_value(s: String) return Natural is -- NB: up to 31 bit; usually 24 bit.
  begin
    return Integer'Value("16" & s & '#');
  exception
    when others =>
      return 0;
  end Get_hex_value;

end Wasabee.Util;
