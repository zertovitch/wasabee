package body Wasabee.Util is

  function Version return String is
  begin
    return "WASABEE v" & Version_number;
  end Version;

  function Version_number return String is
  begin
    return "0.0.1";
  end Version_number;

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
