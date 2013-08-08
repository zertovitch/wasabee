package body Wasabee.Util is

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

end Wasabee.Util;
