with Ada.Text_IO;                       use Ada.Text_IO;

package body Wasabee.Util is

  function Version return String is
  begin
    return "Wasabee v" & Version_number;
  end Version;

  function Version_number return String is
  begin
    return "0.0.2";
  end Version_number;

  function Filter_blanks (s : Wide_String; fully: Boolean) return Wide_String is
    t: Wide_String(s'Range);
    j: Integer:= t'First-1;
    previous_blank: Boolean:= False;
  begin
    for i in s'Range loop
      if fully then
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
      else
        case s(i) is
          when Wide_Character'Val(9) | Wide_Character'Val(13) =>
            null;
          when others =>
            j:= j + 1;
            t(j):= s(i);
        end case;
       end if;
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

  function To_Enum_func(key: String) return Enum is
    s: String:= key;
  begin
    for i in s'Range loop
      if s(i)='-' then s(i):= '_'; end if;
    end loop;
    return Enum'Value(s);
  exception
    when Constraint_Error =>
      return default;
  end To_Enum_func;

  procedure To_Enum_proc(key: String; variable: in out Enum) is
    s: String:= key;
    old: Enum;
  begin
    for i in s'Range loop
      if s(i)='-' then s(i):= '_'; end if;
    end loop;
    old:= variable;
    variable:= Enum'Value(s);
  exception
    when Constraint_Error =>
      variable:= old;  --  keep existing value in variable
  end To_Enum_proc;

  procedure Dump_string(file_name, s: String) is
    f: File_Type;
  begin
    Create(f, Out_File, file_name);
    Put(f, s);
    Close(f);
  end Dump_string;

  procedure Inc(x: in out Integer) is
  begin
    x:= x + 1;
  end;

  procedure Dec(x: in out Integer) is
  begin
    x:= x - 1;
  end;

  procedure Add(x: in out Integer; y: Integer) is
  begin
    x:= x + y;
  end;

  procedure Sub(x: in out Integer; y: Integer) is
  begin
    x:= x - y;
  end;

  procedure Min(x: in out Integer; y: Integer) is
  begin
    x:= Integer'Min(x, y);
  end;

  procedure Max(x: in out Integer; y: Integer) is
  begin
    x:= Integer'Max(x, y);
  end;

end Wasabee.Util;
