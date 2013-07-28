with Ada.Directories, Ada.Environment_Variables, Ada.Sequential_IO;

package body Wasabee_common.Caches is

  function Load(file_name: String) return String is
    size: constant Ada.Directories.File_Size:= Ada.Directories.Size(file_name);
    subtype Contents_type is String(1..Natural(size));
    package SIO is new Ada.Sequential_IO(Contents_type);
    contents: Contents_type;
    use SIO;
    file: File_Type;
  begin
    Open(file, In_File, file_name);
    Read(file, contents);
    Close(file);
    return contents;
  end Load;

  procedure Save(file_name, contents: String) is
    subtype Contents_type is String(contents'Range);
    package SIO is new Ada.Sequential_IO(Contents_type);
    use SIO;
    file: File_Type;
  begin
    Create(file, Out_File, file_name);
    Write(file, contents);
    Close(file);
  end Save;

  procedure Get_contents(
    item     : in out Cache_item;
    contents :    out Unbounded_String
  )
  is
  begin
    if item.contents = "" then
      if item.file_name = "" then
        item.contents:= U("from URL..."); -- load item.contents from URL !!
        item.first_hit:= Clock;
        item.hits:= 0;
      else
        item.contents:= U(Load(S(item.file_name)));
      end if;
    end if;
    contents:= item.contents;
    item.hits:= item.hits + 1;
    item.latest_hit:= Clock;
  end Get_contents;

  function Free_cache_item_name return String is
    T  : constant Time:= Clock;
    sY : constant String:= Integer'Image( Year(T));
    sM : constant String:= Integer'Image( Month(T) + 100);
    sD : constant String:= Integer'Image( Day(T) + 100);
    sS : constant String:= Duration'Image( Seconds(T) );
    stamp: constant String:=
    sY( sY'Last-3 .. sY'Last ) & '-' &
    sM( sM'Last-1 .. sM'Last ) & '-' &
    sD( sD'Last-1 .. sD'Last ) &
    sS( sS'First+1 .. sS'Last );
  begin
    for i in Positive loop
      declare
        num0: constant String:= Integer'Image(i);
        num: constant String:= num0(num0'First+1 .. num0'Last);
        -- ^ Skip the @#*% leading space
        test_name: constant String:=
          Ada.Environment_Variables.Value("TEMP") &
          "/wasa_" & stamp & '_' & num & ".cache";
      begin
        if not Ada.Directories.Exists(test_name) then
          return test_name;
        end if;
      end;
    end loop;
    raise Constraint_Error with "No free file name!";
  end Free_cache_item_name;

  procedure Save_to_file(item: in out Cache_item) is
  begin
    if item.file_name = "" then
      item.file_name:= U(Free_cache_item_name);
      Save(S(item.file_name), S(item.contents));
    end if;
  end Save_to_file;

end Wasabee_common.Caches;
