with Ada.Directories, Ada.Environment_Variables, Ada.Sequential_IO;
with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;

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

  function Create_new_item(URL: Unbounded_String) return Cache_item is
    new_item: Cache_item;
    now: constant Time:= Clock;
  begin
    new_item.URL:= URL;
    new_item.first_hit:= now;
    new_item.latest_hit:= now;
    new_item.hits:= 0;
    return new_item;
  end Create_new_item;

  procedure Get_item_contents_from_Web(item: in out Cache_item) is
    now: constant Time:= Clock;
  begin
    item.contents:= U("from URL..."); -- load item.contents from URL !!
    item.uncompressed_contents:= Null_Unbounded_String;
    if Ada.Directories.Exists(S(item.file_name)) then
      Ada.Directories.Delete_File(S(item.file_name));
      item.file_name:= U("");
    end if;
    item.first_hit:= now;
    item.latest_hit:= now;
    item.hits:= 1;
  end Get_item_contents_from_Web;

  procedure Get_item_contents(item: in out Cache_item) is
  begin
    if item.contents = "" then
      if item.file_name = "" then
        Get_item_contents_from_Web(item);
        return;
      else
        -- Read from file
        begin
          item.contents:= U(Load(S(item.file_name)));
          item.uncompressed_contents:= Null_Unbounded_String;
        exception
          when Name_Error | Use_Error =>
            -- Something wrong with the file...
            item.file_name:= Null_Unbounded_String;
            Get_item_contents_from_Web(item);
            return;
        end;
      end if;
    end if;
    item.hits:= item.hits + 1;
    item.latest_hit:= Clock;
  end Get_item_contents;

  procedure Get_contents(
    cache    : in out Cache_type;
    URL      : in     Unbounded_String;
    reload   : in     Boolean
  )
  is
    idx: Positive;
    new_item: Cache_item;
  begin
    begin
      idx:= cache.URL_cat.Element(URL);
    exception
      when Constraint_Error =>
        new_item:= Create_new_item(URL);
        cache.data.Append(new_item);
        idx:= cache.URL_cat.Element(URL);
        -- Update catalogues
        cache.URL_cat.Insert(URL, idx);
        cache.hit_cat.Insert(new_item.latest_hit, idx);
    end;
    if reload then
      cache.data.Update_Element(idx, Get_item_contents_from_Web'Access);
    else
      cache.data.Update_Element(idx, Get_item_contents'Access);
    end if;
  end Get_contents;

  wasa_file_cache_path : constant String:=
    Ada.Environment_Variables.Value("TEMP") & "/wasabee_temp";

  function Available_cache_item_name return String is
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
    Ada.Directories.Create_Path(wasa_file_cache_path);
    for i in Positive loop
      declare
        num0: constant String:= Integer'Image(i);
        num: constant String:= num0(num0'First+1 .. num0'Last);
        -- ^ Skip the @#*% leading space
        test_name: constant String:=
          wasa_file_cache_path & "/wasa_" & stamp & '_' & num & ".cache";
      begin
        if not Ada.Directories.Exists(test_name) then
          return test_name;
        end if;
      end;
    end loop;
    raise Constraint_Error with "No free file name!";
  end Available_cache_item_name;

  -- During idle times, or eventually when closing Wasabee,
  -- we save memory cache to files if not yet done.

  procedure Save_to_file(item: in out Cache_item) is
  begin
    if item.file_name = "" then
      item.file_name:= U(Available_cache_item_name);
      Save(S(item.file_name), S(item.contents));
    end if;
  end Save_to_file;

  function Object_count(c: Cache_type) return Natural is
  begin
    return Integer(c.data.Length);
  end Object_count;

end Wasabee_common.Caches;
