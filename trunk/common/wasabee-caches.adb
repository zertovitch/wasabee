with Wasabee.Util;                      use Wasabee.Util;

with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;
with Ada.Directories,
     Ada.Environment_Variables,
     Ada.Sequential_IO;
with Ada.Unchecked_Deallocation;

-- with Ada.Streams.Stream_IO;

package body Wasabee.Caches is

  -----------------------
  -- Blocking file I/O --
  -----------------------

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
  pragma Unreferenced (Load);

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
  pragma Unreferenced (Save);

  -----------------------------------------------------
  -- Blocking version of retrieval of a Web resource --
  -----------------------------------------------------
  -- !! will use the non-blocking version and wait for completion

  procedure Blocking_retrieval (the_URL : in String ; contents : out Unbounded_String)
  renames Wasabee.Request.Retrieve_from_URL;
  pragma Unreferenced (Blocking_retrieval);

  ---------------------------
  -- Cache item management --
  ---------------------------

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

  procedure Start_Load(item: in out Cache_item) is
  begin
    -- Cache_item is trivial, so actual loading is not needed
    item.status:= loaded;
  end Start_Load;

  -- Get from web, including file:// protocol

  procedure Get_item_contents_from_Web(wrp: in out Cache_item_wrapper) is
    item: Cache_item'Class renames wrp.pointer.all;
    now: constant Time:= Clock;
  begin
    Start_Load(item);
    if item.loading_is_blocking then
      while item.status /= loaded loop
        null;
      end loop;
    end if;
    if Ada.Directories.Exists(S(item.file_name)) then
      Ada.Directories.Delete_File(S(item.file_name));
      item.file_name:= U("");
    end if;
    item.first_hit:= now;
    item.latest_hit:= now;
    item.hits:= 1;
  end Get_item_contents_from_Web;

  procedure Get_item_contents_from_File_cache(wrp: in out Cache_item_wrapper) is
    item: Cache_item'Class renames wrp.pointer.all;
    temp_URL: constant Unbounded_String:= U(""); -- !! create here a temporary file:// URL
    true_URL: constant Unbounded_String:= item.URL;
  begin
    item.URL:= temp_URL;
    Get_item_contents_from_Web(wrp);
    item.URL:= true_URL; -- restore true URL
  exception
    when Name_Error | Use_Error =>
      -- Something went wrong with the file...
      item.file_name:= Null_Unbounded_String; -- forget the cache file
      item.URL:= true_URL; -- restore true URL
      Get_item_contents_from_Web(wrp);
      return;
  end Get_item_contents_from_File_cache;

  procedure Get_item_contents(wrp: in out Cache_item_wrapper) is
    item: Cache_item'Class renames wrp.pointer.all;
  begin
    if item.status = not_loaded then
      if item.file_name = "" then
        Get_item_contents_from_Web(wrp);
        return;
      else
        -- Read from cache file
        Get_item_contents_from_File_cache(wrp);
      end if;
    end if;
    item.hits:= item.hits + 1;
    item.latest_hit:= Clock;
  end Get_item_contents;

  procedure Get_contents(
    item     : in     p_Cache_item;     -- item is already allocated with the
                                        -- appropriate type derivation. If
                                        -- the item is already in the cache,
                                        -- the pointer will be freed.
    cache    : in out Cache_type;       -- Container of all items
    URL      : in     Unbounded_String;
    blocking : in     Boolean;          -- Wait for contents to be fully loaded ?
    reload   : in     Boolean           -- Force reload of resource
  )
  is
    idx: Positive;
    wrp: Cache_item_wrapper;
  begin
    -- First, a Cache_item is searched with its URL, or a new one is created.
    begin
      idx:= cache.URL_cat.Element(URL);
    exception
      when Constraint_Error =>
        Cache_item(item.all):= Create_new_item(URL);
        -- We add the item to the container
        wrp.pointer:= item;
        cache.data.Append(wrp);
        idx:= cache.data.Last_Index;
        -- Update catalogues
        cache.URL_cat.Insert(URL, idx);
        cache.hit_cat.Insert(item.latest_hit, idx);
    end;
    item.loading_is_blocking:= blocking;
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
    url: constant String:= S(item.URL);
  begin
    if url'Length >= 4 and then url(url'First..url'First+3) /= "file" and then
      item.file_name = ""
    then
      item.file_name:= U(Available_cache_item_name);
      -- Save(S(item.file_name), S(item.contents)); -- !! get contents: inherited
    end if;
  end Save_to_file;
  pragma Unreferenced (Save_to_file);

  function Object_count(c: Cache_type) return Natural is
  begin
    return Integer(c.data.Length);
  end Object_count;

  procedure Finalize(Object : in out Cache_item_wrapper) is
    procedure Dispose is new Ada.Unchecked_Deallocation(Cache_item'Class, p_Cache_item);
  begin
    Dispose(Object.pointer);
  end Finalize;


end Wasabee.Caches;
