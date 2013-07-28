--
-- All retrieval of internet contents should go through Get_contents
-- First, a Cache_item is searched with its URL, or a new one is created.
-- Then, call Get_contents to retrieve contents.
--

with Ada.Calendar;                      use Ada.Calendar;

package Wasabee_common.Caches is

  type Cache_item is record
    URL,
    contents,
    file_name  : Unbounded_String;
    first_hit,
    latest_hit : Time;
    hits       : Natural;
  end record;

  -- contents = "" means item is not (yet) in memory
  -- file_name = "" means item is not (yet) in a file

  procedure Get_contents(
    item     : in out Cache_item;
    contents :    out Unbounded_String
  );

  -- During idle times, or eventually when closing Wasabee,
  -- we save memory cache to files if not yet done.

  procedure Save_to_file(item: in out Cache_item);

end Wasabee_common.Caches;

--  Below a tagged version - a "wrong good idea"...

--  type Abstract_cache_item is abstract tagged record
--    URL        : Unbounded_String;
--    first_hit,
--    latest_hit : Time;
--    hits       : Natural;
--  end record;
--
--  function Get_contents(item: Abstract_cache_item) return Unbounded_String
--  is abstract;
--
--  type Memory_cache_item is new Abstract_cache_item with record
--    contents   : Unbounded_String;
--  end record;
--
--  overriding
--  function Get_contents(item: Memory_cache_item) return Unbounded_String;
--
--  type File_cache_item is new Abstract_cache_item with record
--    file_name  : Unbounded_String;
--  end record;
--
--  overriding
--  function Get_contents(item: File_cache_item) return Unbounded_String;
