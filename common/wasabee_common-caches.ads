--
-- All retrieval of internet contents should go through Get_contents
-- First, a Cache_item is searched with its URL, or a new one is created.
-- Then, call Get_contents to retrieve contents.
--

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;

package Wasabee_common.Caches is

  -- A variable of type Cache_type contains the entire browser cache,
  -- with URLs cached in files, memory, not neither (to be loaded)

  type Cache_type is private;

  procedure Get_contents(
    cache    : in out Cache_type;
    URL      : in     Unbounded_String;
    reload   : in     Boolean
  );

  function Object_count(c: Cache_type) return Natural;

private

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

  package Cache_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Cache_item
  );

  -- Quick search by URL

  package URL_catalogues is new Ada.Containers.Hashed_Maps
    (Key_Type        => Unbounded_String,
     Element_Type    => Positive,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."="
    );

  -- Quick sort by latest_hit time - for reducing cache size when needed

  package Access_time_catalogues is new Ada.Containers.Ordered_Maps
    (Key_Type        => Ada.Calendar.Time,
     Element_Type    => Positive
    );

  type Cache_type is record
    data    : Cache_Vectors.Vector;
    URL_cat : URL_catalogues.Map;
    hit_cat : Access_time_catalogues.Map;
  end record;

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
