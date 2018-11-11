--
-- Wasabee memory and file cache management
--
-- All retrieval of Internet contents should go through Get_contents.
--

-- with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Request;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;
with Ada.Finalization;

with Interfaces;                        use Interfaces;

package Wasabee.Caches is

  -- A variable of type Cache_type contains the entire browser cache,
  -- with URLs cached in files, memory, not neither (to be loaded)

  type Cache_type is private;
  function Object_count(c: Cache_type) return Natural;

  type Cache_item is tagged private;

  procedure Start_Load(item: in out Cache_item);
  -- Only derived types will do anything non trivial

  -- Cache_item can be a Web page, an image, etc.

  type p_Cache_item is access Cache_item'Class;

  procedure Get_contents(
    item     : in     p_Cache_item;     -- item is already allocated with the
                                        -- appropriate type derivation. If
                                        -- the item is already in the cache,
                                        -- the pointer will be freed.
    cache    : in out Cache_type;       -- Container of all items
    URL      : in     Unbounded_String;
    blocking : in     Boolean;          -- Wait for contents to be fully loaded ?
    reload   : in     Boolean           -- Force reload of resource
  );

private

  type Loading_status is (not_loaded, being_loaded, loaded);

  type File_cache_item is tagged record
    file_name              : Unbounded_String; -- "" means item is not (yet) in a file
    crc_32                 : Unsigned_32; -- protect against file tampering
    first_hit,
    latest_hit             : Time;
    hits                   : Natural;
  end record;

  type Cache_item is new File_cache_item with record
    URL                    : Unbounded_String;
    connect                : Wasabee.Request.p_Connection;
    status                 : Loading_status:= not_loaded;
    pragma Volatile(status); -- will be changed by loader task
    loading_is_blocking    : Boolean;
  end record;

  -- Just a wrapper to ensure we do not get a memory leak when cache is reduced.
  type Cache_item_wrapper is new Ada.Finalization.Controlled with record
    pointer: p_Cache_item;
  end record;

  overriding procedure Finalize(Object : in out Cache_item_wrapper);

  package Cache_Vectors is new Ada.Containers.Vectors(
    Index_Type   => Positive,
    Element_Type => Cache_item_wrapper
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

end Wasabee.Caches;
