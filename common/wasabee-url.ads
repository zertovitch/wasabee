with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with GNAT.Sockets ; use GNAT.Sockets ; 

package Wasabee.URL is

   type Split_URL is record
      Protocole  : Unbounded_String ; -- http, file, ...
      Host       : Unbounded_String ;
      Port       : Port_Type ;  -- 80 generalement
      Ressource  : Unbounded_String ;
   end record;

   procedure Display_URL_details (U : in Split_URL) ;

   procedure Decode (Adr : in Unbounded_String ; U : in out Split_URL) ;

  --------------------------
  -- Manipulation of URLs --
  --------------------------

  -- Build_URL composes a complete URL out of a base, complete URL (typically
  -- of a web page), and of a partial URL (typically of a linked page or
  -- an image). This necessary for resolving HREF="..." and SRC="..." attributes.
  -- NB: Build_URL does not simplify the URL - see function Simplify_URL
  -- for that.

  -- case [1]: absolute-with-website path given as the partial URL
  -- case [2]: absolute path given as the partial URL
  -- case [3]: partial URL is just an anchor
  -- case [4]: relative path

  function Build_URL (complete_URL, partial_URL: String) return String;

  -- Examples:
  -- ========
  -- [1] http://en.wikipedia.org/wiki/UNIX                ,  //fr.wikipedia.org/wiki/Unix
  -- [2] http://en.wikipedia.org/wiki/OpenVMS             ,  /wiki/Itanium
  -- [3] http://gen-img-dec.sourceforge.net/index.html    ,  #anchor
  -- [4] http://gen-img-dec.sourceforge.net/[index.html]  ,  stickman.gif

  Build_URL_error: exception;

  -- Simplify_URL turns blabla/../bloblo into bloblo and ./bloblo into bloblo
  -- If the protocol is file://, the original URL is returned, since the OS
  -- is able to do the job and there might be more ../ than directory levels
  -- in the original URL, like file://../tests/example1.html

  function Simplify_URL(s: String) return String;
  function Remove_anchor(s: String) return String; -- remove the eventual "#some_name" from an URL
  function Anchor_only(s: String) return String;   -- give the eventual anchor; "" if none

  --

  procedure Test (U : String) ;

  procedure General_Test ;

end ;
