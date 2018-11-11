--  Translate HTML entities like &nbsp; &lt; &gt; &amp; and so on.
--  References: http://www.w3.org/TR/html4/sgml/entities.html
--              http://dev.w3.org/html5/html-author/charref

with Wasabee.Encoding;                  use Wasabee.Encoding;

package Wasabee.Entities is
  
  --  Input:  "entity" of "&entity;"
  --  Output: the corresponding UTF-16 character, or "&entity;" if conversion failed
  function Convert_entity(e: UTF_16_String) return UTF_16_String;
  
  --  Translate a text containing HTML entities
  function Translate_character_entities(s: UTF_16_String) return UTF_16_String;

end Wasabee.Entities;
