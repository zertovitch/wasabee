with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;

package body Wasabee.Entities is

  -- Translate &nbsp; &lt; &gt; &amp; and so on.
  -- http://dev.w3.org/html5/html-author/charref
  -- http://www.w3.org/TR/html4/sgml/entities.html

  type Entity is 
    ( nbsp, thinsp, -- spaces
      lt, gt, amp, quot,
      --
      copy, reg, trade,
      yen,
      para,
      hellip, middot,
      ndash, mdash,
      lrm, rlm,
      laquo, raquo,
      lsaquo, rsaquo,
      lsquo, rsquo,
      hearts,
      --  Accented vowels
      acirc, agrave, atilde, 
      eacute, Eacute_2, ecirc, Ecirc_2, egrave, 
      icirc, iuml,
      ocirc, ouml, 
      ucirc, ugrave, uuml,
      --  Accented consonants
      ccedil, Ccedil_2
    );
    
  conversion: constant array(Entity) of Wide_Character:=
  (
    nbsp     => Wide_Character'Val(160),  --  non-breaking space
    thinsp   => Wide_Character'Val(8201), --  thin space
    lt       => '<',
    gt       => '>',
    amp      => '&',
    quot     => '"',
    --
    --  The following entities should be theoretically deprecated nowadays
    --  thanks to encoding (UTF-8 and so on), but we still find some when
    --  browsing with Wasabee...
    --
    copy     => Wide_Character'Val(169),    --  copyright
    reg      => Wide_Character'Val(174),    --  registered trademark
    trade    => Wide_Character'Val(8482),   --  trademark
    yen      => Wide_Character'Val(165),
    hellip   => Wide_Character'Val(8230),   --  ellipsis (...)
    para     => Wide_Character'Val(182),    --  paragraph sign
    middot   => Wide_Character'Val(183),    --  middle dot
    ndash    => Wide_Character'Val(8211),   --  en dash
    mdash    => Wide_Character'Val(8212),   --  em dash
    lrm      => Wide_Character'Val(8206),   --  left-to-right mark
    rlm      => Wide_Character'Val(8207),   --  right-to-left mark
    laquo    => Wide_Character'Val(171),    --  left pointing guillemet
    raquo    => Wide_Character'Val(187),    --  right pointing guillemet
    lsquo    => Wide_Character'Val(8216),   --  left single quotation mark
    rsquo    => Wide_Character'Val(8217),   --  right single quotation mark
    lsaquo   => Wide_Character'Val(8249),   --  single left-pointing angle quotation mark
    rsaquo   => Wide_Character'Val(8250),   --  single right-pointing angle quotation mark
    hearts   => Wide_Character'Val(9829),
    --  Accented vowels
    acirc    => Wide_Character'Val(226),
    agrave   => Wide_Character'Val(224),
    atilde   => Wide_Character'Val(227),
    eacute   => Wide_Character'Val(233),
    Eacute_2 => Wide_Character'Val(201),
    ecirc    => Wide_Character'Val(234),
    Ecirc_2  => Wide_Character'Val(202),
    egrave   => Wide_Character'Val(232),
    icirc    => Wide_Character'Val(238),
    iuml     => Wide_Character'Val(239),
    ocirc    => Wide_Character'Val(244),
    ouml     => Wide_Character'Val(246),
    ucirc    => Wide_Character'Val(219),
    ugrave   => Wide_Character'Val(249),
    uuml     => Wide_Character'Val(252),
    --  Accented consonants
    ccedil   => Wide_Character'Val(231),
    Ccedil_2 => Wide_Character'Val(199)
  );

  function Convert_entity(e: UTF_16_String) return UTF_16_String is
    n: Positive;
    ent: Entity;
    all_lower: Boolean;
  begin
    if e'Length >= 2 and then e(e'First)='#' then  --  &#entity_number;
      if e(e'First+1) = 'x' then  --  hexadecimal
        n:= Integer'Wide_Value("16#" & e(e'First+2..e'Last) & '#');
      else                        --  decimal
        n:= Integer'Wide_Value(e(e'First+1..e'Last));
      end if;
      return (1=> Wide_Character'Val(n));
    else
      all_lower:= True;
      for c of e loop
        all_lower:= all_lower and Is_Lower(c);
      end loop;
      if all_lower then
        ent := Entity'Wide_Value(e);
      else
        ent := Entity'Wide_Value(e & "_2");
      end if;
      return (1=> conversion(ent));
    end if;
  exception
    when Constraint_Error =>
      return '&' & e & ';'; -- No clue - give back entity itself
  end;
  
  function Translate_character_entities(s: UTF_16_String) return UTF_16_String is
    a: Integer:= 0;
  begin
    for i in s'Range loop
      case s(i) is
        when '&' =>
          a:= i;
        when ';' =>
          if a > 0 then -- &...; pair found
            return s(s'First..a-1) & Convert_entity(s(a+1..i-1)) &
                   Translate_character_entities(s(i+1..s'Last));
          end if;
        when others =>
          null;
      end case;
    end loop;
    -- No &...; pair found
    return s;
  end Translate_character_entities;

end Wasabee.Entities;
