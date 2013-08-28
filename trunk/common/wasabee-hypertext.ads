with Wasabee.CSS;
with Wasabee.Images ; use Wasabee.Images ;
with GID ; use GID ;

with DOM.Core;

with Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;

package Wasabee.Hypertext is

   subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
   subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

   subtype Color_Code is Integer range -1 .. 2**24-1;
   -- solid 24-bit RGB color, plus a -1 value for default color
   Default_color: constant Color_Code:= -1;
   Black        : constant Color_Code:= 0;
   White        : constant Color_Code:= 16#FF_FF_FF#;

   subtype Font_face_name is Unbounded_String;

   type Font_modifier is (bold, italic, underlined, strikethrough);
   type Font_modifier_switch is array(Font_modifier) of Boolean;

   type Font_descriptor is record
      face          : Font_face_name;
      size          : Positive;
      modifier      : Font_modifier_switch;
   end record;

   type Style is record
      text_color : Color_Code;
      font       : Font_Descriptor;
      bg_color   : Color_Code;
      -- more to come...
   end record;

   type p_Style is access Style;

   type Point is record x, y: Natural; end record;
   type Box is record p1, p2: Point; end record; -- p1.x <= p2.x, p1.y <= p2.y for non-empty boxes
   function Max(b1, b2: Box) return Box; -- return the smallest box containing boxes b1 and b2
   pragma Inline(Max);

   --------------------------------------------------------------
   -- This is the HyperText object. It contains all the useful --
   -- informations for displaying a web page or frame.         --
   -- Its structure is close to the HTML language.             --
   --------------------------------------------------------------

   type HT_object is new Ada.Finalization.Controlled with private;

   -- Load an HT object from an XML tree (for the XHTML prototype)
   procedure Load_frame(ho: in out HT_object; from: DOM.Core.Node_List);

   -- Load an HT object from any stream
   -- procedure Load_frame(ho: in out HT_object; from: Root_Stream_Type'Class);

   function Title(ho: HT_object) return UTF_16_String;

   function Bounding_box(ho: HT_object) return Box;

   procedure Dump(ho: HT_object; file: Ada.Wide_Text_IO.File_Type);

private

   type Body_kind is (
                      -- Text or singleton tags
                      text,
                      hr, br,
                      -- Normal tags
                      a,
                      b,i,u,strike,s,
                      strong, em, dfn, var,
                      big, small,
                      sup, sub,
                      code, samp, kbd, tt,
                      del, ins, abbr, acronym, cite, blockquote,
                      article, aside, figure, figcaption,
                      address, nav, q, dl, dt, dd,
                      details, summary,
                      font,
                      h1, h2, h3, h4, h5, h6,
                      p, div, span,
                      ul, ol, Li, -- lists
                      img
                     );

   subtype HTML_tag is Body_kind range hr .. Body_kind'Last;
   subtype Singleton_tag is HTML_tag range HTML_tag'First .. br;
   -- missing singletons so far:
   -- <area> <base> <basefont> <col> <frame> <img> <input> <isindex> <link> <meta> <param>

   subtype Text_or_singleton_tag is Body_kind range Body_kind'First .. Singleton_tag'Last;
   subtype Normal_tag is HTML_tag range HTML_tag'Succ(Singleton_tag'Last) .. HTML_tag'Last;
   subtype Normal_tag_no_a is HTML_tag range b .. HTML_tag'Last;

   type Body_node;
   type p_Body_node is access Body_node;

   type Body_Node(kind: Body_kind) is record
      bounding_box   : Box;
      optional_style : p_Style; -- if null, the parent style prevails
      next           : aliased p_Body_node:= null; -- Next sibling
      case kind is
         -- Text or singleton tags
         when text       => content: UTF_16_Unbounded_String;
         when hr         =>
            hr_height : Natural:= 2;
         when br         => null;

            -- Normal tags
         when Normal_tag =>
            first_child: aliased p_Body_node:= null;
            case kind is
               when a =>
                  URL   : Unbounded_String;
               when font => -- !! we'll remove this and use an optional_style
                  face  : Font_face_name;
                  color : Color_Code:= Default_color;
               when img =>
                  Src   : Unbounded_String ;
                  Desc  : GID.Image_Descriptor ;
               when others =>
                  null;
            end case;
      end case;
   end record;

   type HT_object is new Ada.Finalization.Controlled with record
      title             : UTF_16_Unbounded_String;
      main_bounding_box : Box;
      style_map         : Wasabee.CSS.CSS_Dictionary.Map;
      the_body          : aliased p_Body_node;
   end record;

   overriding
     procedure Finalize(ho: in out HT_object);

end Wasabee.Hypertext;
