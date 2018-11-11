-- with Ada.Command_Line ; use Ada.Command_Line ;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Ada.Strings.Unbounded.Hash ;
with Ada.Containers.Hashed_Maps ;

package Wasabee.CSS is

   --
   -- Definissons quelques types bien utiles
   --

   package CSS_Properties is new Ada.Containers.Hashed_Maps (Unbounded_String,
                                                             Unbounded_String,
                                                             Hash,
                                                             "=");

   type CSS_Properties_Map_Ptr is access all CSS_Properties.Map ;


   package CSS_Dictionary is new Ada.Containers.Hashed_Maps (Unbounded_String,
                                                             CSS_Properties_Map_Ptr,
                                                             Hash, "=") ;

   -- Convention: Elements and keys are stored UPPER-case into maps.

   procedure Add_Or_Replace_CSS_Property (Map     : in out CSS_Dictionary.Map ;
                                          Element : String ;
                                          Key     : String ;
                                          Value   : String) ;

   function Exists_CSS_Element(Map : CSS_Dictionary.Map ; Element : String) return Boolean;

   -- Get a single CSS property

   function  Get_CSS_Property (Map     : CSS_Dictionary.Map ;
                               Element : String ;
                               Key     : String) return String ;

   --
   -- Vecteur de propriétés
   --

   function  Clean (Content : in Unbounded_String) return Unbounded_String ;

   procedure Read_CSS_File (S: String ; Css : in out Unbounded_String) ;

   --  Parse a single element's style properties
   procedure Parse_CSS_Element (Map : in out CSS_Dictionary.Map ;
                                Element : String ;
                                Declarations : Unbounded_String) ;

   --  Parse one or more elements' style properties
   --  Elements are separated by commas
   procedure Parse_CSS_Elements (Map : in out CSS_Dictionary.Map ;
                                 Elements : String ;
                                 Declarations : Unbounded_String) ;

   --  Parse a whole style sheet
   procedure Parse_Information (Map : in out CSS_Dictionary.Map ; Css : Unbounded_String) ;

   --  Trim blanks at ends and replace blanks by one space within S.
   --  Example:  " a   b (tab) c  "    becomes    "a b c"
   function Simplify_Blanks(S: String) return String;

end Wasabee.CSS;
