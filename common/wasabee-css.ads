with Ada.Text_IO ; use Ada.Text_IO ;
with Ada.Command_Line ; use Ada.Command_Line ;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Ada.Containers.Vectors ;

package Wasabee.Css is

   --
   Css : Unbounded_String ;
   --

   type CSS_Property is record
      Key   : Unbounded_String ;
      Value : Unbounded_String ;
   end record ;
   package CSS_Properties is new Ada.Containers.Vectors (Element_Type => CSS_Property,
                                                         Index_Type => Positive) ;
   -- Les properties generale de la page
   General_Properties : access CSS_Properties.Vector ;
   type Css_Properties_Ptr is access all CSS_Properties.Vector ;
   type CSS_Dictionary_Entry is record
      Key   : Unbounded_String ;
      Value : CSS_Properties.Vector ;
   end record ;
   type CDE_Ptr is access all CSS_Dictionary_Entry ;
   package Css_Dictionary is new Ada.Containers.Vectors (Element_Type => CSS_Dictionary_Entry,
                                                         Index_Type => Positive) ;
   function Get_CSS_Value (Object : String ; Key : String) return String ;

   --
   -- Vecteur de propriétés
   --

   function  Clean (Content : in Unbounded_String) return Unbounded_String ;

   procedure Read_CSS_File (S: String) ;

   procedure Parse_CSS_Element (Element : Unbounded_String) ;

   procedure Parse_Information ;






end ;

