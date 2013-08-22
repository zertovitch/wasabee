with Ada.Text_IO ; use Ada.Text_IO ;
with Ada.Command_Line ; use Ada.Command_Line ;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Ada.Containers.Vectors ;
with Ada.Strings.Unbounded.Hash ; 
with Ada.Containers.Hashed_Maps ;

package Wasabee.Css is
   
   --
   -- Definissions quelques types bien utiles
   --
   
   package CSS_Properties is new Ada.Containers.Hashed_Maps (Unbounded_String,
							     Unbounded_String,
							     Hash,
							     "=");
   
   use CSS_Properties ;
   
   type CSS_Properties_Map_Ptr is access all CSS_Properties.Map ;
   
   
   package CSS_Dictionary is new Ada.Containers.Hashed_Maps (Unbounded_String,
							     CSS_Properties_Map_Ptr,
							     Hash, "=") ;
   
   Css_Page_Dictionary : CSS_Dictionary.Map ;
   
   procedure Add_Or_Replace_CSS (Element : String ; Key : String ; Value : String) ;
   
   function  Get_CSS (Element : String ; Key : String) return String ;
   
   --
   Css : Unbounded_String ;
   --

   -- Les properties generale de la page
   General_Properties : access CSS_Properties.Map ;
   
   --
   -- Vecteur de propriétés
   --

   function  Clean (Content : in Unbounded_String) return Unbounded_String ;

   procedure Read_CSS_File (S: String) ;

   procedure Parse_CSS_Element (TKey : String ; Element : Unbounded_String) ;

   procedure Parse_Information ;

   procedure Set_CSS_Value ( Content : String ) ;

end ;

