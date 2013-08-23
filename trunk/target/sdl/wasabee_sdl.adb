with Wasabee;                           use Wasabee;

with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Request;                   use Wasabee.Request;
with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;


with Ada.Characters.Handling ; use Ada.Characters.Handling ;
with Ada.Containers.Vectors ;


with Ada.Text_IO;                       

with DOM.Core;

with SDL_SDL_H ; use SDL_SDL_H ; 
with SDL_SDL_Video_H ; use SDL_SDL_Video_H ; 
with SDL_SDL_Events_H ; use SDL_SDL_Events_H ; 
with SDL_SDL_Ttf_H; use SDL_SDL_Ttf_H;
with SDL_SDL_Stdinc_H ; use SDL_SDL_Stdinc_H ;
  
with Interfaces ; use Interfaces ;
with Interfaces.C ; use Interfaces.C ; 
with Interfaces.C.Strings ; use Interfaces.C.Strings ;

with System ; use System ; 

procedure Wasabee_Sdl is
   
   
   package SDL_Fonts_Vector is new Ada.Containers.Vectors(Positive,System.Address,"=") ;

   Fonts : SDL_Fonts_Vector.Vector ;
   
   Current_Font : System.Address ;
   
   Current_Color : Color_Code ;
   
   --
   -- Declaration
   --
   
   type SDL_Plane is new Frame_Plane with record
      
      Screen : access SDL_Surface ;
      
   end record ;
   
   overriding procedure Clear_area(on: in out SDL_plane);
   overriding procedure Area_size (on: SDL_plane; w,h: out Natural);
   overriding procedure Extend_area_height (on: in out SDL_plane; to: Natural) is null;
   
   overriding procedure Create_target_font(
					   on         : in out SDL_plane;
					   descriptor : in     Font_descriptor;
					   new_index  : in     Positive
					  ) ;
   overriding procedure Select_target_font(
					   on         : in out SDL_plane;
					   index      : in     Positive
					  ) ;
   overriding procedure Destroy_target_fonts(on: in out SDL_plane) ;
   overriding procedure Text_XY(on: in out SDL_plane; x,y: Integer; text: UTF_16_String);
   overriding procedure Text_size (
				   on   : in out SDL_plane;
				   text : in     UTF_16_String;
				   x,y  :    out Natural
				  );
   overriding procedure Select_target_text_color(
						 on: in out SDL_plane;
						 code: in Color_Code
						) ;
   
   
   --
   -- Implementation
   --
   
   
   Window : SDL_Plane ;
   
   use SDL_Fonts_Vector ;   
   
   procedure Clear_area(on: in out SDL_plane) is
      Ret : Int ;
   begin
      --Put_Line("********************************************************");
      -- Ada.Text_IO.Put_Line("__Clear_aera__") ;       
      Ret := SDL_FillRect (Window.Screen, Window.Screen.Clip_Rect'Access, 16#FFFFFF#) ; 
   end;      
      
   procedure Area_size (on: SDL_plane; w,h: out Natural) is
   begin      
      W := Natural(Window.Screen.W) ;
      H := Natural(Window.Screen.H) ;      
   end;
   
   procedure Text_XY(on: in out SDL_plane; x,y: Integer; text: UTF_16_String) is
      Ts : access SDL_Surface ;
      Font : System.Address ;
      Color : SDL_Color := (0,0,0,0);
      
      Rect : aliased SDL_Rect ;
      Ret : Int ;
   begin
      --Put_Line("********************************************************");
      --Put_Line("__Text_Size__") ;       
      Font := Current_Font ; -- Ttf_OpenFont(New_String("arial.ttf"),12);
      Ts := TTF_RenderText_Solid (Font,New_String(To_String(Text)),Color) ;       
      --Ada.Text_IO.Put_Line("X " & Natural'Image(x));
      --Ada.Text_IO.Put_Line("Y " & Natural'Image(y));
      -- Put_Line(Text);
      Rect.X := Sint16(X) ;
      Rect.Y := Sint16(Y) ;
      Rect.W := Uint16(Ts.W) ;
      Rect.H := Uint16(Ts.H) ;      
      Ret := SDL_UpperBlit(Ts,null,Window.Screen,Rect'Access);
   end;
   
   procedure Text_size (on   : in out SDL_plane;
			text : in     UTF_16_String;
			x,y  :    out Natural
		       ) is
      Ts : access SDL_Surface ;
      Font : System.Address ;
      Color : SDL_Color := (0,0,0,0);
      F : SDL_PixelFormat ;      
   begin
      --Put_Line("********************************************************");
      --Put_Line("__Text_Size__") ; 
      
      
      
      Font := Current_Font ; --Ttf_OpenFont(New_String("arial.ttf"),12);
      Ts := TTF_RenderText_Solid (Font,New_String(To_String(Text)),Color) ;       
      X := Natural(Ts.W) ;
      Y := Natural(Ts.H) ;      
      --Ada.Text_IO.Put_Line("X " & Natural'Image(x));
      --Ada.Text_IO.Put_Line("Y " & Natural'Image(y));
      Put_Line(Text);
   end;
   
   procedure Create_target_font(
				on         : in out SDL_plane;
				descriptor : in     Font_descriptor;
				new_index  : in     Positive
			       ) is
      Font : System.Address ;
      
      Style : Uint16 ;
   begin      
      Ada.Text_IO.Put_Line("Creating font " & Positive'Image(New_Index) & " with size " & Integer'Image(Descriptor.Size));
      Font := Ttf_OpenFont(New_String("arial.ttf"),Int(Descriptor.Size) / 2 );      
      Style := TTF_STYLE_NORMAL;
      if Descriptor.Modifier(Bold) = True then
	 Style := Style or TTF_STYLE_BOLD ;
      end if ;
      if Descriptor.Modifier(Italic) = True then
	 Style := Style or TTF_STYLE_ITALIC ;
      end if ;
      if Descriptor.Modifier(Underlined) = True then
	 Style := Style or TTF_STYLE_UNDERLINE ;
      end if ;
      if Descriptor.Modifier(STRIKETHROUGH) = True then
	 Style := Style or TTF_STYLE_STRIKETHROUGH ;
      end if ;
      
      TTF_SetFontStyle(Font, Int(Style));
      
      SDL_Fonts_Vector.Append(Fonts,Font);
   end ;
   
   procedure Select_target_font(
				on         : in out SDL_plane;
				index      : in     Positive
			       ) is 
   begin
      Current_Font := SDL_Fonts_Vector.Element(Fonts,index);
   end ;
   
   procedure Destroy_target_fonts(on: in out SDL_plane) is 
   begin
      Put_Line("Destroyinging font ");
   end ;
   
   
   procedure Select_target_text_color(
				      on: in out SDL_plane;
				      code: in Color_Code
				     ) is 
   begin
      Put_Line("Selecting colors ...");
      Current_Color := Code ;      
   end ;
   
   Xhtml : DOM.Core.Node_List;
   o: HT_object;   
   
   -- function return ;
   ret : Int ;
   
   --
   -- C'est parti pour un peu de SDL
   --
   
   procedure Init is
      Event : aliased SDL_Event ;
   begin
      Ret := SDL_Init(SDL_INIT_EVERYTHING) ;
      Ret := TTF_Init ;
      Window.Screen := SDL_SetVideoMode (1024,768,32,SDL_HWSURFACE or SDL_DOUBLEBUF); 
      Window.Draw(o);
      
      SDL_WM_SetCaption(New_String("Wasabee version 0.0.1 - Gautier de MONTMOLLIN , Frederic BOYER"),New_String(""));
      
      Ret := SDL_Flip(Window.Screen);
      loop
	 Ret := SDL_PollEvent(Event'Access) ;
	 if SDL_EventType(Event.C_Type) = SDL_SDL_Events_H.SDL_QUIT then
	    exit ;
	 end if ;
      end loop ;      
   end ;
   
   procedure Quit is
   begin
      TTF_Quit ;
      SDL_SDL_H.SDL_Quit;
   end ;
   
begin
   
   --
   -- Ouvrir une fenetre
   --
   

   if Argument_Count = 0 then
      Put_Line(Standard_Error, "Provide an URL as command-line argument");
   else
    Wasabee.Request.Open_Url (Argument(1), Xhtml);
    Load_frame(o, Xhtml);
    Init ;   
    Quit ;
  end if;
   
end ;


