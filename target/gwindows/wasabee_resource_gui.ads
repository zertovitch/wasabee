---------------------------------------------------------------------------
-- GUI contents of resource script file: wasabee.rc
-- Transcription time: 2013/07/31   14:33:26
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 25-Nov-2012
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_drawn;      use GWindows.Buttons.Owner_drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

package wasabee_Resource_GUI is

  type Browser_Menu_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
  end record; -- Browser_Menu_Type

  -- Menu at line 26
  procedure Create_Full_Menu
     (Menu        : in out Browser_Menu_Type);

  type Main_control_window_Type is new Window_type with record

    -- Label: IDC_STATIC
    Open_windows_info: Edit_Box_Type;
    -- Label: IDC_STATIC
    Open_tabs_info: Edit_Box_Type;
    -- Label: IDC_STATIC
    Cached_objects_info: Edit_Box_Type;
  end record; -- Main_control_window_Type

  -- Dialog at resource line 45

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Main_control_window_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Wasabee - main control window";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Main_control_window_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin, Frédéric Boyer";
    FileDescription: constant String:= "Wasabee";
    FileVersion: constant String:= "0.001";
    LegalCopyright: constant String:= "© 2013 The Authors - MIT license";
    ProductName: constant String:= "Wasabee";
    Translation: constant:= 1033;
  end Version_info;


  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC           : constant:=     -1;
  Main_control_window  : constant:=    100;
  Browser_Menu         : constant:=    106;
  Cached_objects_info  : constant:=   1001;
  Open_windows_info    : constant:=   1004;
  Open_tabs_info       : constant:=   1005;
  ID_New_Browser_Window: constant:=  40000;
  ID_New_Tab           : constant:=  40001;
  ID_New_Address       : constant:=  40002;
  ID_Next_Tab          : constant:=  40003;
  ID_Close_Tab         : constant:=  40004;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;


  -- Last line of resource script file: 103

end wasabee_Resource_GUI;
