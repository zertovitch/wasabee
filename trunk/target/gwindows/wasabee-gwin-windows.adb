with Wasabee.Encoding;                  use Wasabee.Encoding;
with Wasabee.Hypertext;                 use Wasabee.Hypertext;
with Wasabee.Hypertext.Parsing;         use Wasabee.Hypertext.Parsing;
-- with Wasabee.Hypertext.Display;         use Wasabee.Hypertext.Display;
with Wasabee.Hypertext.Locations;       use Wasabee.Hypertext.Locations;
with Wasabee.URL;                       use Wasabee.URL;
with Wasabee.Util;                      use Wasabee.Util;
with Wasabee.Request;

with Wasabee.GWin.Main;                 use Wasabee.GWin.Main;
with Wasabee.GWin.Tabs;                 use Wasabee.GWin.Tabs;
with Wasabee_Resource_GUI;              use Wasabee_Resource_GUI;

with GWindows.Base;                     use GWindows.Base;
-- with GWindows.Constants;                use GWindows.Constants;
-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Wide_Text_IO;

package body Wasabee.GWin.Windows is

  --------------------------
  -- URL_box_type methods --
  --------------------------

  procedure On_Message
    (Window       : in out URL_box_type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult)
  is
    use Interfaces.C, GWindows.Types;
    WM_CHAR: constant := 258;
    the_browser: Browser_window_type renames Browser_window_type(Window.Parent.Parent.Parent.all);
  begin
    if message = WM_CHAR then
      case wParam is
        when 13 => -- Return was pressed -> Go!
          the_browser.Focus;
          the_browser.Go_on_URL;
          return;
        when 27 => -- Esc -> like Ctrl-L again.
          the_browser.On_Menu_Select(ID_New_Address);
          return;
        when others =>
          null;
      end case;
    end if;
    Edit_Box_Type(Window).On_Message(message, wparam, lParam, return_Value);
  end On_Message;

  beginner_URL_blurb: constant GString:=
    "Click here or press Ctrl-L for entering new Web address";

  procedure On_Focus (Window : in out URL_box_type) is
  begin
    if Window.Text = beginner_URL_blurb then
      Window.Text("");
    end if;
  end On_Focus;

  ------------------------------
  -- Control_box_type methods --
  ------------------------------

  procedure On_Create (Window : in out Control_box_type) is
  begin
    Window.navigation_and_url.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 20,
      Width      => 1, -- will be extended by docking
      Height     => 1  -- will be extended by docking
    );
    Window.navigation_buttons.Create_As_Control(
      Parent     => Window.navigation_and_url,
      Left       => 0,
      Top        => 0,
      Width      => 50,
      Height     => 1  -- will be extended by docking
    );
    Window.navigation_buttons.Dock(At_Left);
    Window.url_box.Create(
      Parent     => Window.navigation_and_url,
      Left       => 50,
      Top        => 0,
      Width      => 1, -- will be extended by docking
      Height     => 1, -- will be extended by docking
      Text       => ""
    );
    Window.url_box.Set_Font(URL_box_font);
    Window.url_box.Dock(Fill);
    Window.tab_visuals.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 0,
      Width      => 1,  -- will be extended by docking
      Height     => 20
    );
    Window.tab_visuals.Dock(At_Top);
    Window.navigation_and_url.Dock(Fill);
  end On_Create;

  ---------------------------------
  -- Browser_window_type methods --
  ---------------------------------

  procedure On_Create (Window : in out Browser_window_type) is
  begin
    Use_Gui_Font(Window);
    Window.control_box.Create_As_Control(
      Parent     => Window,
      Left       => 0,
      Top        => 0,
      Width      => 1, -- will be extended by docking
      Height     => 40
    );
    Window.control_box.Dock(At_Top);
    --  Wasabee_Resource_GUI.Create_Full_Menu(Window.hidden_menu);
    --  Window.Menu(Window.hidden_menu.Main);
    --
    -- First tab
    --
    Window.New_Tab;
  end On_Create;

  procedure On_Focus (Window : in out Browser_window_type) is
  begin
    Window.tabs.Element(Window.active_tab).Focus;
  end On_Focus;

  procedure Refresh_title_and_URL(Window : in out Browser_window_type) is
    active_tab: HT_area_type renames Window.tabs.Element(Window.active_tab).all;
    title: constant GString:= active_tab.HT_contents.Title;
  begin
    if title = "" then
      Window.Text(
        GU2G(Window.window_info_string) & " - active tab:" &
             Integer'Wide_Image(Window.active_tab)
        );
    else
      Window.Text(title & " - " & S2G(Version));
    end if;
    Window.control_box.url_box.Text(S2G(S(active_tab.URL_with_anchor)));
  end Refresh_title_and_URL;

  procedure Set_active_tab(
    Window : in out Browser_window_type;
    idx    : in     Positive)
  is
    t_curs: Tabs_Vectors.Cursor:= Window.tabs.First;
    use Tabs_vectors;
  begin
    Window.active_tab:= idx;
    while t_curs /= Tabs_vectors.No_Element loop
      if To_Index(t_curs)=idx then
        Element(t_curs).Show;
      else
        Element(t_curs).Hide;
      end if;
      t_curs:= Next(t_curs);
    end loop;
    Refresh_title_and_URL(Window);
  end Set_active_tab;

  procedure New_tab(Window : in out Browser_window_type) is
    newcomer: constant Tab_access:= new HT_area_type;
    main_window: Main_Wasa_Window_Type renames Main_Wasa_Window_Type(Window.main.all);
  begin
    newcomer.Create_As_Control(
      Parent => Window,
      Left   => 0,
      Top    => 100, -- will be adjusted by docking
      Width  => 1,   -- will be extended by docking
      Height => 1    -- will be extended by docking
    );
    newcomer.Dock(Fill);
    Window.Dock_Children;
    newcomer.Finish_creation;
    newcomer.Draw_with_resize;
    Window.tabs.Append(newcomer);
    Set_active_tab(Window, Window.tabs.Find_Index(newcomer));
    Window.control_box.url_box.Text(beginner_URL_blurb);
    Window.Dock_Children;
    main_window.Update_control_frame;
  end New_tab;

  procedure Next_tab(Window : in out Browser_window_type) is
    t_curs: Tabs_Vectors.Cursor:= Window.tabs.To_Cursor(Window.active_tab);
    use Tabs_vectors;
  begin
    t_curs:= Next(t_curs);
    if t_curs = No_Element then
      t_curs:= Window.tabs.First;
    end if;
    Set_active_tab(Window, To_Index(t_curs));
  end Next_tab;

  procedure Close_tab(Window : in out Browser_window_type) is
    tabs_cursor_to_delete: Tabs_Vectors.Cursor;
    p_active_tab: Tab_access;
    use Tabs_vectors, Ada.Containers;
    main_window: Main_Wasa_Window_Type renames Main_Wasa_Window_Type(Window.main.all);
  begin
    if Window.tabs.Length = 0 then
      return;
    end if;
    Window.tabs.Element(Window.active_tab).Close;
    tabs_cursor_to_delete:= Window.tabs.To_Cursor(Window.active_tab);
    if Window.tabs.Length = 1 then
      Window.Close; -- Last tab closing -> we close the whole window
    else
      Window.Next_tab;
      p_active_tab:= Window.tabs.Element(Window.active_tab);
      Window.tabs.Delete(tabs_cursor_to_delete);
      -- The new active index may have changed through deletion
      Window.active_tab:= Window.tabs.Find_Index(p_active_tab);
    end if;
    main_window.Update_control_frame;
  end Close_tab;

  procedure Go_on_URL(Window : in out Browser_window_type) is
    active_tab: HT_area_type renames Window.tabs.Element(Window.active_tab).all;
    HTML : Unbounded_String;
    URL_from_box: constant String:= G2S(Window.control_box.url_box.Text);
    URL_without_anchor: constant String:= Remove_anchor(URL_from_box);
  begin
    if active_tab.HT_contents.Get_own_URL = URL_without_anchor then
      null; -- Do nothing! Web page is already there.
    elsif Index(URL_from_box, "://") = 0 then  --  Web address typed without "http://"
      Window.control_box.url_box.Text("http://" & Window.control_box.url_box.Text);
      Window.Go_on_URL;
      return;
    else
      active_tab.HT_contents.Set_own_URL(URL_without_anchor);
      active_tab.URL_with_anchor:= U(URL_from_box);
      --  We are here on Windows. ISO-8859-1 is close to Windows 1252.
      --  A web page with non-ASCII characters should specify its encoding.
      active_tab.HT_contents.Set_Encoding(iso_8859_1);
      put_line("--> Getting HTML from Web");
      Wasabee.Request.Retrieve_from_URL (active_tab.HT_contents.Get_own_URL, HTML) ;
      Dump_string("HTML Source.txt", To_String(HTML));
      put_line("--> Getting hypertext from HTML");
      Load_frame(active_tab.HT_contents, To_String(HTML));
      put_line("--> Done.");
      active_tab.HT_contents.Post_loading_processing;
      active_tab.HT_contents.Dump("HTML Tree.txt");
      active_tab.Set_minimal_sliding_panel_size;
      active_tab.Draw_with_resize;
      Refresh_title_and_URL(Window);
    end if;
    active_tab.Scroll_to_point(
      (0, 200) + Anchor_position(active_tab.HT_contents, Anchor_only(URL_from_box))
    );
    active_tab.Focus;
    active_tab.navi.Register(URL_from_box);
  end Go_on_URL;

  procedure Activate_current_navigation_item(Window : in out Browser_window_type) is
    active_tab: HT_area_type renames Window.tabs.Element(Window.active_tab).all;
  begin
    if active_tab.navi.Current_URL = "" then
      return; -- Perhaps the log is empty, or we have an empty URL
    end if;
    Window.control_box.url_box.Text(S2G(active_tab.navi.Current_URL));
    Window.Go_on_URL;
  end Activate_current_navigation_item;

  procedure Back(Window : in out Browser_window_type) is
  begin
    Window.tabs.Element(Window.active_tab).navi.Back; -- go back in the navigation log
    Activate_current_navigation_item(Window);
  end Back;

  procedure Forward(Window : in out Browser_window_type) is
  begin
    Window.tabs.Element(Window.active_tab).navi.Forward; -- go forward in the navigation log
    Activate_current_navigation_item(Window);
  end Forward;

  procedure On_Menu_Select (
        Window : in out Browser_window_type;
        Item   : in     Integer)
  is
    main_window: Main_Wasa_Window_Type renames Main_Wasa_Window_Type(Window.main.all);
  begin
    case Item is
      when ID_New_Browser_Window =>
        main_window.New_Browser_Window;
      when ID_New_Tab =>
        Window.New_Tab;
      when ID_Next_Tab =>
        Window.Next_Tab;
      when ID_Close_Tab =>
        Window.Close_tab;
      when ID_Back =>
        Window.Back;
      when ID_Forward =>
        Window.Forward;
      when ID_New_Address =>
        Window.control_box.url_box.Set_Selection(
          0, Window.control_box.url_box.Text'Length
        );
        Window.control_box.url_box.Focus;
      when others =>
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  procedure On_Close (Window    : in out Browser_window_type;
                      Can_Close :    out Boolean)
  is
    w_curs: Windows_Vectors.Cursor;
    main_window: Main_Wasa_Window_Type renames Main_Wasa_Window_Type(Window.main.all);
  begin
    Can_Close:= True or Window.main/=null; -- !! check "Leave page" "Close tabs" etc.
    if not Can_Close then
      return;
    end if;
    w_curs:= main_window.windows.Find(Window'Unchecked_Access);
    main_window.windows.Delete(w_curs);
    main_window.Update_control_frame;
    if main_window.windows.Is_Empty then
      Close(main_window); -- Close everything if no more browser window is open
    end if;
  end On_Close;

end Wasabee.GWin.Windows;
