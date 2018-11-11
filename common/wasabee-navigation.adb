with Wasabee.Util;                      use Wasabee.Util;

package body Wasabee.Navigation is

  --------------
  -- Register --
  --------------

  procedure Register (nav: in out Navigation_log; new_URL: String) is
    procedure Append is
    begin
      nav.log.Append( U(new_URL) );
      nav.index:= nav.log.Last_Index;
    end Append;
  begin
    if nav.log.Is_Empty then
      Append;
    elsif nav.Current_URL = new_URL then
      null;                       -- case [1]
    elsif nav.index = nav.log.Last_Index then
      Append;                     -- special case [3], with nothing at next position
    elsif nav.log.Element(nav.index + 1) = new_URL then
      nav.index:= nav.index + 1;  -- case [2]: just do forward, keep the forward list
    else
                                  -- general case [3], with something at next position
      for count in reverse nav.index + 1 .. nav.log.Last_Index loop
        nav.log.Delete_Last;
      end loop;
      Append;
    end if;
  end Register;

  ----------
  -- Back --
  ----------

  procedure Back (nav: in out Navigation_log) is
  begin
    if nav.log.Is_Empty then
      return; -- nothing to do...
    end if;
    if nav.index > nav.log.First_Index then
      nav.index:= nav.index - 1;
    end if;
  end Back;

  -------------
  -- Forward --
  -------------

  procedure Forward (nav: in out Navigation_log) is
  begin
    if nav.log.Is_Empty then
      return; -- nothing to do...
    end if;
    if nav.index < nav.log.Last_Index then
      nav.index:= nav.index + 1;
    end if;
  end Forward;

  -----------------
  -- Current_URL --
  -----------------

  function Current_URL (nav: Navigation_log) return String is
  begin
    if nav.log.Is_Empty then
      return "";
    end if;
    return S(nav.log.Element(nav.index));
  end Current_URL;

end Wasabee.Navigation;
