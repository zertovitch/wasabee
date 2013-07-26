with Wasabee_common;                    use Wasabee_common;

with GWindows.Cursors;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;
with GWindows.Types;

package Wasabee_GWin.Display is

  procedure Draw(
    Canvas : in out GWindows.Drawing.Canvas_Type'Class;
    ho     : in out HTML_Object; -- in out: ho.refresh is changed
    width  : Integer;
    height : Integer);

end Wasabee_GWin.Display;
