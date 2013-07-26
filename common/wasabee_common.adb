package body Wasabee_common is

   ----------------
   -- Load_frame --
   ----------------

   procedure Load_frame (object: in out HTML_object; URL: String) is
   begin
      -- Wasy !!
      object.refresh:= full;
   end Load_frame;

end Wasabee_common;
