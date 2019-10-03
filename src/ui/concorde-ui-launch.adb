with Concorde.UI.Web_UI;

package body Concorde.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI (Name : String) return UI_Interface'Class is
      pragma Unreferenced (Name);
   begin
      return Concorde.UI.Web_UI.Get_Web_UI;
   end Get_UI;

end Concorde.UI.Launch;
