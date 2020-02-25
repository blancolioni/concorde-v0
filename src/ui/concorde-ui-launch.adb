with Concorde.UI.Gtk_UI;
with Concorde.UI.Web_UI;

package body Concorde.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI (Name : String) return UI_Type is
   begin
      if Name = "" then
         return Concorde.UI.Gtk_UI.Get_Gtk_UI;
      elsif Name = "aws" then
         return Concorde.UI.Web_UI.Get_Web_UI;
      elsif Name = "gtk" then
         return Concorde.UI.Gtk_UI.Get_Gtk_UI;
      else
         raise Constraint_Error with
           "undedefined user interface: " & Name;
      end if;
   end Get_UI;

end Concorde.UI.Launch;
