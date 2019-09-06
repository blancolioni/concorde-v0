package body Concorde.UI.Models.Toolbar is

   function Create_Toolbar_Model
      return Toolbar_Model
   is
   begin
      return new Root_Toolbar_Model;
   end Create_Toolbar_Model;

end Concorde.UI.Models.Toolbar;
