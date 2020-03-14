with Nazar.Models.Table;

with Concorde.Handles.Colony;

package Concorde.UI.Models.Colonies is

   function Colony_Pop_Group_Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Colonies;
