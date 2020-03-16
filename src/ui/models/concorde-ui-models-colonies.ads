with Nazar.Models.Table;

with Concorde.Handles.Colony;
with Concorde.Handles.Faction;

package Concorde.UI.Models.Colonies is

   function Faction_Colony_Table
     (Faction : Concorde.Handles.Faction.Faction_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

   function Colony_Pop_Group_Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

   function Colony_Policy_Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Colonies;
