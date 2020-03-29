with Nazar.Models.Draw;

with Concorde.Handles.Faction;
with Concorde.Handles.World;

package Concorde.UI.Models.World is

   function World_Model
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      World   : Concorde.Handles.World.World_Class)
      return Nazar.Models.Draw.Nazar_Draw_Model;

end Concorde.UI.Models.World;
