with Tropos;

with Concorde.Handles.Faction;
with Concorde.Handles.World;
with Concorde.Handles.World_Sector;

package Concorde.Colonies.Create is

   procedure Initial_Colony
     (Faction : Concorde.Handles.Faction.Faction_Class;
      World   : Concorde.Handles.World.World_Class;
      Capital : Concorde.Handles.World_Sector.World_Sector_Class;
      Init    : Tropos.Configuration);

end Concorde.Colonies.Create;
