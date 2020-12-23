with Tropos;

with Concorde.Handles.Faction;
with Concorde.Handles.World;
with Concorde.Handles.World_Sector;

package Concorde.Colonies.Create is

   procedure New_Colony
     (World   : Concorde.Handles.World.World_Class;
      Sector  : Concorde.Handles.World_Sector.World_Sector_Class;
      Faction : Concorde.Handles.Faction.Faction_Class;
      Config  : Tropos.Configuration);

end Concorde.Colonies.Create;
