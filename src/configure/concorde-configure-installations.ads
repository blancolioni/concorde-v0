with Tropos;

with Concorde.Db;

package Concorde.Configure.Installations is

   procedure Configure_Installation
     (Owner  : Concorde.Handles.Faction.Faction_Handle;
      Sector : Concorde.Handles.World_Sector_Reference;
      Config : Tropos.Configuration);

end Concorde.Configure.Installations;
