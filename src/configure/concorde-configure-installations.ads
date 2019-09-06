with Tropos;

with Concorde.Db;

package Concorde.Configure.Installations is

   procedure Configure_Installation
     (Owner  : Concorde.Db.Faction_Reference;
      Sector : Concorde.Db.World_Sector_Reference;
      Config : Tropos.Configuration);

end Concorde.Configure.Installations;
