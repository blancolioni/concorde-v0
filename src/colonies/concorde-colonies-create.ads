with Tropos;

with Concorde.Db;

package Concorde.Colonies.Create is

   procedure New_Colony
     (World   : Concorde.Db.World_Reference;
      Sector  : Concorde.Db.World_Sector_Reference;
      Faction : Concorde.Db.Faction_Reference;
      Config  : Tropos.Configuration);

end Concorde.Colonies.Create;
