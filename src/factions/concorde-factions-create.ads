with Tropos;

with Concorde.Color;

with Concorde.Db;

package Concorde.Factions.Create is

   function Create_Faction
     (User        : Concorde.Db.User_Reference;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Concorde.Db.Faction_Reference;

end Concorde.Factions.Create;
