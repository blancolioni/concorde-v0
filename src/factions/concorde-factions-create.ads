with Tropos;

with Concorde.Color;

with Concorde.Handles.Faction;
with Concorde.Handles.User;

package Concorde.Factions.Create is

   function Create_Faction
     (User        : Concorde.Handles.User.User_Class;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Concorde.Handles.Faction.Faction_Class;

end Concorde.Factions.Create;
