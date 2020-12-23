with Concorde.Handles.Faction;
with Concorde.Handles.User;

package Concorde.Factions is

   subtype Faction_Handle is Concorde.Handles.Faction.Faction_Handle;
   subtype Faction_Class is Concorde.Handles.Faction.Faction_Class;

   function Get_User_Faction
     (User : Concorde.Handles.User.User_Class)
      return Faction_Class;

end Concorde.Factions;
