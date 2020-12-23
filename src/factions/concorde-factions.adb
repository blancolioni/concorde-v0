package body Concorde.Factions is

   ----------------------
   -- Get_User_Faction --
   ----------------------

   function Get_User_Faction
     (User : Concorde.Handles.User.User_Class)
      return Faction_Class
   is
   begin
      return Concorde.Handles.Faction.First_By_User (User);
   end Get_User_Faction;

end Concorde.Factions;
