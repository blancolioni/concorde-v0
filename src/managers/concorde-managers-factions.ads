package Concorde.Managers.Factions is

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type;

   function Create_Faction_Company_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type;

end Concorde.Managers.Factions;
