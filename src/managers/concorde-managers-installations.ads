package Concorde.Managers.Installations is

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

   function Create_Default_Agora_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

end Concorde.Managers.Installations;
