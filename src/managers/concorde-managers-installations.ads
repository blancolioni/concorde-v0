package Concorde.Managers.Installations is

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type;

   function Create_Default_Agora_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type;

end Concorde.Managers.Installations;
