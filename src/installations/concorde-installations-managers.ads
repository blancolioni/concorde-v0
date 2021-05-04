with Concorde.Handles.Managed;
with Concorde.Managers;

package Concorde.Installations.Managers is

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Outpost_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Service_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

end Concorde.Installations.Managers;
