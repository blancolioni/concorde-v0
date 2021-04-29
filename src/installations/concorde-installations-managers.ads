with Concorde.Handles.Managed;
with Concorde.Managers;

package Concorde.Installations.Managers is

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type;

   function Create_Outpost_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type;

end Concorde.Installations.Managers;
