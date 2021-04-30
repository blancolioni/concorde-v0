with Concorde.Managers;
with Concorde.Handles.Managed;

package Concorde.Pops.Managers is

   function Create_Pop_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type;

end Concorde.Pops.Managers;
