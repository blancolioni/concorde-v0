with Concorde.Logging;
with Concorde.Updates.Events;

package body Concorde.Managers is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
   begin
      Update.Manager.Has_Next_Update := False;
      Update.Manager.Activate;

      if Update.Manager.Has_Next_Update then
         Update.Manager.Is_Active := True;
         Concorde.Updates.Events.Update_At
           (Clock  => Update.Manager.Next_Update,
            Update => Update);
         Concorde.Db.Managed.Update_Managed (Update.Manager.Managed)
           .Set_Next_Event (Update.Manager.Next_Update)
           .Set_Active (True)
           .Done;
      else
         Concorde.Logging.Log
           (Actor    => "update",
            Location => "",
            Category => "",
            Message  => "deactivating");
         Concorde.Db.Managed.Update_Managed (Update.Manager.Managed)
           .Set_Active (False)
           .Done;
      end if;

   end Activate;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration)
   is
      use type Concorde.Calendar.Time;
   begin
      Manager.Set_Next_Update_Time (Concorde.Calendar.Clock + Update_Delay);
   end Set_Next_Update_Delay;

   --------------------------
   -- Set_Next_Update_Time --
   --------------------------

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Concorde.Calendar.Time)
   is
   begin
      if Manager.Is_Active then
         Manager.Has_Next_Update := True;
         Manager.Next_Update := Update_Time;
      else
         declare
            Update : constant Manager_Update :=
                       (Manager => Manager_Type (Manager));
         begin
            Manager.Is_Active := True;
            Concorde.Updates.Events.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;

   end Set_Next_Update_Time;

end Concorde.Managers;
