with Ada.Exceptions;
with Ada.Text_IO;

with Concorde.Logging;
with Concorde.Updates.Events;

package body Concorde.Managers is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
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
         Update.Manager.Managed.Element.Update_Managed
           .Set_Next_Event (Update.Manager.Next_Update)
           .Set_Active (True)
           .Done;
      else
         Concorde.Logging.Log
           (Actor    => "update",
            Location => "",
            Category => "",
            Message  => "deactivating");
         Update.Manager.Managed.Element.Update_Managed
           .Set_Active (False)
           .Done;
      end if;

   exception
      when E : others =>
         declare
            Message : constant String := Ada.Exceptions.Exception_Message (E);
         begin
            Concorde.Logging.Log
              (Category => Update.Manager.Identifier,
               Message  => Message);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Message);
         end;

   end Execute;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Concorde_Duration)
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
                       (Concorde.Updates.Root_Update_Type with
                        Manager => Manager_Type (Manager));
         begin
            Manager.Is_Active := True;
            Concorde.Updates.Events.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;

   end Set_Next_Update_Time;

end Concorde.Managers;
