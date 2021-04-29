with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with Concorde.Random;
with Concorde.Updates.Events;

with Concorde.Handles.Managed;

with Concorde.Db;

package body Concorde.Managers.Execution is

   type Check_Manager_Update is
     new Concorde.Updates.Root_Update_Type with null record;

   overriding function Name
     (Update : Check_Manager_Update)
      return String
   is ("check-manager");

   overriding procedure Execute
     (Update : Check_Manager_Update);

   package Managed_Reference_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Concorde.Handles.Managed.Managed_Class,
        Concorde.Handles.Managed."=");

   function Get_Manager_Name
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return String;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Check_Manager_Update)
   is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of
        Concorde.Handles.Managed.Select_By_Active_Scheduled (True, False)
      loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;

      Concorde.Updates.Events.Update_With_Delay
        (Wait   => Concorde.Calendar.Days (1.0),
         Update => Update);
   end Execute;

   ----------------------
   -- Get_Manager_Name --
   ----------------------

   function Get_Manager_Name
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return String
   is
   begin
      return Managed.Manager;
   end Get_Manager_Name;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of Concorde.Handles.Managed.Scan_By_Top_Record loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;
      declare
         Update : constant Check_Manager_Update :=
                    (Concorde.Updates.Root_Update_Type with null record);
      begin
         Concorde.Updates.Events.Update_With_Delay
           (Wait   =>
              Concorde.Calendar.Days (Concorde.Random.Unit_Random + 0.5),
            Update => Update);
      end;

   end Load_Managers;

   -------------------
   -- Start_Manager --
   -------------------

   procedure Start_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
   is
      Key  : constant String := Managed.Identifier;
      Name : constant String := Get_Manager_Name (Managed);
      Manager : constant Manager_Type :=
                  Register.Element (Name) (Managed);

   begin
      if Manager /= null then
         Manager.Is_Active := Managed.Active;
         Manager.Managed := Managed_Holders.To_Holder (Managed);
         Active_Map.Insert (Key, Manager);
         if Manager.Is_Active then
            declare
               Update : constant Manager_Update :=
                          (Concorde.Updates.Root_Update_Type with
                           Manager => Manager);
            begin
               Concorde.Updates.Events.Update_At
                 (Clock  => Managed.Next_Event,
                  Update => Update);
            end;
         end if;

         Managed.Update_Managed.Set_Scheduled (Manager.Is_Active).Done;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "cannot create manager '"
            & Name
            & "' for "
            & Concorde.Db.Record_Type'Image
              (Managed.Top_Record));
         Concorde.Handles.Managed.Update_Managed (Managed)
           .Set_Active (False)
           .Set_Scheduled (False)
           .Done;
      end if;
   end Start_Manager;

end Concorde.Managers.Execution;
