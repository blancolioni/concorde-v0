with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Concorde.Random;
with Concorde.Updates.Events;

with Concorde.Db.Managed;

package body Concorde.Managers.Execution is

   type Check_Manager_Update is
     new Concorde.Updates.Update_Interface with null record;

   overriding procedure Activate
     (Update : Check_Manager_Update);

   package Managed_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Db.Managed_Reference,
        Concorde.Db."=");

   function Get_Manager_Name
     (Managed : Concorde.Db.Managed_Reference)
      return String;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Check_Manager_Update)
   is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of
        Concorde.Db.Managed.Select_By_Active_Scheduled (True, False)
      loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed.Get_Managed_Reference);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;

      Concorde.Updates.Events.Update_With_Delay
        (Wait   => Concorde.Calendar.Days (1.0),
         Update => Update);
   end Activate;

   ----------------------
   -- Get_Manager_Name --
   ----------------------

   function Get_Manager_Name
     (Managed : Concorde.Db.Managed_Reference)
      return String
   is
      Rec : constant Concorde.Db.Managed.Managed_Type :=
              Concorde.Db.Managed.Get (Managed);
   begin
      return Rec.Manager;
   end Get_Manager_Name;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of Concorde.Db.Managed.Scan_By_Top_Record loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed.Get_Managed_Reference);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;
      declare
         Update : Check_Manager_Update;
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
     (Managed : Concorde.Db.Managed_Reference)
   is
      Key  : constant String := Concorde.Db.To_String (Managed);
      Name : constant String := Get_Manager_Name (Managed);
      Manager : constant Manager_Type :=
                  Register.Element (Name) (Managed);

   begin
      if Manager /= null then
         declare
            Rec : constant Concorde.Db.Managed.Managed_Type :=
                    Concorde.Db.Managed.Get (Managed);
         begin
            Manager.Is_Active := Rec.Active;
            Manager.Managed := Rec.Get_Managed_Reference;
            Active_Map.Insert (Key, Manager);
            if Rec.Active then
               declare
                  Update : constant Manager_Update :=
                             (Manager => Manager);
               begin
                  Concorde.Updates.Events.Update_At
                    (Clock  => Rec.Next_Event,
                     Update => Update);
               end;
            end if;

            Concorde.Db.Managed.Update_Managed (Managed)
              .Set_Scheduled (Rec.Active)
              .Done;
         end;
      else
         declare
            Rec : constant Concorde.Db.Managed.Managed_Type :=
                    Concorde.Db.Managed.Get (Managed);
         begin
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot create manager '"
               & Rec.Manager
               & "' for "
               & Concorde.Db.Record_Type'Image
                 (Rec.Top_Record));
            Concorde.Db.Managed.Update_Managed (Managed)
              .Set_Active (False)
              .Set_Scheduled (False)
              .Done;
         end;
      end if;
   end Start_Manager;

end Concorde.Managers.Execution;
