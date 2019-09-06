private with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Updates;

with Concorde.Db.Managed;

package Concorde.Managers is

   type Root_Manager_Type is abstract tagged private;

   function Identifier
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   procedure Activate
     (Manager : not null access Root_Manager_Type)
   is abstract;

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Concorde.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Constructor_Function is access
     function (Managed : Concorde.Db.Managed_Reference)
               return Manager_Type;

private

   type Root_Manager_Type is abstract tagged
      record
         Managed         : Concorde.Db.Managed_Reference :=
                             Concorde.Db.Null_Managed_Reference;
         Is_Active       : Boolean := False;
         Has_Next_Update : Boolean := False;
         Next_Update     : Concorde.Calendar.Time;
      end record;

   package Register_Maps is
     new WL.String_Maps (Constructor_Function);

   Register : Register_Maps.Map;

   package Manager_Maps is
     new WL.String_Maps (Manager_Type);

   Active_Map : Manager_Maps.Map;

   type Manager_Update is
     new Concorde.Updates.Update_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Activate
     (Update : Manager_Update);

   function Managed_Key
     (Managed : Concorde.Db.Managed.Managed_Type)
      return String
   is (Managed.Manager & Managed.Identity);

end Concorde.Managers;
