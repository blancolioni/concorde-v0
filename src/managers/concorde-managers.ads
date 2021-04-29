private with Ada.Containers.Indefinite_Holders;
private with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Updates;

with Concorde.Handles.Managed;

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
      Update_Delay : Concorde_Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Constructor_Function is access
     function (Managed : Concorde.Handles.Managed.Managed_Class)
               return Manager_Type;

private

   package Managed_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Handles.Managed.Managed_Class,
        Concorde.Handles.Managed."=");

   type Root_Manager_Type is abstract tagged
      record
         Managed         : Managed_Holders.Holder;
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
     new Concorde.Updates.Root_Update_Type with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Execute
     (Update : Manager_Update);

   overriding function Name
     (Update : Manager_Update)
      return String
   is (Update.Manager.Identifier);

   function Managed_Key
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return String
   is (Managed.Manager & Managed.Identifier);

end Concorde.Managers;
