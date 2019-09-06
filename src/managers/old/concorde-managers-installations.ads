with Concorde.Commodities;

private with Concorde.Managers.Agents;
private with Concorde.Db.Facility;
private with Concorde.Money;

package Concorde.Managers.Installations is

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

   function Create_Hub_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Installation_Manager is
     abstract new Concorde.Managers.Agents.Root_Agent_Manager with
      record
         Employer     : Concorde.Db.Employer_Reference;
         Installation : Concorde.Db.Installation_Reference;
         Facility     : Concorde.Db.Facility_Reference;
         Payroll      : Concorde.Money.Money_Type;
      end record;

   overriding function Identifier
     (Manager : Root_Installation_Manager)
      return String
   is ("installation"
       & Concorde.Db.To_String (Manager.Installation) & " manager");

   overriding function Managed_Object_Id
     (Manager : Root_Installation_Manager)
      return String
   is (Concorde.Db.Facility.Get (Manager.Facility).Tag
       & Concorde.Db.To_String (Manager.Installation));

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Installation_Manager);

end Concorde.Managers.Installations;
