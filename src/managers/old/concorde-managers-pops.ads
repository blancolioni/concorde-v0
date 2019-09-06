with Concorde.Commodities;
private with Concorde.Quantities;

private with Concorde.Managers.Agents;

package Concorde.Managers.Pops is

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Pop_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager with
      record
         Pop                 : Concorde.Db.Pop_Reference;
         Commodity           : Concorde.Db.Commodity_Reference;
         Employer            : Concorde.Db.Employer_Reference;
         Employed            : Boolean;
         Consumption_Quality : Positive;
         Service_Quality     : Positive;
      end record;

   overriding function Identifier
     (Manager : Root_Pop_Manager)
      return String
   is ("pop" & Concorde.Db.To_String (Manager.Pop) & " manager");

   overriding function Managed_Object_Id
     (Manager : Root_Pop_Manager)
      return String
   is ("pop" & Concorde.Db.To_String (Manager.Pop));

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Pop_Manager);

   overriding procedure Get_Required_Stock
     (Manager : Root_Pop_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Pop_Manager);

   function Size
     (Manager : Root_Pop_Manager'Class)
      return Concorde.Quantities.Quantity_Type;

end Concorde.Managers.Pops;
