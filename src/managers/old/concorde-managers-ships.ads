private with Concorde.Managers.Agents;
private with Concorde.Commodities;
private with Concorde.Quantities;

package Concorde.Managers.Ships is

   function Create_Trade_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Ship_Manager is
     abstract new Concorde.Managers.Agents.Root_Agent_Manager with
      record
         Ship           : Concorde.Db.Ship_Reference;
         Current_World  : Concorde.Db.World_Reference;
         Current_System : Concorde.Db.Star_System_Reference;
         Cargo_Space    : Concorde.Quantities.Quantity_Type;
      end record;

   overriding function Identifier
     (Manager : Root_Ship_Manager)
      return String
   is ("ship" & Concorde.Db.To_String (Manager.Ship) & " manager");

   overriding function Managed_Object_Id
     (Manager : Root_Ship_Manager)
      return String
   is ("ship" & Concorde.Db.To_String (Manager.Ship));

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Ship_Manager);

   overriding procedure Get_Required_Stock
     (Manager : Root_Ship_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Ship_Manager);

end Concorde.Managers.Ships;
