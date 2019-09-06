with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db.Agent;

package Concorde.Managers.Agents is

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager);

   function Managed_Object_Id
     (Manager : Root_Agent_Manager)
      return String
      is abstract;

   procedure Create_Market_Offers
     (Manager : in out Root_Agent_Manager);

   procedure Get_Required_Stock
     (Manager  : Root_Agent_Manager;
      Stock    : in out Concorde.Commodities.Stock_Type)
   is abstract;

   procedure Get_Desired_Stock
     (Manager  : Root_Agent_Manager;
      Stock    : in out Concorde.Commodities.Stock_Type);

   procedure Execute_Agent_Tasks
     (Manager : in out Root_Agent_Manager)
   is abstract;

   function Calculate_Capacity
     (Manager : Root_Agent_Manager;
      Stock   : Concorde.Commodities.Stock_Type)
      return Unit_Real;

   function Next_Sleep_Duration
     (Manager : Root_Agent_Manager)
     return Duration;

   function Current_Market_Ask_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Current_Market_Ask_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Current_Market_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Current_Market_Bid_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Minimum_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Current_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type;

   procedure Set_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Price     : Concorde.Money.Price_Type);

   function World
     (Manager : Root_Agent_Manager'Class)
      return Concorde.Db.World_Reference;

   procedure Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Stock   : out Concorde.Commodities.Stock_Type);

   function Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Scan_Current_Stock
     (Manager   : Root_Agent_Manager'Class;
      Process   : not null access
        procedure (Item     : Concorde.Db.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type));

   procedure Scan_Historical_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Days      : Non_Negative_Real;
      Process   : not null access
        procedure (Date : Concorde.Calendar.Time;
                   Quantity : Concorde.Quantities.Quantity_Type));

   procedure Try_Bids
     (Manager   : Root_Agent_Manager'Class;
      Stock     : Concorde.Commodities.Stock_Type;
      Available : out Concorde.Commodities.Stock_Type);

   procedure Place_Bid
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Place_Ask
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   function Capacity
     (Manager : Root_Agent_Manager'Class)
      return Unit_Real;

   function Cash
     (Manager : Root_Agent_Manager'Class)
      return Concorde.Money.Money_Type;

   procedure Pay
     (Manager : Root_Agent_Manager'Class;
      Amount  : Concorde.Money.Money_Type);

   procedure Earn
     (Manager : Root_Agent_Manager'Class;
      Amount  : Concorde.Money.Money_Type);

   procedure Log
     (Manager : Root_Agent_Manager'Class;
      Message : String);

   procedure Log_Market_State
     (Manager : Root_Agent_Manager'Class);

   procedure Initialize_Agent_Manager
     (Manager : in out Root_Agent_Manager'Class;
      Agent   : Concorde.Db.Agent.Agent_Type;
      World   : Concorde.Db.World_Reference);

private

   type Root_Agent_Manager is
     abstract new Root_Manager_Type with
      record
         Agent        : Concorde.Db.Agent_Reference;
         Has_Stock    : Concorde.Db.Has_Stock_Reference;
         Market       : Concorde.Db.Market_Reference;
         Account      : Concorde.Db.Account_Reference;
         World        : Concorde.Db.World_Reference;
         Capacity     : Unit_Real := 1.0;
      end record;

   function Capacity
     (Manager : Root_Agent_Manager'Class)
      return Unit_Real
   is (Manager.Capacity);

end Concorde.Managers.Agents;
