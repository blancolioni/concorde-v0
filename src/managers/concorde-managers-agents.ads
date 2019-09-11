with Concorde.Commodities;
with Concorde.Markets;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db.Agent;

package Concorde.Managers.Agents is

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager_Type);

   procedure Create_Planning
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Create_Bids
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Execute_Production
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   procedure Execute_Consumption
     (Manager : in out Root_Agent_Manager_Type)
   is abstract;

   function Current_Cash
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type;

   function Current_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Current_Bid_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Historical_Mean_Price
     (Manager : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Current_Buy_Cost
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   function Current_Sell_Earn
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   function Available
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Boolean;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Bid_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Ask_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   procedure Create_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Create_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   function Previous_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Previous_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Previous_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Remaining_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Remaining_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Has_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Boolean;

   function Stock_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Stock_Value
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Money_Type;

   function Stock_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type;

   procedure Add_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Scan_Stock
     (Manager : Root_Agent_Manager_Type'Class;
      Process : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type));

   procedure Initialize_Agent_Manager
     (Manager           : in out Root_Agent_Manager_Type'Class;
      Agent             : Concorde.Db.Agent.Agent_Type;
      Market            : Concorde.Markets.Concorde_Market;
      Planning_Cycle    : Positive);

   procedure Log
     (Manager : Root_Agent_Manager_Type'Class;
      Message : String);

private

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with
      record
         Agent             : Concorde.Db.Agent_Reference;
         Has_Stock         : Concorde.Db.Has_Stock_Reference;
         Account           : Concorde.Db.Account_Reference;
         Market            : Concorde.Markets.Concorde_Market;
         Planning_Cycle    : Positive;
         Update_Count      : Natural := 0;
      end record;

end Concorde.Managers.Agents;
