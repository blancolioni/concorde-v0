private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Markets;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Handles.Account;
with Concorde.Handles.Agent;
with Concorde.Handles.Commodity;
with Concorde.Handles.Has_Stock;

package Concorde.Managers.Agents is

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with private;

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager_Type);

   procedure Create_Planning
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure Set_Requirements
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure Create_Asks
     (Manager : in out Root_Agent_Manager_Type);

   procedure Create_Bids
     (Manager : in out Root_Agent_Manager_Type);

   procedure Set_Sale_Stock
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure Pay_Daily_Costs
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure Execute_Production
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure Execute_Consumption
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   procedure On_Activation_Begin
     (Manager : in out Root_Agent_Manager_Type);

   procedure On_Activation_End
     (Manager : in out Root_Agent_Manager_Type);

   procedure Add_Requirement
     (Manager   : in out Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Necessary : Concorde.Quantities.Quantity_Type;
      Desired   : Concorde.Quantities.Quantity_Type);

   procedure Add_Ask
     (Manager   : in out Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   function Current_Cash
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type;

   procedure Earn
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Concorde.Money.Money_Type;
      Tag     : String);

   procedure Spend
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Concorde.Money.Money_Type;
      Tag     : String);

   function Last_Earn
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type;

   function Last_Spend
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type;

   procedure Reset_Cashflow
     (Manager : Root_Agent_Manager_Type'Class);

   function Current_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Current_Bid_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Historical_Mean_Price
     (Manager : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type;

   function Current_Buy_Cost
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   function Current_Sell_Earn
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   function Available
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Boolean;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Bid_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Ask_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Max_Price : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Min_Price : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   procedure Create_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Create_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   --  function Previous_Ask
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type;
   --
   --  function Previous_Ask_Price
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Money.Price_Type;
   --
   --  function Previous_Bid
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type;
   --
   --  function Remaining_Ask
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type;
   --
   --  function Remaining_Bid
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type;

   function Has_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Boolean;

   function Stock_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Stock_Value
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Money_Type;

   function Stock_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type;

   procedure Add_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Scan_Stock
     (Manager : Root_Agent_Manager_Type'Class;
      Process : not null access
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type));

   procedure Initialize_Agent_Manager
     (Manager           : in out Root_Agent_Manager_Type'Class;
      Agent             : Concorde.Handles.Agent.Agent_Class;
      Market            : Concorde.Markets.Concorde_Market;
      Planning_Cycle    : Positive);

   procedure Log
     (Manager : Root_Agent_Manager_Type'Class;
      Message : String);

private

   type Stock_Item_Record is
      record
         Commodity : Concorde.Handles.Commodity.Commodity_Handle;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type;
      end record;

   package Stock_Item_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Item_Record);

   type Root_Agent_Manager_Type is
     abstract new Root_Manager_Type with
      record
         Agent             : Concorde.Handles.Agent.Agent_Handle;
         Has_Stock         : Concorde.Handles.Has_Stock.Has_Stock_Handle;
         Account           : Concorde.Handles.Account.Account_Handle;
         Market            : Concorde.Markets.Concorde_Market;
         Planning_Cycle    : Positive;
         Update_Count      : Natural := 0;
         Last_Earn         : Concorde.Money.Money_Type :=
                               Concorde.Money.Zero;
         Last_Spend        : Concorde.Money.Money_Type :=
                               Concorde.Money.Zero;
         Necessary         : Stock_Item_Lists.List;
         Desired           : Stock_Item_Lists.List;
         Sell              : Stock_Item_Lists.List;
      end record;

end Concorde.Managers.Agents;
