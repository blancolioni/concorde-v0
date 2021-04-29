private with Agora.Agents;
private with Agora.Commodities;
private with Agora.Markets;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Handles.Agent;
with Concorde.Handles.Commodity;
with Concorde.Handles.Market;
with Concorde.Handles.World;

with Concorde.Db;

package Concorde.Markets is

   type Concorde_Market is tagged private;

   function Current_Price
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Current_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Cash      : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Value
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   function Historical_Mean_Price
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   procedure Create_Offer
     (Market    : Concorde_Market'Class;
      Agent     : Concorde.Handles.Agent.Agent_Class;
      Offer     : Concorde.Db.Offer_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure Load
     (Market : in out Concorde_Market'Class;
      Db     : Concorde.Handles.Market.Market_Class);

   procedure Save
     (Market : Concorde_Market'Class);

   procedure Load_Markets;

   function World_Market
     (World : Concorde.Handles.World.World_Class)
      return Concorde_Market;

private

   type Agora_Commodity is
     new Agora.Commodities.Commodity_Interface with
      record
         Handle : Concorde.Handles.Commodity.Commodity_Handle;
      end record;

   overriding function Tag (Item : Agora_Commodity) return String
   is (Item.Handle.Tag);

   overriding function Base_Price
     (Item : Agora_Commodity)
      return Agora.Price_Type
   is (Agora.Price_Type (Concorde.Money.To_Real (Item.Handle.Base_Price)));

   type Agora_Agent is
     new Agora.Agents.Agent_Interface with
      record
         Handle : Concorde.Handles.Agent.Agent_Handle;
      end record;

   overriding function Tag (Item : Agora_Agent) return String
   is (Item.Handle.Identifier);

   overriding procedure On_Buy
     (Agent     : Agora_Agent;
      Seller    : Agora.Agents.Agent_Interface'Class;
      Commodity : Agora.Commodities.Commodity_Interface'Class;
      Quantity  : Agora.Quantity_Type;
      Price     : Agora.Price_Type);

   overriding procedure On_Sell
     (Agent     : Agora_Agent;
      Buyer     : Agora.Agents.Agent_Interface'Class;
      Commodity : Agora.Commodities.Commodity_Interface'Class;
      Quantity  : Agora.Quantity_Type;
      Price     : Agora.Price_Type);

   type Agora_Market_Access is
     access all Agora.Markets.Agora_Market_Type'Class;

   type Concorde_Market is tagged
      record
         Agora_Market : Agora_Market_Access;
         Db_Market    : Concorde.Handles.Market.Market_Handle;
      end record;

end Concorde.Markets;
