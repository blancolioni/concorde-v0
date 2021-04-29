with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Handles.Account;
with Concorde.Handles.Agent;
with Concorde.Handles.Commodity;
with Concorde.Handles.Has_Stock;
with Concorde.Handles.Market;

with Concorde.Db;

package Concorde.Markets is

   subtype Concorde_Market is Concorde.Handles.Market.Market_Class;

   subtype Account_Class is
     Concorde.Handles.Account.Account_Class;

   subtype Agent_Class is
     Concorde.Handles.Agent.Agent_Class;

   subtype Commodity_Class is
     Concorde.Handles.Commodity.Commodity_Class;

   subtype Has_Stock_Class is
     Concorde.Handles.Has_Stock.Has_Stock_Class;

   function Current_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Historical_Mean_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Cash      : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Value
     (Market    : Concorde_Market;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   procedure Create_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent.Agent_Class;
      Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Account   : Concorde.Handles.Account.Account_Class;
      Offer     : Concorde.Db.Offer_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   function Previous_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Agent_Class;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   function Previous_Agent_Offer_Price
     (Market    : Concorde_Market;
      Agent     : Agent_Class;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Money.Price_Type;

   function Remaining_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Agent_Class;
      Commodity : Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   procedure Log_Market
     (Market  : Concorde_Market;
      Message : String);

end Concorde.Markets;
