with Concorde.Commodities;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db;

package Concorde.Markets is

   subtype Concorde_Market is Concorde.Handles.Market_Reference;

   function Current_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   function Historical_Mean_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type;

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Cash      : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type;

   function Current_Value
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type;

   procedure Create_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Account   : Concorde.Handles.Account_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   function Previous_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   function Previous_Agent_Offer_Price
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Money.Price_Type;

   function Remaining_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type;

   procedure Log_Market
     (Market  : Concorde_Market;
      Message : String);

end Concorde.Markets;
