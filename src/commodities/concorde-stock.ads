with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Handles.Has_Stock;

package Concorde.Stock is

   procedure Add_Stock
     (To       : Concorde.Handles.Has_Stock.Has_Stock_Type;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type);

   procedure Add_Stock
     (To       : Concorde.Handles.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type);

--     procedure Set_Stock
--       (To       : Concorde.Handles.Has_Stock_Reference;
--        Item     : Concorde.Handles.Commodity.Commodity_Class;
--        Quantity : Concorde.Quantities.Quantity_Type;
--        Value    : Concorde.Money.Money_Type);

   procedure Remove_Stock
     (From     : Concorde.Handles.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Remove_Stock
     (From     : Concorde.Handles.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : out Concorde.Money.Money_Type);

   procedure Move_Stock
     (From     : Concorde.Handles.Has_Stock_Reference;
      To       : Concorde.Handles.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Add_Initial_Stock
     (To       : Concorde.Handles.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type);

   function Get_Quantity
     (Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Get_Value
     (Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Money_Type;

   function Get_Price_Per_Item
     (Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type;

   procedure Get_Stock
     (Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : out Concorde.Quantities.Quantity_Type;
      Value     : out Concorde.Money.Money_Type);

   procedure Scan_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Type;
      Process   : not null access
        procedure (Item     : Concorde.Commodities.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type));

   procedure Scan_Stock
     (Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Process   : not null access
        procedure (Item     : Concorde.Commodities.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type));

   procedure Log_Stock
     (Stock : Concorde.Handles.Has_Stock_Reference);

end Concorde.Stock;
