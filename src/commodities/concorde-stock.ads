with Concorde.Money;
with Concorde.Quantities;

with Concorde.Handles.Commodity;
with Concorde.Handles.Has_Stock;

package Concorde.Stock is

   procedure Add_Stock
     (To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type);

   procedure Remove_Stock
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Remove_Stock
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : out Concorde.Money.Money_Type);

   procedure Move_Stock
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Add_Initial_Stock
     (To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type);

   function Get_Quantity
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Get_Value
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Money_Type;

   function Get_Price_Per_Item
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type;

   procedure Get_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : out Concorde.Quantities.Quantity_Type;
      Value     : out Concorde.Money.Money_Type);

   procedure Scan_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Process   : not null access
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type));

   procedure Log_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class);

end Concorde.Stock;
