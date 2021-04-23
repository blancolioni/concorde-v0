private with WL.String_Maps;

with Concorde.Quantities;

with Concorde.Handles.Commodity;
with Concorde.Handles.Has_Stock;
with Concorde.Handles.Resource;

package Concorde.Commodities is

   subtype Commodity_Class is Concorde.Handles.Commodity.Commodity_Class;
   subtype Commodity_Handle is Concorde.Handles.Commodity.Commodity_Handle;

   type Array_Of_Commodities is array (Positive range <>) of Commodity_Handle;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Class
     with Pre => Exists (Tag);

   function Is_Resource
     (Commodity : Commodity_Class)
      return Boolean;

   function To_Resource
     (Commodity : Commodity_Class)
      return Concorde.Handles.Resource.Resource_Handle;

   procedure Scan_Ingredients
     (Commodity : Commodity_Class;
      Process   : not null access
        procedure (Ingredient : Commodity_Class;
                   Quantity   : Concorde.Quantities.Quantity_Type));

   function Raw_Resources return Commodity_Class;

   function Current_Quantity
     (Of_Stock  : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Remove_Stock
     (From      : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   type Stock_Type is private;

   function Current_Quantity
     (Of_Stock  : Stock_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Scan
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type));

private

   package Stock_Maps is
     new WL.String_Maps (Concorde.Quantities.Quantity_Type,
                         Concorde.Quantities."=");

   type Stock_Type is new Stock_Maps.Map with null record;

end Concorde.Commodities;
