private with WL.String_Maps;

with Concorde.Quantities;

with Concorde.Db;

package Concorde.Commodities is

   subtype Commodity_Reference is Concorde.Db.Commodity_Reference;

   type Array_Of_Commodities is
     array (Positive range <>) of Concorde.Db.Commodity_Reference;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Reference
     with Pre => Exists (Tag);

   function Is_Resource
     (Commodity : Commodity_Reference)
      return Boolean;

   function To_Resource
     (Commodity : Commodity_Reference)
      return Concorde.Db.Resource_Reference;

   function Is_Manufactured
     (Commodity : Commodity_Reference)
      return Boolean;

   procedure Scan_Ingredients
     (Commodity : Commodity_Reference;
      Process   : not null access
        procedure (Ingredient : Commodity_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type));

   function Raw_Resources return Commodity_Reference;

   function Current_Quantity
     (Of_Stock  : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Remove_Stock
     (From      : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type);

   type Stock_Type is private;

   function Current_Quantity
     (Of_Stock  : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Scan
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Concorde.Db.Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type));

private

   package Stock_Maps is
     new WL.String_Maps (Concorde.Quantities.Quantity_Type,
                         Concorde.Quantities."=");

   type Stock_Type is new Stock_Maps.Map with null record;

end Concorde.Commodities;
