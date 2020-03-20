with Concorde.Db.Commodity;
with Concorde.Db.Input_Commodity;
with Concorde.Db.Manufactured;
with Concorde.Db.Resource;
with Concorde.Db.Stock_Item;

package body Concorde.Commodities is

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Db;
      use Concorde.Quantities;
      Item : constant Stock_Item.Stock_Item_Type :=
               Concorde.Db.Stock_Item.Get_By_Stock_Item
                 (To, Commodity);
   begin
      if Item.Has_Element then
         Concorde.Db.Stock_Item.Update_Stock_Item
           (Item.Get_Stock_Item_Reference)
           .Set_Quantity (Item.Quantity + Quantity)
           .Done;
      else
         Concorde.Db.Stock_Item.Create
           (To, Commodity, Quantity);
      end if;
   end Add_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
      Tag : constant String := Concorde.Db.Commodity.Get (Commodity).Tag;
   begin
      if To.Contains (Tag) then
         declare
            Q : Concorde.Quantities.Quantity_Type renames To (Tag);
         begin
            Q := Q + Quantity;
         end;
      else
         To.Insert (Tag, Quantity);
      end if;
   end Add_Stock;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Of_Stock  : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      Stock_Item : constant Concorde.Db.Stock_Item.Stock_Item_Type :=
        Concorde.Db.Stock_Item.Get_By_Stock_Item
          (Of_Stock, Commodity);
   begin
      if Stock_Item.Has_Element then
         return Stock_Item.Quantity;
      else
         return Concorde.Quantities.Zero;
      end if;
   end Current_Quantity;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Of_Stock  : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      Tag : constant String := Concorde.Db.Commodity.Get (Commodity).Tag;
   begin
      if Of_Stock.Contains (Tag) then
         return Of_Stock.Element (Tag);
      else
         return Concorde.Quantities.Zero;
      end if;
   end Current_Quantity;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
      use type Concorde.Db.Commodity_Reference;
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag (Tag)
        /= Concorde.Db.Null_Commodity_Reference;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Commodity_Reference is
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag (Tag);
   end Get;

   ---------------------
   -- Is_Manufactured --
   ---------------------

   function Is_Manufactured
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Concorde.Db.Commodity.Get (Commodity)
        .Top_Record = R_Manufactured;
   end Is_Manufactured;

   -----------------
   -- Is_Resource --
   -----------------

   function Is_Resource
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Concorde.Db.Commodity.Get (Commodity)
        .Top_Record = R_Resource;
   end Is_Resource;

   -------------------
   -- Raw_Resources --
   -------------------

   function Raw_Resources return Commodity_Reference is
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag ("raw-resources");
   end Raw_Resources;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From      : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Stock : constant Concorde.Db.Stock_Item.Stock_Item_Type :=
        Concorde.Db.Stock_Item.Get_By_Stock_Item (From, Commodity);
   begin
      pragma Assert (Stock.Has_Element or else Quantity = Zero);
      pragma Assert (Quantity = Zero
                     or else Quantity <= Scale (Stock.Quantity, 1.01));
      if Quantity > Zero then
         Concorde.Db.Stock_Item.Update_Stock_Item
           (Stock.Get_Stock_Item_Reference)
           .Set_Quantity (Stock.Quantity - Min (Quantity, Stock.Quantity))
           .Done;
      end if;
   end Remove_Stock;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Stock   : Stock_Type;
      Process : not null access
        procedure (Commodity : Concorde.Db.Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Position in Stock.Iterate loop
         Process
           (Concorde.Db.Commodity.Get_Reference_By_Tag
              (Stock_Maps.Key (Position)),
            Stock_Maps.Element (Position));
      end loop;
   end Scan;

   ----------------------
   -- Scan_Ingredients --
   ----------------------

   procedure Scan_Ingredients
     (Commodity : Commodity_Reference;
      Process   : not null access
        procedure (Ingredient : Commodity_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Input of
        Concorde.Db.Input_Commodity.Select_By_Manufactured
          (Concorde.Db.Manufactured.Get_Manufactured (Commodity)
           .Get_Manufactured_Reference)
      loop
         Process (Input.Commodity, Input.Quantity);
      end loop;
   end Scan_Ingredients;

   -----------------
   -- To_Resource --
   -----------------

   function To_Resource
     (Commodity : Commodity_Reference)
      return Concorde.Db.Resource_Reference
   is
   begin
      return Concorde.Db.Resource.Get_Resource (Commodity)
        .Get_Resource_Reference;
   end To_Resource;

end Concorde.Commodities;
