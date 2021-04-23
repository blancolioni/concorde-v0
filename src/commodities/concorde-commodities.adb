with Concorde.Handles.Input_Commodity;
with Concorde.Handles.Stock_Item;

with Concorde.Db;

package body Concorde.Commodities is

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Item : constant Concorde.Handles.Stock_Item.Stock_Item_Handle :=
               Concorde.Handles.Stock_Item.Get_By_Stock_Item
                 (To, Commodity);
   begin
      if Item.Has_Element then
         Item.Update_Stock_Item
           .Set_Quantity (Item.Quantity + Quantity)
           .Done;
      else
         Concorde.Handles.Stock_Item.Create
           (To, Commodity, Quantity);
      end if;
   end Add_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To        : in out Stock_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
      Tag : constant String := Commodity.Tag;
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
     (Of_Stock  : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
      Stock_Item : constant Handles.Stock_Item.Stock_Item_Handle :=
        Concorde.Handles.Stock_Item.Get_By_Stock_Item
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
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
      Tag : constant String := Commodity.Tag;
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
   begin
      return Concorde.Handles.Commodity.Get_By_Tag (Tag).Has_Element;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Commodity_Class is
   begin
      return Concorde.Handles.Commodity.Get_By_Tag (Tag);
   end Get;

   -----------------
   -- Is_Resource --
   -----------------

   function Is_Resource
     (Commodity : Commodity_Class)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Commodity.Top_Record = R_Resource;
   end Is_Resource;

   -------------------
   -- Raw_Resources --
   -------------------

   function Raw_Resources return Commodity_Class is
   begin
      return Concorde.Handles.Commodity.Get_By_Tag ("raw-resources");
   end Raw_Resources;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From      : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Stock : constant Concorde.Handles.Stock_Item.Stock_Item_Handle :=
        Concorde.Handles.Stock_Item.Get_By_Stock_Item (From, Commodity);
   begin
      pragma Assert (Stock.Has_Element or else Quantity = Zero);
      pragma Assert (Quantity = Zero
                     or else Quantity <= Scale (Stock.Quantity, 1.01));
      if Quantity > Zero then
         Stock.Update_Stock_Item
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
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Position in Stock.Iterate loop
         Process
           (Concorde.Handles.Commodity.Get_By_Tag
              (Stock_Maps.Key (Position)),
            Stock_Maps.Element (Position));
      end loop;
   end Scan;

   ----------------------
   -- Scan_Ingredients --
   ----------------------

   procedure Scan_Ingredients
     (Commodity : Commodity_Class;
      Process   : not null access
        procedure (Ingredient : Commodity_Class;
                   Quantity   : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Input of
        Concorde.Handles.Input_Commodity.Select_By_Commodity
          (Commodity)
      loop
         Process (Input.Input, Input.Quantity);
      end loop;
   end Scan_Ingredients;

   -----------------
   -- To_Resource --
   -----------------

   function To_Resource
     (Commodity : Commodity_Class)
      return Concorde.Handles.Resource.Resource_Handle
   is
   begin
      return Concorde.Handles.Resource.Get_From_Commodity (Commodity);
   end To_Resource;

end Concorde.Commodities;
