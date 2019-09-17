with Ada.Text_IO;

with Concorde.Calendar;
with Concorde.Logging;

with Concorde.Db.Commodity;
with Concorde.Db.Historical_Stock;
with Concorde.Db.Stock_Item;

package body Concorde.Stock is

   Log_Stock_Enabled : constant Boolean := False;

   procedure Register_Stock
     (Stock     : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference);

   -----------------------
   -- Add_Initial_Stock --
   -----------------------

   procedure Add_Initial_Stock
     (To       : Concorde.Db.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Add_Stock (To, Item, Quantity,
                 Concorde.Money.Total
                   (Concorde.Commodities.Initial_Price (Item),
                    Quantity));
   end Add_Initial_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To       : Concorde.Db.Has_Stock.Has_Stock_Type;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
   begin
      if Quantity > Zero then
         pragma Assert (Value > Zero);
         Add_Stock (To.Get_Has_Stock_Reference, Item, Quantity, Value);
      end if;
   end Add_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To       : Concorde.Db.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
   begin

      if Quantity > Zero then
         pragma Assert (Value > Zero);
         declare
            Ref : constant Concorde.Db.Commodity_Reference :=
              Concorde.Commodities.To_Database_Reference (Item);
            Stock : constant Concorde.Db.Stock_Item.Stock_Item_Type :=
              Concorde.Db.Stock_Item.Get_By_Stock_Item
                (To, Ref);
         begin
            if Stock.Has_Element then
               Concorde.Db.Stock_Item.Update_Stock_Item
                 (Stock.Get_Stock_Item_Reference)
                 .Set_Quantity (Stock.Quantity + Quantity)
                 .Set_Value (Stock.Value + Value)
                 .Done;
            else
               Concorde.Db.Stock_Item.Create
                 (Has_Stock => To,
                  Commodity => Ref,
                  Quantity  => Quantity,
                  Value     => Value);
            end if;
         end;
         Register_Stock (To, Item);
      end if;

   end Add_Stock;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Concorde.Money.Price (Value, Quantity);
   end Get_Price_Per_Item;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Quantity;
   end Get_Quantity;

   ---------------
   -- Get_Stock --
   ---------------

   procedure Get_Stock
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : out Concorde.Quantities.Quantity_Type;
      Value     : out Concorde.Money.Money_Type)
   is
      Stock : constant Concorde.Db.Stock_Item.Stock_Item_Type :=
        Concorde.Db.Stock_Item.Get_By_Stock_Item
          (Has_Stock,
           Concorde.Commodities.To_Database_Reference (Commodity));
   begin
      if Stock.Has_Element then
         Quantity := Stock.Quantity;
         Value := Stock.Value;
      else
         Quantity := Quantities.Zero;
         Value := Money.Zero;
      end if;
   end Get_Stock;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Money_Type
   is
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type;
   begin
      Get_Stock (Has_Stock, Commodity, Quantity, Value);
      return Value;
   end Get_Value;

   ---------------
   -- Log_Stock --
   ---------------

   procedure Log_Stock
     (Stock : Concorde.Db.Has_Stock_Reference)
   is
      Actor : constant String :=
                "has-stock" & Concorde.Db.To_String (Stock);
   begin
      if Log_Stock_Enabled then
         for Stock_Item of
           Concorde.Db.Stock_Item.Select_By_Has_Stock
             (Stock)
         loop
            Concorde.Logging.Log
              (Actor    => Actor,
               Location => "",
               Category => "stock",
               Message  =>
                 Concorde.Db.Commodity.Get (Stock_Item.Commodity).Tag
               & " "
               & Concorde.Quantities.Show (Stock_Item.Quantity));
         end loop;
      end if;
   end Log_Stock;

   ----------------
   -- Move_Stock --
   ----------------

   procedure Move_Stock
     (From     : Concorde.Db.Has_Stock_Reference;
      To       : Concorde.Db.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      Q : constant Quantity_Type :=
        Min (Quantity, Get_Quantity (From, Item));
      V : constant Concorde.Money.Money_Type :=
        Concorde.Money.Total (Get_Price_Per_Item (From, Item), Q);
   begin
      Remove_Stock (From, Item, Q);
      Add_Stock (To, Item, Q, V);
   end Move_Stock;

   --------------------
   -- Register_Stock --
   --------------------

   procedure Register_Stock
     (Stock     : Concorde.Db.Has_Stock_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
   is
      use type Concorde.Db.Historical_Stock_Reference;
      Clock : constant Concorde.Calendar.Time :=
        Concorde.Calendar.Clock;
      Ref : constant Concorde.Db.Commodity_Reference :=
        Concorde.Commodities.To_Database_Reference (Commodity);
      Historical : constant Db.Historical_Stock_Reference :=
                     Db.Historical_Stock.Get_Reference_By_Historical_Stock
                       (Stock, Ref, Clock);
      Quantity   : Concorde.Quantities.Quantity_Type;
      Value      : Concorde.Money.Money_Type;
   begin
      Get_Stock (Stock, Commodity, Quantity, Value);

      if Historical /= Concorde.Db.Null_Historical_Stock_Reference then
         Concorde.Db.Historical_Stock.Update_Historical_Stock
           (Historical)
           .Set_Quantity (Quantity)
           .Set_Value (Value)
           .Done;
      else
         Concorde.Db.Historical_Stock.Create
           (Time_Stamp => Clock,
            Has_Stock  => Stock,
            Commodity  => Ref,
            Quantity   => Quantity,
            Value      => Value);
      end if;
   end Register_Stock;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From     : Concorde.Db.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
      Value : Concorde.Money.Money_Type with Unreferenced;
   begin
      Remove_Stock (From, Item, Quantity, Value);
   end Remove_Stock;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From     : Concorde.Db.Has_Stock_Reference;
      Item     : Concorde.Commodities.Commodity_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : out Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
      Ref : constant Concorde.Db.Commodity_Reference :=
        Concorde.Commodities.To_Database_Reference (Item);
   begin

      if Quantity > Zero then
         declare
            Stock : constant Concorde.Db.Stock_Item.Stock_Item_Type :=
                      Concorde.Db.Stock_Item.Get_By_Stock_Item
                        (From, Ref);
            Available : constant Quantity_Type := Stock.Quantity;
         begin
            if Available < Quantity then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "remove-stock: "
                  & Concorde.Commodities.Local_Name (Item)
                  & ": attempt to remove "
                  & Image (Quantity) & " but have only "
                  & Image (Available));
            end if;

            pragma Assert (Stock.Has_Element);
            pragma Assert (Available >= Quantity);

            Value := Total (Price (Stock.Value, Stock.Quantity), Quantity);

            if Value = Stock.Value and then Quantity < Stock.Quantity then
               Value := Stock.Value - To_Money (0.01);
            end if;

            Concorde.Db.Stock_Item.Update_Stock_Item
              (Stock.Get_Stock_Item_Reference)
              .Set_Quantity (Stock.Quantity - Quantity)
              .Set_Value (Stock.Value - Value)
              .Done;
         end;
         Register_Stock (From, Item);
      end if;

   end Remove_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Process   : not null access
        procedure (Item     : Concorde.Commodities.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type))
   is
      Stock : Concorde.Commodities.Stock_Type;
   begin
      Stock.Load (Has_Stock);
      Stock.Iterate (Process);
   end Scan_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Has_Stock : Concorde.Db.Has_Stock.Has_Stock_Type;
      Process   : not null access
        procedure (Item     : Concorde.Commodities.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type))
   is
   begin
      Scan_Stock (Has_Stock.Get_Has_Stock_Reference, Process);
   end Scan_Stock;

end Concorde.Stock;
