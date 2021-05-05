with Ada.Text_IO;

with Concorde.Calendar;
with Concorde.Db;
with Concorde.Logging;

with Concorde.Handles.Historical_Stock;
with Concorde.Handles.Stock_Item;

package body Concorde.Stock is

   Log_Stock_Enabled : constant Boolean := False;

   procedure Register_Stock
     (Stock     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class);

   -----------------------
   -- Add_Initial_Stock --
   -----------------------

   procedure Add_Initial_Stock
     (To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Add_Stock (To, Item, Quantity,
                 Concorde.Money.Total
                   (Item.Base_Price,
                    Quantity));
   end Add_Initial_Stock;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
   begin

      if Quantity > Zero then
         pragma Assert (Value > Zero);
         declare
            Stock : constant Concorde.Handles.Stock_Item.Stock_Item_Class :=
              Concorde.Handles.Stock_Item.Get_By_Stock_Item
                (To, Item);
         begin
            if Stock.Has_Element then
               Stock.Update_Stock_Item
                 .Set_Quantity (Stock.Quantity + Quantity)
                 .Set_Value (Stock.Value + Value)
                 .Done;
            else
               Concorde.Handles.Stock_Item.Create
                 (Has_Stock => To,
                  Commodity => Item,
                  Quantity  => Quantity,
                  Value     => Value);
            end if;
         end;
         Register_Stock (To, Item);
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "error while adding "
            & Concorde.Quantities.Show (Quantity)
            & " "
            & Item.Tag
            & " valued at "
            & Concorde.Money.Show (Value)
            & " to stock "
            & To.Identifier);
         raise;
   end Add_Stock;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
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
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
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
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : out Concorde.Quantities.Quantity_Type;
      Value     : out Concorde.Money.Money_Type)
   is
      Stock : constant Concorde.Handles.Stock_Item.Stock_Item_Class :=
                Concorde.Handles.Stock_Item.Get_By_Stock_Item
                  (Has_Stock, Commodity);
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
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
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
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class)
   is
      Actor : constant String :=
                "has-stock"
                & Concorde.Db.To_String (Has_Stock.Reference_Has_Stock);
   begin
      if Log_Stock_Enabled then
         for Stock_Item of
           Concorde.Handles.Stock_Item.Select_By_Has_Stock
             (Has_Stock)
         loop
            Concorde.Logging.Log
              (Actor    => Actor,
               Location => "",
               Category => "stock",
               Message  =>
                 Stock_Item.Commodity.Tag
               & " "
               & Concorde.Quantities.Show (Stock_Item.Quantity));
         end loop;
      end if;
   end Log_Stock;

   ----------------
   -- Move_Stock --
   ----------------

   procedure Move_Stock
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      To       : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
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
     (Stock     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
   is
      Clock : constant Concorde.Calendar.Time :=
        Concorde.Calendar.Clock;
      Historical : constant Concorde.Handles.Historical_Stock
        .Historical_Stock_Class :=
          Concorde.Handles.Historical_Stock.Get_By_Historical_Stock
            (Has_Stock  => Stock,
             Commodity  => Commodity,
             Time_Stamp => Clock);
      Quantity   : Concorde.Quantities.Quantity_Type;
      Value      : Concorde.Money.Money_Type;
   begin
      Get_Stock (Stock, Commodity, Quantity, Value);

      if Historical.Has_Element then
         Historical.Update_Historical_Stock
           .Set_Quantity (Quantity)
           .Set_Value (Value)
           .Done;
      else
         Concorde.Handles.Historical_Stock.Create
           (Time_Stamp => Clock,
            Has_Stock  => Stock,
            Commodity  => Commodity,
            Quantity   => Quantity,
            Value      => Value);
      end if;
   end Register_Stock;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
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
     (From     : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Item     : Concorde.Handles.Commodity.Commodity_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : out Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
   begin

      if Quantity > Zero then
         declare
            Stock : constant Concorde.Handles.Stock_Item.Stock_Item_Class :=
                      Concorde.Handles.Stock_Item.Get_By_Stock_Item
                        (From, Item);
            pragma Assert (Stock.Has_Element);

            Available : constant Quantity_Type :=
                          (if Stock.Quantity >= Quantity
                           then Stock.Quantity
                           elsif To_Real (Stock.Quantity)
                           >= To_Real (Quantity) - 0.1
                           then Quantity
                           else Stock.Quantity);
            Stock_Value : constant Money_Type := Stock.Value;
            Removed_Value : constant Money_Type :=
                              Adjust (Stock_Value,
                                      To_Real (Quantity)
                                      / To_Real (Available));
         begin
            if Available < Quantity then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "remove-stock: "
                  & Item.Tag
                  & ": attempt to remove "
                  & Image (Quantity) & " but have only "
                  & Image (Available));
            end if;

            pragma Assert (Available >= Quantity);

            Value := Removed_Value;

            pragma Assert (Value <= Stock_Value);
            pragma Assert (Value < Stock_Value
                           or else Quantity = Available);

            Stock.Update_Stock_Item
              .Set_Quantity (Available - Quantity)
              .Set_Value (Stock_Value - Value)
              .Done;
         end;
         Register_Stock (From, Item);
      end if;

   end Remove_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Process   : not null access
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type))
   is
   begin
      for Stock_Item of
        Concorde.Handles.Stock_Item.Select_By_Has_Stock
          (Has_Stock)
      loop
         Process (Stock_Item.Commodity, Stock_Item.Quantity, Stock_Item.Value);
      end loop;
   end Scan_Stock;

end Concorde.Stock;
