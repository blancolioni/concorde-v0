with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db.Historical_Ask;
with Concorde.Db.Historical_Bid;
with Concorde.Db.Commodity;
with Concorde.Db.Transaction;

package body Concorde.UI.Models.Market is

   type Market_Model_Data is
     new Concorde.Markets.Market_Data with
      record
         Model : Market_Model;
      end record;

   procedure Create_Table
     (Model : Market_Model);

   procedure On_Market_Offer
     (Data      : Concorde.Markets.Market_Data'Class;
      Offer     : Concorde.Db.Offer_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   procedure On_Market_Transaction
     (Data      : Concorde.Markets.Market_Data'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   Commodity_Column     : constant := 1;
   Bid_Column           : constant := 2;
   Ask_Column           : constant := 3;
   Last_Trade_Column    : constant := 4;
   Last_Price_Column    : constant := 5;
   Last_Quantity_Column : constant := 6;
   Demand_Column        : constant := 7;
   Supply_Column        : constant := 8;

   procedure Update_Commodity
     (Model     : Market_Model;
      Commodity : Concorde.Db.Commodity_Reference;
      Row       : Concorde.UI.Models.Tables.Table_Row_Index);

   ------------
   -- Create --
   ------------

   function Create
     (Market  : Concorde.Db.Market_Reference)
      return Market_Model
   is
      use Concorde.UI.Models.Tables;
   begin
      return Model : constant Market_Model := new Root_Market_Model do
         Model.Reference := Market;
         Model.Add_Column ("commodity");
         Model.Add_Column ("bid");
         Model.Add_Column ("ask");
         Model.Add_Column ("last-trade");
         Model.Add_Column ("last-price");
         Model.Add_Column ("last-traded");
         Model.Add_Column ("demand");
         Model.Add_Column ("supply");

         Create_Table (Model);

         declare
            Data : constant Market_Model_Data :=
                     (Model => Model);
         begin
            Model.Market_Watcher_Id :=
              Concorde.Markets.Add_Market_Watcher
                (Market         => Market,
                 Data           => Data,
                 On_Offer       => On_Market_Offer'Access,
                 On_Transaction => On_Market_Transaction'Access);
         end;

      end return;
   end Create;

   ------------------
   -- Create_Table --
   ------------------

   procedure Create_Table
     (Model : Market_Model)
   is
   begin
      Model.Clear_Rows;
      for Commodity of Concorde.Db.Commodity.Scan_By_Tag loop
         declare
            use Concorde.UI.Models.Tables;
            Row   : constant Table_Row_Index :=
                      Model.Add_Row;
         begin
            Model.Commodity_Row.Replace_Element
              (Commodity.Get_Commodity_Reference, Row);

            Model.Set_Cell
              (Row, Commodity_Column,
               Concorde.Commodities.Local_Name
                 (Commodity.Get_Commodity_Reference));
            for I in Table_Column_Index range Bid_Column .. Supply_Column loop
               Model.Set_Cell
                 (Row, I, "-");
            end loop;

            Update_Commodity (Model, Commodity.Get_Commodity_Reference, Row);

         end;
      end loop;
      Model.Clear_Changes;
   end Create_Table;

   ---------------------
   -- On_Market_Offer --
   ---------------------

   procedure On_Market_Offer
     (Data      : Concorde.Markets.Market_Data'Class;
      Offer     : Concorde.Db.Offer_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      pragma Unreferenced (Offer, Quantity, Price);
      use Concorde.UI.Models.Tables;
      Model : constant Market_Model :=
                Market_Model_Data (Data).Model;
      Row   : constant Table_Row_Index :=
                Model.Commodity_Row.Element (Commodity);
   begin
      Update_Commodity (Model, Commodity, Row);
      Model.Notify_Changed;
   end On_Market_Offer;

   ---------------------------
   -- On_Market_Transaction --
   ---------------------------

   procedure On_Market_Transaction
     (Data      : Concorde.Markets.Market_Data'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      pragma Unreferenced (Quantity, Price);
      use Concorde.UI.Models.Tables;
      Model : constant Market_Model :=
                Market_Model_Data (Data).Model;
      Row   : constant Table_Row_Index :=
                Model.Commodity_Row.Element (Commodity);
   begin
      Update_Commodity (Model, Commodity, Row);
      Model.Notify_Changed;
   end On_Market_Transaction;

   ----------------------
   -- Update_Commodity --
   ----------------------

   procedure Update_Commodity
     (Model     : Market_Model;
      Commodity : Concorde.Db.Commodity_Reference;
      Row       : Concorde.UI.Models.Tables.Table_Row_Index)
   is
      use Concorde.Money, Concorde.Quantities;
      use type Concorde.Calendar.Time;
      Price    : Price_Type := Zero;
      Quantity : Quantity_Type := Zero;
      Now      : constant Concorde.Calendar.Time := Concorde.Calendar.Clock;
      Date     : Concorde.Calendar.Time := Now;
   begin
      for Offer of
        Concorde.Db.Historical_Bid
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Concorde.Calendar.Days (1), Now)
      loop
         Price := Offer.Price;
         Quantity := Quantity + Offer.Quantity;
      end loop;

      Model.Set_Cell
        (Row, Bid_Column, Concorde.Money.Show (Price));
      Model.Set_Cell (Row, Demand_Column, Show (Quantity));

      Price := Zero;
      Quantity := Zero;

      for Offer of
        Concorde.Db.Historical_Ask
          .Select_Historical_Offer_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Concorde.Calendar.Days (1), Now)
      loop
         Price := Offer.Price;
         Quantity := Quantity + Offer.Quantity;
      end loop;

      Model.Set_Cell
        (Row, Ask_Column, Concorde.Money.Show (Price));
      Model.Set_Cell (Row, Supply_Column, Show (Quantity));

      for Transaction of
        Concorde.Db.Transaction
          .Select_Transaction_Bounded_By_Time_Stamp
            (Model.Reference, Commodity,
             Now - Concorde.Calendar.Days (7), Now)
      loop
         Price := Transaction.Price;
         Quantity := Transaction.Quantity;
         Date := Transaction.Time_Stamp;
      end loop;

      Model.Set_Cell
        (Row, Last_Trade_Column,
         Concorde.Calendar.Image (Date));
      Model.Set_Cell
        (Row, Last_Price_Column,
         Concorde.Money.Show (Price));
      Model.Set_Cell
        (Row, Last_Quantity_Column,
         Concorde.Quantities.Show (Quantity));

   end Update_Commodity;

end Concorde.UI.Models.Market;
