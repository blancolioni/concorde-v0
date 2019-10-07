with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.UI.Models.Data_Source;

with Concorde.Commodities;
with Concorde.Markets;

with Concorde.Db.Market;
with Concorde.Db.World;

package body Concorde.UI.Models.Markets is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Market_Table is
     new Ada.Containers.Vectors
       (Positive, String_Vectors.Vector, String_Vectors."=");

   type Market_Price_Model_Type is
     new Concorde.UI.Models.Data_Source.Root_Data_Source_Model with
      record
         Market   : Concorde.Db.Market_Reference;
         Headings : String_Vectors.Vector;
         Data     : Market_Table.Vector;
      end record;

   overriding function Column_Count
     (Model : Market_Price_Model_Type)
      return Natural
   is (Model.Headings.Last_Index);

   overriding function Row_Count
     (Model : Market_Price_Model_Type)
      return Natural
   is (Model.Data.Last_Index);

   overriding function Column_Heading
     (Model : Market_Price_Model_Type;
      Column      : Positive)
      return String
   is (Model.Headings.Element (Column));

   overriding function Cell_Value
     (Model : Market_Price_Model_Type;
      Row         : Positive;
      Column      : Positive)
      return String
   is (Model.Data.Element (Row).Element (Column));

   ------------------------
   -- Market_Price_Model --
   ------------------------

   function Market_Price_Model
     (World_Name : String)
      return Root_Concorde_Model'Class
   is
      World : constant Concorde.Db.World_Reference :=
        Concorde.Db.World.First_Reference_By_Name (World_Name);
      Market : constant Concorde.Db.Market_Reference :=
        Concorde.Db.Market.Get_Reference_By_World (World);
   begin
      return Model : Market_Price_Model_Type do
         Model.Market := Market;
         Model.Headings.Append ("Name");
         Model.Headings.Append ("Supply");
         Model.Headings.Append ("Demand");
         Model.Headings.Append ("Price");

         declare
            use Concorde.Commodities;

            procedure Add_Row
              (Commodity : Commodity_Reference);

            -------------
            -- Add_Row --
            -------------

            procedure Add_Row
              (Commodity : Commodity_Reference)
            is
               Row : String_Vectors.Vector;
            begin
               Row.Append (Local_Name (Commodity));
               Row.Append
                 (Concorde.Quantities.Show
                    (Concorde.Markets.Historical_Offer_Quantity
                         (Market    => Market,
                          Commodity => Commodity,
                          Offer     => Concorde.Db.Ask,
                          Since     => Concorde.Calendar.Days (1))));
               Row.Append
                 (Concorde.Quantities.Show
                    (Concorde.Markets.Historical_Offer_Quantity
                         (Market    => Market,
                          Commodity => Commodity,
                          Offer     => Concorde.Db.Bid,
                          Since     => Concorde.Calendar.Days (1))));
               Row.Append
                 (Concorde.Money.Show
                    (Concorde.Markets.Historical_Mean_Price
                         (Market    => Market,
                          Commodity => Commodity)));
               Model.Data.Append (Row);
            end Add_Row;

         begin
            for Commodity of Concorde.Commodities.All_Commodities loop
               Add_Row (Commodity);
            end loop;
         end;
      end return;
   end Market_Price_Model;

end Concorde.UI.Models.Markets;
