with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.UI.Models.Data_Source;
with Concorde.UI.Models.Renderers;
with Concorde.UI.Models.Values;

with Concorde.Commodities;
with Concorde.Markets;

with Concorde.Db.Commodity;
with Concorde.Db.Market;
with Concorde.Db.World;

package body Concorde.UI.Models.Markets is

   type Market_Price_Row is
      record
         Commodity      : Concorde.Commodities.Commodity_Reference;
         Supply, Demand : Real;
         Price          : Non_Negative_Real;
      end record;

   package Market_Table is
     new Ada.Containers.Vectors
       (Positive, Market_Price_Row);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Market_Price_Column is
     (Commodity_Tag, Commodity_Name, Supply, Demand, Price);

   type Market_Price_Model_Type is
     new Concorde.UI.Models.Data_Source.Root_Data_Source_Model with
      record
         Market   : Concorde.Db.Market_Reference;
         Headings : String_Vectors.Vector;
         Ids      : String_Vectors.Vector;
         Data     : Market_Table.Vector;
      end record;

   overriding function Name
     (Model : Market_Price_Model_Type)
      return String
   is ("market-price-data-source");

   overriding function Column_Count
     (Model : Market_Price_Model_Type)
      return Natural
   is (Model.Ids.Last_Index);

   overriding function Row_Count
     (Model : Market_Price_Model_Type)
      return Natural
   is (Model.Data.Last_Index);

   overriding function Column_Heading_Id
     (Model       : Market_Price_Model_Type;
      Column      : Positive)
      return String
   is (Model.Ids.Element (Column));

   overriding function Column_Heading_Label
     (Model       : Market_Price_Model_Type;
      Column      : Positive)
      return String
   is (Model.Headings.Element (Column));

   overriding function Column_Type
     (Model       : Market_Price_Model_Type;
      Column      : Positive)
      return Values.Model_Value_Data_Type
   is (case Market_Price_Column'Val (Column - 1) is
          when Commodity_Tag | Commodity_Name  =>
             Values.Text_Type,
          when Supply | Demand | Price =>
             Values.Real_Type);

   overriding function Column_Renderer
     (Model       : Market_Price_Model_Type;
      Column      : Positive)
      return Renderers.Render_Interface'Class
   is (case Market_Price_Column'Val (Column - 1) is
          when Commodity_Tag | Commodity_Name =>
             Renderers.Default_Renderer,
          when Supply | Demand =>
             Renderers.Quantity_Renderer,
          when Price           =>
             Renderers.Price_Renderer);

   overriding function Cell_Value
     (Model  : Market_Price_Model_Type;
      Row    : Positive;
      Column : Positive)
      return Values.Model_Value_Type;

   ----------------
   -- Cell_Value --
   ----------------

   overriding function Cell_Value
     (Model  : Market_Price_Model_Type;
      Row    : Positive;
      Column : Positive)
      return Values.Model_Value_Type
   is
      use Concorde.UI.Models.Values;
      Price_Row : Market_Price_Row renames Model.Data (Row);
   begin
      case Market_Price_Column'Val (Column - 1) is
         when Commodity_Tag =>
            return Text_Value
              (Concorde.Db.Commodity.Get
                 (Concorde.Commodities.To_Database_Reference
                      (Price_Row.Commodity))
               .Tag);
         when Commodity_Name =>
            return Text_Value
              (Concorde.Commodities.Local_Name (Price_Row.Commodity));
         when Supply =>
            return Real_Value
              (Price_Row.Supply);
         when Demand =>
            return Real_Value
              (Price_Row.Demand);
         when Price =>
            return Real_Value
              (Price_Row.Price);
      end case;
   end Cell_Value;

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
         Model.Ids.Append ("id");
         Model.Ids.Append ("name");
         Model.Ids.Append ("supply");
         Model.Ids.Append ("demand");
         Model.Ids.Append ("price");

         Model.Headings.Append ("Id");
         Model.Headings.Append ("Commodity");
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
               use Concorde.Markets;
               use Concorde.Quantities, Concorde.Money;
               Supply : constant Quantity_Type :=
                 Historical_Offer_Quantity
                   (Market, Commodity, Db.Ask, Calendar.Days (1));
               Demand : constant Quantity_Type :=
                 Historical_Offer_Quantity
                   (Market, Commodity, Db.Bid, Calendar.Days (1));
               Price  : constant Price_Type :=
                 Historical_Mean_Price (Market, Commodity);
            begin
               Model.Data.Append
                 (Market_Price_Row'
                    (Commodity => Commodity,
                     Supply    => To_Real (Supply),
                     Demand    => To_Real (Demand),
                     Price     => To_Real (Price)));
            end Add_Row;

         begin
            for Commodity of Concorde.Commodities.All_Commodities loop
               Add_Row (Commodity);
            end loop;
         end;
      end return;
   end Market_Price_Model;

end Concorde.UI.Models.Markets;
