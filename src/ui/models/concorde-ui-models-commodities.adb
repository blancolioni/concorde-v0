with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;

with Concorde.Updates.Events;

with Concorde.Db.Commodity;

with Concorde.Db.Colony;
with Concorde.Handles.Colony;

with Concorde.Handles.World;

with Concorde.Db.Colony_Price;
with Concorde.Db.Network_Value;
with Concorde.Db.Node;

package body Concorde.UI.Models.Commodities is

   type Table_Column is
     (World_Name, Demand, Supply, Share, Pressure, Price);

   type Row_Record is
      record
         World      : Concorde.Handles.World.World_Handle;
         Colony     : Concorde.Handles.Colony.Colony_Handle;
         Demand     : Concorde.Db.Network_Value_Reference;
         Supply     : Concorde.Db.Network_Value_Reference;
         Share      : Concorde.Db.Network_Value_Reference;
         Pressure   : Concorde.Db.Network_Value_Reference;
         Price      : Concorde.Db.Network_Value_Reference;
      end record;

   Column_Type_Array : constant array (Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (World_Name   => Nazar.Values.Text_Value_Type,
        Demand       => Nazar.Values.Text_Value_Type,
        Supply       => Nazar.Values.Text_Value_Type,
        Share        => Nazar.Values.Text_Value_Type,
        Pressure     => Nazar.Values.Text_Value_Type,
        Price        => Nazar.Values.Text_Value_Type);

   package Row_Vectors is
     new Ada.Containers.Vectors
       (Positive, Row_Record);

   type Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Commodity : Concorde.Db.Commodity_Reference;
         State     : Row_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Model_Record)
      return Natural
   is (Table_Column'Pos (Table_Column'Last) + 1);

   overriding function Column_Name
     (Model : Model_Record;
      Column_Index : Positive)
      return String
   is (Table_Column'Image
       (Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model : Model_Record;
      Column_Index : Positive)
      return String
   is (Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model : Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model : Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Model_Access is
     access all Model_Record'Class;

   procedure Load
     (Model : in out Model_Record'Class);

   type Market_Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Model_Access;
      end record;

   overriding procedure Activate
     (Update : Market_Model_Update);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Market_Model_Update) is
   begin
      Update.Model.Load;
      Update.Model.Notify_Observers;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1), Update);
   end Activate;

   ----------------------------
   -- Commodity_Market_Model --
   ----------------------------

   function Commodity_Market_Model
     (Commodity : Concorde.Db.Commodity_Reference)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Model : constant Model_Access := new Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Commodity => Commodity,
         State  => <>);
   begin
      Model.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Market_Model_Update'(Model => Model));
      return Nazar.Models.Table.Nazar_Table_Model (Model);
   end Commodity_Market_Model;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Model : Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Row_Record renames Model.State (Row);
      Col   : constant Table_Column :=
                Table_Column'Val (Column - 1);

      function Get_Value (V : Concorde.Db.Network_Value_Reference) return Real
      is (Concorde.Db.Network_Value.Get (V).Current_Value);

      function Quantity (X : Real) return String
      is (Concorde.Quantities.Show (Concorde.Quantities.To_Quantity (X)));

      function Supply return String
      is (Quantity (Get_Value (Info.Supply)));

      function Demand return String
      is (Quantity (Get_Value (Info.Demand)));

      function Share return String
      is (Image (Get_Value (Info.Share) * 100.0));

      function Pressure return String
      is (Image (Get_Value (Info.Pressure) * 100.0));

      function Price return String
      is (Concorde.Money.Show
          (Concorde.Money.Adjust_Price
           (Concorde.Db.Colony_Price.Get_By_Commodity_Price
            (Info.Colony.Reference_Colony, Model.Commodity)
            .Price,
            1.0 + Get_Value (Info.Price))));

      Value : constant String :=
                (case Col is
                    when World_Name => Info.World.Name,
                    when Supply     => Supply,
                    when Demand     => Demand,
                    when Share      => Share,
                    when Pressure   => Pressure,
                    when Price      => Price);
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Model_Record'Class)
   is

      Tag : constant String :=
              Concorde.Db.Commodity.Get (Model.Commodity).Tag;

      procedure Add (Colony : Concorde.Db.Colony.Colony_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Colony : Concorde.Db.Colony.Colony_Type) is

         function Net (Suffix : String)
                       return Concorde.Db.Network_Value_Reference
         is (Concorde.Db.Network_Value.Get_Reference_By_Network_Value
             (Colony.Get_Network_Reference,
              Concorde.Db.Node.Get_Reference_By_Tag
                (Tag & "-" & Suffix)));

         Demand   : constant Concorde.Db.Network_Value_Reference :=
                      Net ("demand");
         Supply   : constant Concorde.Db.Network_Value_Reference :=
                      Net ("supply");
         Share    : constant Concorde.Db.Network_Value_Reference :=
                      Net ("share");
         Pressure : constant Concorde.Db.Network_Value_Reference :=
                      Net ("p-prod");
         Price    : constant Concorde.Db.Network_Value_Reference :=
                      Net ("price");

      begin
         Model.State.Append
           (Row_Record'
              (World    => Concorde.Handles.World.Get (Colony.World),
               Colony   => Concorde.Handles.Colony.Get
                 (Colony.Get_Colony_Reference),
               Demand    => Demand,
               Supply    => Supply,
               Share     => Share,
               Pressure  => Pressure,
               Price     => Price));
      end Add;

   begin
      Model.State.Clear;
      for Colony of
        Concorde.Db.Colony.Scan_By_Top_Record
      loop
         Add (Colony);
      end loop;
   end Load;

end Concorde.UI.Models.Commodities;