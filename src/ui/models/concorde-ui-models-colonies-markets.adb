with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Quantities;
with Concorde.Real_Images;
with Concorde.Updates.Events;

with Concorde.Handles.Commodity;

with Concorde.Db.Commodity;
with Concorde.Db.Network_Value;
with Concorde.Db.Node;

package body Concorde.UI.Models.Colonies.Markets is

   type Market_Table_Column is
     (Name, Demand, Supply, Share, Pressure, Price);

   type Market_Record is
      record
         Commodity : Concorde.Handles.Commodity.Commodity_Handle;
         Demand    : Concorde.Db.Network_Value_Reference;
         Supply    : Concorde.Db.Network_Value_Reference;
         Share     : Concorde.Db.Network_Value_Reference;
         Pressure  : Concorde.Db.Network_Value_Reference;
         Price     : Concorde.Db.Network_Value_Reference;
      end record;

   Column_Type_Array : constant array (Market_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (others => Nazar.Values.Text_Value_Type);

   package Market_Vectors is
     new Ada.Containers.Vectors
       (Positive, Market_Record);

   type Market_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Colony        : Concorde.Handles.Colony.Colony_Handle;
         Network       : Concorde.Db.Network_Reference;
         State         : Market_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Market_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Market_Model_Record)
      return Natural
   is (Market_Table_Column'Pos (Market_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model        : Market_Model_Record;
      Column_Index : Positive)
      return String
   is (Market_Table_Column'Image
       (Market_Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model        : Market_Model_Record;
      Column_Index : Positive)
      return String
   is (Market_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model        : Market_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Market_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model       : Market_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Market_Model_Access is access all Market_Model_Record'Class;

   procedure Load
     (Model : in out Market_Model_Record'Class);

   type Market_Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Market_Model_Access;
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

   -------------
   -- Element --
   -------------

   overriding function Element
     (Model       : Market_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Market_Record renames Model.State (Row);
      Col   : constant Market_Table_Column :=
                Market_Table_Column'Val (Column - 1);

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
      is (Image (Get_Value (Info.Price) * 100.0));

      Value : constant String :=
                (case Col is
                    when Name     => Info.Commodity.Tag,
                    when Supply   => Supply,
                    when Demand   => Demand,
                    when Share    => Share,
                    when Pressure => Pressure,
                    when Price    => Price);
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Market_Model_Record'Class)
   is
      procedure Add (Commodity : String);

      ---------
      -- Add --
      ---------

      procedure Add (Commodity : String) is

         function Net (Suffix : String)
                       return Concorde.Db.Network_Value_Reference
         is (Concorde.Db.Network_Value.Get_Reference_By_Network_Value
             (Model.Network,
              Concorde.Db.Node.Get_Reference_By_Tag
                (Commodity & "-" & Suffix)));

         Handle : constant Concorde.Handles.Commodity.Commodity_Handle :=
                    Concorde.Handles.Commodity.Get
                      (Concorde.Db.Commodity.Get_Reference_By_Tag (Commodity));
         Demand : constant Concorde.Db.Network_Value_Reference :=
                    Net ("demand");
         Supply : constant Concorde.Db.Network_Value_Reference :=
                    Net ("supply");
         Share  : constant Concorde.Db.Network_Value_Reference :=
                    Net ("share");
         Pressure : constant Concorde.Db.Network_Value_Reference :=
                    Net ("pressure");
         Price : constant Concorde.Db.Network_Value_Reference :=
                    Net ("price");

      begin
         Model.State.Append
           (Market_Record'
              (Commodity => Handle,
               Demand    => Demand,
               Supply    => Supply,
               Share     => Share,
               Pressure  => Pressure,
               Price     => Price));
      end Add;

   begin

      Model.State.Clear;
      Add ("silicon");
      Add ("hydrocarbon");
      Add ("crystals");
      Add ("plastic");
      Add ("refined-crystals");
      Add ("food");
      Add ("basic-electronics");
   end Load;

   -----------
   -- Model --
   -----------

   function Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Result : constant Market_Model_Access := new Market_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Colony => Colony,
         Network => Colony.Network_Handle.Reference_Network,
         others => <>);
   begin
      Result.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Market_Model_Update'(Model => Result));
      return Nazar.Models.Table.Nazar_Table_Model (Result);
   end Model;

end Concorde.UI.Models.Colonies.Markets;
