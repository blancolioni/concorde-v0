with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;

with Concorde.Updates.Events;

with Concorde.Db.Pop;
with Concorde.Handles.Pop;

with Concorde.Db.Account;
with Concorde.Db.Utility_Class;

package body Concorde.UI.Models.Population is

   type Population_Table_Column is
     (Utility, Size, Cash, Salary, Hours, Health, Happiness, Education);

   type Population_Record is
      record
         Population : Concorde.Handles.Pop.Pop_Handle;
         Utility    : Concorde.Db.Utility_Class_Reference;
         Size       : Concorde.Quantities.Quantity_Type;
         Cash       : Concorde.Money.Money_Type;
         Salary     : Concorde.Money.Price_Type;
         Hours      : Real;
         Health     : Real;
         Happiness  : Real;
         Education  : Real;
      end record;

   Column_Type_Array : constant array (Population_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (others => Nazar.Values.Text_Value_Type);

   package Population_Vectors is
     new Ada.Containers.Vectors
       (Positive, Population_Record);

   type Population_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         World  : Concorde.Db.World_Reference;
         State  : Population_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Population_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Population_Model_Record)
      return Natural
   is (Population_Table_Column'Pos (Population_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model        : Population_Model_Record;
      Column_Index : Positive)
      return String
   is (Population_Table_Column'Image
       (Population_Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model        : Population_Model_Record;
      Column_Index : Positive)
      return String
   is (Population_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model        : Population_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Population_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model       : Population_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Population_Model_Access is access all Population_Model_Record'Class;

   procedure Load
     (Model : in out Population_Model_Record'Class);

   type Population_Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Population_Model_Access;
      end record;

   overriding procedure Activate
     (Update : Population_Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Population_Model_Update) is
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
     (Model       : Population_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Population_Record renames Model.State (Row);
      Value : constant String :=
        (case Population_Table_Column'Val (Column - 1) is
            when Utility =>
              Concorde.Db.Utility_Class.Get (Info.Utility).Name,
            when Size    =>
              Concorde.Quantities.Show (Info.Size),
            when Cash    =>
              Concorde.Money.Show (Info.Cash),
            when Salary    =>
              Concorde.Money.Show (Info.Salary),
            when Hours    =>
              Concorde.Real_Images.Approximate_Image (Info.Hours),
            when Health   =>
              Concorde.Real_Images.Approximate_Image (Info.Health),
            when Happiness =>
              Concorde.Real_Images.Approximate_Image (Info.Happiness),
            when Education =>
              Concorde.Real_Images.Approximate_Image (Info.Education));
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Population_Model_Record'Class)
   is
   begin
      Model.State.Clear;
      for Pop of
        Concorde.Db.Pop.Select_By_World (Model.World)
      loop
         declare
            Ref  : constant Concorde.Db.Pop_Reference :=
              Pop.Get_Pop_Reference;
            Info : constant Population_Record :=
              Population_Record'
                (Population =>
                   Concorde.Handles.Pop.Get (Ref),
                 Utility    => Pop.Utility_Class,
                 Size       => Pop.Size,
                 Cash       => Concorde.Db.Account.Get (Pop.Account).Cash,
                 Salary     => Pop.Salary,
                 Hours      => Pop.Hours,
                 Health     => Pop.Health,
                 Happiness  => Pop.Happy,
                 Education  => Pop.Education);
         begin
            Model.State.Append (Info);
         end;
      end loop;
   end Load;

   ------------------
   -- Population_Model --
   ------------------

   function Population_Model
     (World : Concorde.Handles.World.World_Class)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Model : constant Population_Model_Access := new Population_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         World => World.Reference_World,
         State  => <>);
   begin
      Model.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Population_Model_Update'(Model => Model));
      return Nazar.Models.Table.Nazar_Table_Model (Model);
   end Population_Model;

end Concorde.UI.Models.Population;
