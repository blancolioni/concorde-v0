with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Real_Images;
with Concorde.Updates.Events;

with Concorde.Db.Colony_Policy;
with Concorde.Db.Network_Value;
with Concorde.Db.Policy;

package body Concorde.UI.Models.Colonies.Policies is

   type Policy_Table_Column is
     (Name, Setting, Income, Expenses);

   type Policy_Record is
      record
         Is_Total : Boolean;
         Policy   : Concorde.Db.Colony_Policy_Reference;
      end record;

   Column_Type_Array : constant array (Policy_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (others => Nazar.Values.Text_Value_Type);

   package Policy_Vectors is
     new Ada.Containers.Vectors
       (Positive, Policy_Record);

   type Policy_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Colony        : Concorde.Handles.Colony.Colony_Handle;
         State         : Policy_Vectors.Vector;
         Total_Income  : Concorde.Money.Money_Type := Concorde.Money.Zero;
         Total_Expense : Concorde.Money.Money_Type := Concorde.Money.Zero;
      end record;

   overriding function Row_Count
     (Model : Policy_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Policy_Model_Record)
      return Natural
   is (Policy_Table_Column'Pos (Policy_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model        : Policy_Model_Record;
      Column_Index : Positive)
      return String
   is (Policy_Table_Column'Image
       (Policy_Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model        : Policy_Model_Record;
      Column_Index : Positive)
      return String
   is (Policy_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model        : Policy_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Policy_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model       : Policy_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Policy_Model_Access is access all Policy_Model_Record'Class;

   procedure Load
     (Model : in out Policy_Model_Record'Class);

   type Policy_Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Policy_Model_Access;
      end record;

   overriding procedure Activate
     (Update : Policy_Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Policy_Model_Update) is
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
     (Model       : Policy_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      use Concorde.Money;

      Info  : Policy_Record renames Model.State (Row);
      Col   : constant Policy_Table_Column :=
                Policy_Table_Column'Val (Column - 1);
   begin
      if Info.Is_Total then
         declare
            Value : constant String :=
                      (case Col is
                          when Name => "TOTAL",
                          when Setting => "",
                          when Income  => Show (Model.Total_Income),
                          when Expenses => Show (Model.Total_Expense));
         begin
            return Nazar.Values.To_Value (Value);
         end;

      else
         declare
            use Concorde.Db.Colony_Policy;
            use Concorde.Db.Network_Value;
            use Concorde.Db.Policy;
            Colony_Policy : constant Colony_Policy_Type := Get (Info.Policy);
            Policy        : constant Policy_Type := Get (Colony_Policy.Policy);
            Value_Node    : constant Network_Value_Type :=
                              Get (Colony_Policy.Setting);
            Value : constant String :=
                      (case Col is
                          when Name     => Policy.Tag,
                          when Setting  =>
                            Concorde.Real_Images.Approximate_Image
                         (Value_Node.Real_Value * 100.0),
                          when Income   =>
                            Concorde.Money.Show (Colony_Policy.Revenue),
                          when Expenses =>
                            Concorde.Money.Show (Colony_Policy.Expense));
         begin
            return Nazar.Values.To_Value (Value);
         end;
      end if;
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Policy_Model_Record'Class)
   is
      use Concorde.Money;
      Total_Income   : Money_Type := Zero;
      Total_Expenses : Money_Type := Zero;
   begin
      Model.State.Clear;
      for Policy of
        Concorde.Db.Policy.Select_By_Internal (False)
      loop
         declare
            use Concorde.Db.Colony_Policy;
            Item : constant Colony_Policy_Type :=
                     Get_By_Colony_Policy
                       (Model.Colony.Reference_Colony,
                        Policy.Get_Policy_Reference);
            Info : constant Policy_Record :=
                     Policy_Record'
                       (Policy   => Item.Get_Colony_Policy_Reference,
                        Is_Total => False);
         begin
            Model.State.Append (Info);
            Total_Income := Total_Income + Item.Revenue;
            Total_Expenses := Total_Expenses + Item.Expense;
         end;
      end loop;
      Model.State.Append
        (Policy_Record'
           (Policy => Concorde.Db.Null_Colony_Policy_Reference,
            Is_Total => True));
      Model.Total_Income := Total_Income;
      Model.Total_Expense := Total_Expenses;
   end Load;

   -----------
   -- Model --
   -----------

   function Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Result : constant Policy_Model_Access := new Policy_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Colony => Colony, others => <>);
   begin
      Result.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Policy_Model_Update'(Model => Result));
      return Nazar.Models.Table.Nazar_Table_Model (Result);
   end Model;

end Concorde.UI.Models.Colonies.Policies;
