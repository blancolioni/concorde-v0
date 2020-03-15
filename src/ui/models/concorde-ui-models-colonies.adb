with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
--  with Concorde.Money;
--  with Concorde.Quantities;
with Concorde.Real_Images;

with Concorde.Updates.Events;

with Concorde.Db.Pop_Group;

with Concorde.Db.Network_Value;

with Concorde.Db.Node;

with Concorde.UI.Models.Colonies.Policies;

package body Concorde.UI.Models.Colonies is

   type Pop_Group_Table_Column is
     (Name, Size, Income, Happiness);

   type Pop_Group_Record is
      record
         Pop_Group       : Concorde.Db.Pop_Group_Reference;
         Size_Node       : Concorde.Db.Network_Value_Reference;
         Income_Node     : Concorde.Db.Network_Value_Reference;
         Happiness_Node  : Concorde.Db.Network_Value_Reference;
      end record;

   Column_Type_Array : constant array (Pop_Group_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (others => Nazar.Values.Text_Value_Type);

   package Pop_Group_Vectors is
     new Ada.Containers.Vectors
       (Positive, Pop_Group_Record);

   type Pop_Group_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Colony : Concorde.Handles.Colony.Colony_Handle;
         State  : Pop_Group_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Pop_Group_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Pop_Group_Model_Record)
      return Natural
   is (Pop_Group_Table_Column'Pos (Pop_Group_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model        : Pop_Group_Model_Record;
      Column_Index : Positive)
      return String
   is (Pop_Group_Table_Column'Image
       (Pop_Group_Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model        : Pop_Group_Model_Record;
      Column_Index : Positive)
      return String
   is (Pop_Group_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model        : Pop_Group_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Pop_Group_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model       : Pop_Group_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Pop_Group_Model_Access is access all Pop_Group_Model_Record'Class;

   procedure Load
     (Model : in out Pop_Group_Model_Record'Class);

   type Pop_Group_Model_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Pop_Group_Model_Access;
      end record;

   overriding procedure Activate
     (Update : Pop_Group_Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Pop_Group_Model_Update) is
   begin
      Update.Model.Load;
      Update.Model.Notify_Observers;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1), Update);
   end Activate;

   -------------------------
   -- Colony_Policy_Model --
   -------------------------

   function Colony_Policy_Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
   begin
      return Policies.Model (Colony);
   end Colony_Policy_Model;

   ----------------------------
   -- Colony_Pop_Group_Model --
   ----------------------------

   function Colony_Pop_Group_Model
     (Colony : Concorde.Handles.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Model : constant Pop_Group_Model_Access := new Pop_Group_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Colony => Colony, State => <>);
   begin
      Model.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Pop_Group_Model_Update'(Model => Model));
      return Nazar.Models.Table.Nazar_Table_Model (Model);
   end Colony_Pop_Group_Model;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Model       : Pop_Group_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Pop_Group_Record renames Model.State (Row);
      Value : constant String :=
                (case Pop_Group_Table_Column'Val (Column - 1) is
                    when Name =>
                      Concorde.Db.Pop_Group.Get (Info.Pop_Group).Tag,
                    when Size =>
                      Natural'Image
                   (Natural
                      (Concorde.Db.Network_Value.Get
                         (Info.Size_Node).Real_Value)),

--                        Concorde.Quantities.Show
--                          (Concorde.Quantities.To_Quantity
--                          (Concorde.Db.Network_Value.Get
--                               (Info.Size_Node).Real_Value)),
                    when Income =>
                      Concorde.Real_Images.Approximate_Image
                        (Concorde.Db.Network_Value.Get
                        (Info.Income_Node).Real_Value * 100.0),
                    when Happiness =>
                       Concorde.Real_Images.Approximate_Image
                   (Concorde.Db.Network_Value.Get
                        (Info.Happiness_Node).Real_Value * 100.0));
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Pop_Group_Model_Record'Class)
   is
      Network : constant Concorde.Db.Network_Reference :=
                  Model.Colony.Network_Handle.Reference;
   begin
      Model.State.Clear;
      for Pop_Group of
        Concorde.Db.Pop_Group.Scan_By_Tag
      loop
         declare
            Tag : constant String := Pop_Group.Tag;

            function Node_Reference
              (Name : String)
               return Concorde.Db.Network_Value_Reference
            is (Concorde.Db.Network_Value.Get_Reference_By_Network_Value
                 (Network,
                  Concorde.Db.Node.Get_Reference_By_Tag (Name)));

            Info : constant Pop_Group_Record :=
                     Pop_Group_Record'
                       (Pop_Group      =>
                          Pop_Group.Get_Pop_Group_Reference,
                        Size_Node      =>
                          Node_Reference (Tag & "-population"),
                        Income_Node    =>
                          Node_Reference (Tag & "-income"),
                        Happiness_Node =>
                          Node_Reference (Tag));
         begin
            Model.State.Append (Info);
         end;
      end loop;
   end Load;

end Concorde.UI.Models.Colonies;
