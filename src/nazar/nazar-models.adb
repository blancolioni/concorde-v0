package body Nazar.Models is

   type Null_Model_Record is
     new Root_Model_Type with null record;

   ------------------
   -- Add_Observer --
   ------------------

   procedure Add_Observer
     (Model    : in out Root_Model_Type'Class;
      Observer :        Model_Observer_Interface'Class)
   is
   begin
      Model.Observers.Append (Observer);
   end Add_Observer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model : in out Root_Model_Type)
   is
   begin
      Model.Id := WL.Guids.New_Guid;
      Model.Declare_Property ("identifier", "");
   end Initialize;

   ----------------------
   -- Notify_Observers --
   ----------------------

   procedure Notify_Observers
     (Model : Root_Model_Type'Class)
   is
   begin
      for Observer of Model.Observers loop
         Observer.Notify;
      end loop;
   end Notify_Observers;

   ----------------
   -- Null_Model --
   ----------------

   function Null_Model return Root_Model_Type'Class is
   begin
      return Null_Model_Record'
        (Nazar.Interfaces.Properties.Root_Property_Container with
           Id        => WL.Guids.Null_Guid,
         Observers => <>);
   end Null_Model;

   ---------------------
   -- Remove_Observer --
   ---------------------

   procedure Remove_Observer
     (Model    : in out Root_Model_Type'Class;
      Observer :        Model_Observer_Interface'Class)
   is
      Position : Observer_Lists.Cursor := Model.Observers.Find (Observer);
   begin
      if Observer_Lists.Has_Element (Position) then
         Model.Observers.Delete (Position);
      else
         raise Constraint_Error with
           "observer not found in model";
      end if;
   end Remove_Observer;

end Nazar.Models;
