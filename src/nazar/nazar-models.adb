package body Nazar.Models is

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
