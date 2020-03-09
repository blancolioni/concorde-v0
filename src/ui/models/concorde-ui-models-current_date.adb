with Concorde.Calendar;
with Concorde.Updates.Events;

package body Concorde.UI.Models.Current_Date is

   type Date_Update is
     new Concorde.Updates.Update_Interface with
      record
         Model : Current_Date_Model;
      end record;

   overriding procedure Activate
     (Update : Date_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Date_Update) is
   begin
      Update.Model.Set_Text
        (Concorde.Calendar.Image (Concorde.Calendar.Clock));
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1), Update);
   end Activate;

   ----------------------------
   -- Current_Date_Model_New --
   ----------------------------

   function Current_Date_Model_New return Current_Date_Model is
      Update : Date_Update;
   begin
      return Model : constant Current_Date_Model :=
        new Current_Date_Model_Record
      do
         Model.Set_Text
           (Concorde.Calendar.Image (Concorde.Calendar.Clock));
         Update.Model := Model;
         Concorde.Updates.Events.Update_With_Delay
           (Concorde.Calendar.Days (1), Update);
      end return;
   end Current_Date_Model_New;

end Concorde.UI.Models.Current_Date;
