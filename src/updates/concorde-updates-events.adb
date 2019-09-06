with Concorde.Updates.Tasks;

package body Concorde.Updates.Events is

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Concorde.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Concorde.Updates.Tasks.Update_Map.Add_Update (Clock, Update);
   end Update_At;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class)
   is
      use type Concorde.Calendar.Time;
   begin
      Concorde.Updates.Tasks.Update_Map.Add_Update
        (Concorde.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Concorde.Updates.Events;
