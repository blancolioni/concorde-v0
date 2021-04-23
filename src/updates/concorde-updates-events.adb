with Reiko.Control;

package body Concorde.Updates.Events is

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Concorde.Calendar.Time;
      Update : Root_Update_Type'Class)
   is
   begin
      if False then
         Reiko.Control.Add_Update
           (Update    => Update,
            Update_At =>
              Reiko.Reiko_Time (Concorde.Calendar.To_Real (Clock)));
      end if;
   end Update_At;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Concorde_Duration;
      Update : Root_Update_Type'Class)
   is
      use type Concorde.Calendar.Time;
   begin
      Update_At (Concorde.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Concorde.Updates.Events;
