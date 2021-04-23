with Concorde.Calendar;

package Concorde.Updates.Events is

   procedure Update_At
     (Clock  : Concorde.Calendar.Time;
      Update : Root_Update_Type'Class);

   procedure Update_With_Delay
     (Wait   : Concorde_Duration;
      Update : Root_Update_Type'Class);

end Concorde.Updates.Events;
