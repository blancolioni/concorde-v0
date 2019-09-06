with Concorde.Calendar;

package Concorde.Updates.Events is

   procedure Update_At
     (Clock  : Concorde.Calendar.Time;
      Update : Update_Interface'Class);

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class);

end Concorde.Updates.Events;
