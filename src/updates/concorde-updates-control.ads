with Ada.Calendar;

package Concorde.Updates.Control is

   procedure Start_Updates;
   procedure Stop_Updates;

   procedure Pause_Updates;
   procedure Resume_Updates;
   procedure Set_Advance_Speed
     (Advance_Per_Second : Duration);

   procedure Get_Status
     (Start_Time         : out Ada.Calendar.Time;
      Paused             : out Boolean;
      Advance_Per_Second : out Duration);

   function Is_Paused return Boolean;
   function Is_Active return Boolean;

private

   function Is_Active return Boolean
   is (not Is_Paused);

end Concorde.Updates.Control;
