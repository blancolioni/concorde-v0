with Ada.Calendar;

package Concorde.Server is

   procedure Initialize;

   procedure Create_Scenario;

   procedure Add_Factions;

   procedure Start;

   function Start_Time
     return Ada.Calendar.Time;

end Concorde.Server;
