package body Concorde.Server is

   Server_Start_Time : Ada.Calendar.Time;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Server_Start_Time := Ada.Calendar.Clock;
   end Start;

   ----------------
   -- Start_Time --
   ----------------

   function Start_Time return Ada.Calendar.Time is
   begin
      return Server_Start_Time;
   end Start_Time;

end Concorde.Server;
