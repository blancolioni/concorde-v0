with Ada.Text_IO;

with WL.Processes;

with Concorde.Options;

with Concorde.UI.Launch;

with Concorde.Calendar;

with Concorde.Logging;
with Concorde.Logs;

with Concorde.Managers.Loader;
with Concorde.Managers.Execution;

with Concorde.Server;
with Concorde.Updates.Control;

with Concorde.Db.Database;

with Nazar.Main;

procedure Concorde.Driver is

   Database_Open   : Boolean := False;
   Updates_Running : Boolean := False;

begin

   Concorde.Server.Initialize;

   if Concorde.Options.Create then
      Concorde.Logging.Start_Logging ("create");
      Concorde.Server.Create_Scenario;
      Concorde.Logging.Stop_Logging;
      return;
   end if;

   Concorde.Managers.Loader.Register_Managers;

   Concorde.Logging.Start_Logging ("");

   Ada.Text_IO.Put_Line ("opening database ...");

   Concorde.Db.Database.Open;
   Database_Open := True;

   Concorde.Managers.Execution.Load_Managers;

   Ada.Text_IO.Put_Line ("starting server ...");

   Concorde.Server.Start;

   Concorde.Calendar.Load_Clock;

   Ada.Text_IO.Put_Line
     ("Start date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Concorde.Updates.Control.Start_Updates;
   Updates_Running := True;

   if Concorde.Options.Batch_Mode then
      declare
         Process     : WL.Processes.Process_Type;
         Update_Days : constant Natural :=
           Concorde.Options.Update_Count;
      begin
         if Update_Days > 0 then
            Process.Start_Bar ("Updating", Update_Days * 24, True);

            for Day_Index in 1 .. Update_Days loop
               for Hour_Index in 1 .. 24 loop
                  for Minute_Index in 1 .. 60 loop
                     Concorde.Calendar.Advance (60.0);
                     Concorde.Updates.Control.Execute_Pending_Updates;
                  end loop;
                  Process.Tick;
               end loop;
               Process.Tick;
            end loop;
            Process.Finish;
         end if;

      end;

   else

      Nazar.Main.Init;

      declare
         UI : constant Concorde.UI.UI_Type :=
           Concorde.UI.Launch.Get_UI (Concorde.Options.User_Interface);
      begin
         UI.Start;

         Ada.Text_IO.Put_Line ("Stopping ...");

      end;

      Nazar.Main.Stop;

   end if;

   Updates_Running := False;
   Concorde.Updates.Control.Stop_Updates;

   Ada.Text_IO.Put_Line
     ("Stop date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Ada.Text_IO.Put_Line ("Closing database");
   Concorde.Db.Database.Close;
   Database_Open := False;

   Concorde.Logs.Flush_Logs (True);

   Ada.Text_IO.Put_Line ("exit");

   if Concorde.Options.Detailed_Logging then
      Concorde.Logging.Stop_Logging;
   end if;

exception

   when others =>
      if Updates_Running then
         Concorde.Updates.Control.Stop_Updates;
      end if;
      if Database_Open then
         Concorde.Db.Database.Close;
      end if;
      Concorde.Logs.Flush_Logs (True);
      Concorde.Logging.Stop_Logging;
      raise;

end Concorde.Driver;
