with Ada.Calendar.Formatting;

with Marlowe.Version;
with Kit.Version;

with Concorde.Version;

with Concorde.Calendar;
with Concorde.Contexts;
with Concorde.Updates.Control;

with Concorde.UI;

with Marlowe.Database;
with Concorde.Db.Marlowe_Keys;

package body Concorde.Commands.System is

   type Echo_Command is
     new Root_Concorde_Command with null record;

   overriding procedure Perform
     (Command   : Echo_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   type History_Command is
     new Root_Concorde_Command with null record;

   overriding procedure Perform
     (Command   : History_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   type Status_Command_Type is
     (Pause_Server, Resume_Server, Stop_Server,
      Update_Speed,
      Show_Status, Show_Database_Statistics);

   type Status_Command (Command : Status_Command_Type) is
     new Root_Concorde_Command with null record;

   overriding function Administrator_Only
     (Command : Status_Command)
      return Boolean
   is (True);

   overriding procedure Perform
     (Command   : Status_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
--        Cat                   : Cat_Command;
--        Change_Scope          : Change_Scope_Command;
      Echo                  : Echo_Command;
      History               : History_Command;
--        List                  : List_Command;
      Pause_Command         : Status_Command (Pause_Server);
      Resume_Command        : Status_Command (Resume_Server);
      Stop_Command          : Status_Command (Stop_Server);
      Get_Status_Command    : Status_Command (Show_Status);
      Get_Db_Status_Command : Status_Command (Show_Database_Statistics);
      Update_Speed_Command  : Status_Command (Update_Speed);
   begin
--        Register ("cat", Cat);
--        Register ("cd", Change_Scope);
--        Register ("change-scope", Change_Scope);
      Register ("echo", Echo);
      Register ("history", History);
--        Register ("ls", List);
      Register ("pause", Pause_Command);
      Register ("resume", Resume_Command);
      Register ("update-speed", Update_Speed_Command);
      Register ("stop-server", Stop_Command);
      Register ("status", Get_Status_Command);
      Register ("db-status", Get_Db_Status_Command);
   end Load_System_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Echo_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Session);
   begin
      for I in 1 .. Argument_Count (Arguments) loop
         if I > 1 then
            Writer.Put (" ");
         end if;
         Writer.Put (Argument (Arguments, I));
      end loop;

      if not Contains (Arguments, "n") then
         Writer.New_Line;
      end if;

   end Perform;

   overriding procedure Perform
     (Command   : History_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Arguments);
   begin
      for I in 1 .. Session.History_Length loop
         declare
            Index_Image : String (1 .. 5);
            It          : Natural := I;
         begin
            for Ch of reverse Index_Image loop
               if It = 0 then
                  Ch := ' ';
               else
                  Ch := Character'Val (It mod 10 + 48);
                  It := It / 10;
               end if;
            end loop;
            Writer.Put_Line (Index_Image & "  " & Session.History (I));
         end;
      end loop;
   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Status_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
   begin
      case Command.Command is
         when Pause_Server =>
            Concorde.Updates.Control.Pause_Updates;
         when Resume_Server =>
            Concorde.Updates.Control.Resume_Updates;
         when Stop_Server =>
            Concorde.UI.Current_UI.Stop
              (Argument (Arguments, "message", "stop server command"));
            Writer.Put_Line
              (Session.User_Name
               & ": server stopped via stop-server command");
         when Update_Speed =>

            if Argument_Count (Arguments) > 1 then
               Writer.Put_Error ("Usage: update-speed [time factor]");
               return;
            end if;

            if Argument_Count (Arguments) = 1 then
               declare
                  Value : Duration;
               begin
                  Value := Duration'Value (Argument (Arguments, 1));
                  Concorde.Updates.Control.Set_Advance_Speed (Value);
               exception
                  when Constraint_Error =>
                     Writer.Put_Error ("Usage: update-speed [time factor]");
                     return;
               end;
            end if;

            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Concorde.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

         when Show_Status =>
            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Writer.Put_Line ("logged in as " & Session.User_Name);
               Concorde.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 (Concorde.Version.Name
                  & " version "
                  & Concorde.Version.Version_String);
               Writer.Put_Line
                 ("kit     "
                  & Kit.Version.Version_String);
               Writer.Put_Line
                 ("marlowe "
                  & Marlowe.Version.Version_String);
               Writer.Put_Line
                 ("Server started "
                  & Ada.Calendar.Formatting.Image
                    (Start_Time));
               Writer.Put_Line
                 ("status: " & (if Paused then "paused" else "running"));
               Writer.Put_Line
                 ("current server date: "
                  & Concorde.Calendar.Image
                    (Concorde.Calendar.Clock));
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

         when Show_Database_Statistics =>
            declare
               use Concorde.Db.Marlowe_Keys;
               Info : constant Marlowe.Database.Database_Information :=
                        Handle.Get_Data_Store_Information;
            begin
               Writer.Put_Line
                 ("new blocks:   " & Info.Blocks'Image);
               Writer.Put_Line
                 ("cached pages: " & Info.Pages'Image);
               Writer.Put_Line
                 ("total records:" & Info.Record_Count'Image);
               Writer.Put_Line
                 ("cache hits:   " & Info.Hits'Image);
               Writer.Put_Line
                 ("cache misses: " & Info.Misses'Image);
               Writer.Put_Line
                 ("file reads:   " & Info.Reads'Image);
               Writer.Put_Line
                 ("file writes:  " & Info.Writes'Image);
            end;
      end case;
   end Perform;

end Concorde.Commands.System;
