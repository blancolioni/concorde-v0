with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;
with WL.Processes;
with WL.Random.Names;

with Tropos.Reader;
with Tropos.Writer;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Color;
with Concorde.Random;

with Concorde.UI.Web_UI;

with Concorde.Calendar;
with Concorde.Configure;
with Concorde.Configure.Scenarios;
with Concorde.Factions.Create;
with Concorde.Logging;
with Concorde.Logs;

with Concorde.Managers.Loader;
with Concorde.Managers.Execution;

with Concorde.Updates.Control;

with Concorde.Db.Database;

with Concorde.Db.Faction;
with Concorde.Db.User;

procedure Concorde.Driver is
   Name_Generator : WL.Random.Names.Name_Generator;

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean;

   ----------------------------
   -- Have_Required_Argument --
   ----------------------------

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean
   is
   begin
      if Argument_Value = "" then
         Ada.Text_IO.Put_Line
           ("missing required argument: --" & Argument_Name);
         return False;
      else
         return True;
      end if;
   end Have_Required_Argument;

   Database_Open   : Boolean := False;
   Updates_Running : Boolean := False;

begin

   if not Ada.Directories.Exists ("options.txt") then
      Ada.Directories.Copy_File
        (Source_Name => Concorde.Paths.Config_File ("default-options.txt"),
         Target_Name => "options.txt");
   end if;

   WL.Command_Line.Load_Defaults ("options.txt");

   WL.Localisation.Read_Localisation
     (Concorde.Paths.Config_File
        ("localisation/" & Concorde.Options.Language & ".txt"));

   WL.Random.Names.Load_Lexicon
     (Name_Generator,
      Concorde.Paths.Config_File ("totro-vowels.txt"),
      Concorde.Paths.Config_File ("totro-consonants.txt"));

   if Concorde.Options.Randomise then
      WL.Random.Randomise;
   end if;

   if Concorde.Options.Create then
      Concorde.Db.Database.Create;
      Database_Open := True;

      Concorde.Configure.Initialize_Database;

      Concorde.Configure.Scenarios.Load_Scenario
        (Scenario_Name  => Concorde.Options.Scenario,
         Name_Generator => Name_Generator);

      Concorde.Db.Database.Close;
      Database_Open := False;
      return;
   end if;

   if Concorde.Options.Add_Faction then

      if Concorde.Options.Faction_Names_File /= "" then
         if not Have_Required_Argument
           ("faction-colors-file", Concorde.Options.Faction_Colors_File)
         then
            return;
         end if;

         if Concorde.Options.Randomise then
            Concorde.Random.Reset;
            WL.Random.Randomise;
         else
            Concorde.Random.Reset (Concorde.Options.Random_Seed);
            WL.Random.Reset (Concorde.Options.Random_Seed);
         end if;

         Concorde.Db.Database.Open;
         Database_Open := True;

         declare
            Name_Config : constant Tropos.Configuration :=
                               Tropos.Reader.Read_Config
                                 (Concorde.Paths.Config_File
                                    (Concorde.Options.Faction_Names_File));
            Color_Config   : constant Tropos.Configuration :=
                               Tropos.Reader.Read_Config
                                 (Concorde.Paths.Config_File
                                    (Concorde.Options.Faction_Colors_File));
            Scenario_Name  : constant String :=
                               (if Concorde.Options.Scenario = ""
                                then "default"
                                else Concorde.Options.Scenario);
            Faction_Setup  : constant String :=
                               Concorde.Options.Faction_Setup_Path;
            Setup_Path     : constant String :=
                               Concorde.Configure.Scenario_File
                                 (Scenario_Name  => Scenario_Name,
                                  Directory_Name => "factions",
                                  File_Name      =>
                                    (if Faction_Setup = ""
                                     then "default"
                                     else Faction_Setup)
                                  & ".faction");
            Faction_Config : Tropos.Configuration :=
                               Tropos.New_Config ("factions");
            Position : Tropos.Cursor := Color_Config.First;
         begin

            for Config of Name_Config loop
               declare
                  New_Config : Tropos.Configuration := Config;
               begin
                  New_Config.Add
                    ("color", Tropos.Element (Position).Config_Name);
                  Faction_Config.Add (New_Config);
                  Tropos.Next (Position);
               end;
            end loop;
            Tropos.Writer.Write_Config (Faction_Config, "factions.config");

            Concorde.Factions.Create.Create_Factions
              (Faction_Config => Faction_Config,
               Setup_Config   => Tropos.Reader.Read_Config (Setup_Path));
         end;

         Concorde.Db.Database.Close;
         Database_Open := False;

         return;
      end if;

      if not Have_Required_Argument
        ("account-name", Concorde.Options.Account_Name)
        or else not Have_Required_Argument
          ("faction-name", Concorde.Options.Faction_Name)
      then
         return;
      end if;

      Concorde.Db.Database.Open;
      Database_Open := True;

      declare
         use Concorde.Db;
         User           : constant Concorde.Db.User_Reference :=
                            Concorde.Db.User.Get_Reference_By_Login
                              (Login => Concorde.Options.Account_Name);
         Scenario_Name  : constant String :=
                            (if Concorde.Options.Scenario = ""
                             then "default"
                             else Concorde.Options.Scenario);
         Faction_Name   : constant String :=
                            Concorde.Options.Faction_Name;
         Faction_Adj    : constant String :=
                            Concorde.Options.Faction_Adjective;
         Faction_Plural : constant String :=
                            Concorde.Options.Faction_Plural_Name;
         Faction_Color  : constant String :=
                            Concorde.Options.Faction_Color;
         Faction_Setup  : constant String :=
                            Concorde.Options.Faction_Setup_Path;
         Faction        : constant Concorde.Db.Faction_Reference :=
                            Concorde.Db.Faction.First_Reference_By_Name
                              (Name => Faction_Name);
         Setup_Path     : constant String :=
                              Concorde.Configure.Scenario_File
                                (Scenario_Name  => Scenario_Name,
                                 Directory_Name => "factions",
                                 File_Name      =>
                                   (if Faction_Setup = ""
                                    then "default"
                                    else Faction_Setup)
                                 & ".faction");
      begin
         if User = Null_User_Reference then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Concorde.Options.Account_Name
               & ": unknown user name");
            Concorde.Db.Database.Close;
            return;
         end if;

         if Faction /= Null_Faction_Reference then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Faction_Name
               & ": faction already exists");
            Concorde.Db.Database.Close;
            return;
         end if;

         if not Ada.Directories.Exists (Setup_Path) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Cannot find setup file " & Setup_Path);
            Concorde.Db.Database.Close;
            return;
         end if;

         if Faction_Setup = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Using default faction setup " & Setup_Path);
         end if;

         declare
            Color : constant Concorde.Color.Concorde_Color :=
                      (if Faction_Color /= ""
                       then Concorde.Color.From_String (Faction_Color)
                       else Concorde.Color.Concorde_Color'
                         (Red   => Concorde.Random.Unit_Random,
                          Green => Concorde.Random.Unit_Random,
                          Blue  => Concorde.Random.Unit_Random,
                          Alpha => 1.0));
            New_Faction : constant Faction_Reference :=
                            Concorde.Factions.Create.Create_Faction
                              (User        => User,
                               Name        => Faction_Name,
                               Adjective   => Faction_Adj,
                               Plural_Name => Faction_Plural,
                               Color       => Color,
                               Setup       =>
                                 Tropos.Reader.Read_Config (Setup_Path));
         begin
            if New_Faction = Null_Faction_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to create faction");
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Faction created successfully");
            end if;
         end;

         Concorde.Db.Database.Close;
         Database_Open := False;
         return;
      end;

   end if;

   Concorde.Managers.Loader.Register_Managers;
--     Concorde.Commands.Loader.Load_Commands;

   Concorde.Logging.Start_Logging;

   Ada.Text_IO.Put_Line ("opening database ...");

   Concorde.Db.Database.Open;
   Database_Open := True;

   Concorde.Managers.Execution.Load_Managers;

   Ada.Text_IO.Put_Line ("starting server ...");

   Concorde.Calendar.Load_Clock;
--     Concorde.Markets.Initialize_Markets;

   Ada.Text_IO.Put_Line
     ("Start date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Concorde.Updates.Control.Start_Updates;
   Updates_Running := True;

   if Concorde.Options.Batch_Mode
     or else Concorde.Options.Command_Line
   then
      Ada.Text_IO.Put_Line ("C O N C O R D E");

      if Concorde.Options.Batch_Mode then
         declare
            Process     : WL.Processes.Process_Type;
            Update_Days : constant Natural :=
              Concorde.Options.Update_Count;
         begin
            Process.Start_Bar ("Updating", Update_Days * 24, True);

            for Day_Index in 1 .. Update_Days loop
               for Hour_Index in 1 .. 24 loop
                  for Minute_Index in 1 .. 60 loop
                     Concorde.Calendar.Advance (60.0);
                     Concorde.Updates.Control.Execute_Pending_Updates;
                  end loop;
                  Process.Tick;
               end loop;
            end loop;
         end;

         Ada.Text_IO.New_Line;
      else
         null;
      end if;

--        declare
--           User    : constant Concorde.Db.User_Reference :=
--                       Concorde.Db.User.Get_Reference_By_Login ("root");
--           Session : Concorde.Sessions.Concorde_Session :=
--                       Concorde.Sessions.New_Repl_Session (User);
--        begin
--           if Ada.Directories.Exists (".concorderc") then
--              Concorde.Repl.Read (Session, ".concorderc");
--           end if;
--           Concorde.Repl.Execute (Session);
--           Concorde.Sessions.End_Session (Session);
--        end;
   else

      declare
         Web : Concorde.UI.Web_UI.Web_UI_Type;
      begin
         Web.Start;
      end;

--        Gnoga.Application.Title ("Concorde");
--
--        Gnoga.Application.HTML_On_Close ("Application disconnected.");
--
--        Gnoga.Application.Multi_Connect.Initialize
--          (Port    => 8080,
--           Boot    => "concorde.html",
--           Verbose => True);
--
--        Gnoga.Application.Multi_Connect.On_Connect_Handler
--      (Event => Concorde.UI.Gnoga_UI.On_Connect_Default'Unrestricted_Access,
--           Path  => "default");
--
--        Gnoga.Application.Multi_Connect.Message_Loop;
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
