with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;
with WL.Random.Names;

with Tropos.Reader;
with Tropos.Writer;

with Concorde.Configure.Scenarios;

with Concorde.Options;
with Concorde.Paths;
with Concorde.Random;

with Concorde.Color;
with Concorde.Factions.Create;

with Concorde.Db.Database;

with Concorde.Db.Faction;
with Concorde.Db.User;

package body Concorde.Server is

   Name_Generator    : WL.Random.Names.Name_Generator;
   Server_Start_Time : Ada.Calendar.Time;

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean;

   ------------------
   -- Add_Factions --
   ------------------

   procedure Add_Factions is
      Database_Open : Boolean := False;
   begin
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
            Name_Config    : constant Tropos.Configuration :=
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
            Position       : Tropos.Cursor := Color_Config.First;
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
            Color       : constant Concorde.Color.Concorde_Color :=
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

   exception

      when others =>
         if Database_Open then
            Concorde.Db.Database.Close;
         end if;
         raise;

   end Add_Factions;

   ---------------------
   -- Create_Scenario --
   ---------------------

   procedure Create_Scenario is
      Database_Open : Boolean := False;
   begin
      Concorde.Db.Database.Create;
      Database_Open := True;

      Concorde.Configure.Initialize_Database;

      Concorde.Configure.Scenarios.Load_Scenario
        (Scenario_Name  => Concorde.Options.Scenario,
         Name_Generator => Name_Generator);

      Concorde.Db.Database.Close;
      Database_Open := False;

   exception

      when others =>
         if Database_Open then
            Concorde.Db.Database.Close;
         end if;
         raise;

   end Create_Scenario;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Server_Start_Time := Ada.Calendar.Clock;

      if not Ada.Directories.Exists (".concorde-options") then
         Ada.Directories.Copy_File
           (Source_Name => Concorde.Paths.Config_File ("default-options.txt"),
            Target_Name => ".concorde-options");
      end if;

      WL.Command_Line.Load_Defaults (".concorde-options");

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

   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      null;
   end Start;

   ----------------
   -- Start_Time --
   ----------------

   function Start_Time return Ada.Calendar.Time is
   begin
      return Server_Start_Time;
   end Start_Time;

end Concorde.Server;
