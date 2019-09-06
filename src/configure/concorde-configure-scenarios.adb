with Tropos.Properties_Reader;
with Tropos.Writer;

with Concorde.Options;

with Concorde.Configure.Climates;
with Concorde.Configure.Commodities;
with Concorde.Configure.Facilities;
with Concorde.Configure.Galaxies;
with Concorde.Configure.Production;
with Concorde.Configure.Terrain;
with Concorde.Configure.Utility;
with Concorde.Configure.Zones;

with Concorde.Db.Scenario;

package body Concorde.Configure.Scenarios is

   procedure Convert_Starcorp_Configuration
     (Scenario_Name  : String;
      Directory_Path : String);

   ------------------------------------
   -- Convert_Starcorp_Configuration --
   ------------------------------------

   procedure Convert_Starcorp_Configuration
     (Scenario_Name  : String;
      Directory_Path : String)
   is
      Item_Config      : constant Tropos.Configuration :=
                           Tropos.Properties_Reader.Read_Properties
                             (Directory_Path & "/items.properties");
      Pop_Config       : constant Tropos.Configuration :=
                           Tropos.Properties_Reader.Read_Properties
                             (Directory_Path & "/population.properties");
      Facility_Config  : constant Tropos.Configuration :=
                           Tropos.Properties_Reader.Read_Properties
                             (Directory_Path & "/facilities.properties");
      Terrain_Config   : constant Tropos.Configuration :=
                           Tropos.Properties_Reader.Read_Properties
                             (Directory_Path & "/terrain.properties");

      procedure Write
        (Top       : Tropos.Configuration;
         To        : String;
         Extension : String);

      -----------
      -- Write --
      -----------

      procedure Write
        (Top       : Tropos.Configuration;
         To        : String;
         Extension : String)
      is
      begin
         for Config of Top loop
            Tropos.Writer.Write_Config
              (Config,
               Scenario_Directory (Scenario_Name, To)
               & "/" & Config.Config_Name & "." & Extension);
         end loop;
      end Write;

   begin
      Write (Item_Config, "commodities", "commodity");
      Write (Pop_Config, "poptypes", "poptype");
      Write (Facility_Config, "facilities", "facility");
      Write (Terrain_Config, "terrain", "terrain");
   end Convert_Starcorp_Configuration;

   -------------------
   -- Load_Scenario --
   -------------------

   procedure Load_Scenario
     (Scenario_Name  : String;
      Name_Generator : WL.Random.Names.Name_Generator)
   is
      R  : constant Natural := Concorde.Options.Galaxy_Radius;
      RX : constant Natural := Concorde.Options.Galaxy_Radius_X;
      RY : constant Natural := Concorde.Options.Galaxy_Radius_Y;
      RZ : constant Natural := Concorde.Options.Galaxy_Radius_Z;
   begin
      Concorde.Db.Scenario.Create
        (Scenario_Name, True, Concorde.Db.Null_Star_System_Reference);

      if False then
         if Scenario_Directory (Scenario_Name, "starcorp") /= "" then
            Convert_Starcorp_Configuration
              (Scenario_Name,
               Scenario_Directory (Scenario_Name, "starcorp"));
         end if;
      end if;

      Concorde.Configure.Commodities.Configure_Commodities
        (Scenario_Name);

      Concorde.Configure.Utility.Configure_Utility (Scenario_Name);

      Concorde.Configure.Terrain.Configure_Terrain (Scenario_Name);
      Concorde.Configure.Zones.Configure_Zones (Scenario_Name);
      Concorde.Configure.Climates.Configure_Climates (Scenario_Name);

      Concorde.Configure.Production.Configure_Production (Scenario_Name);
      Concorde.Configure.Facilities.Configure_Facilities (Scenario_Name);

      Concorde.Configure.Galaxies.Generate_Galaxy
        (Number_Of_Systems  => Concorde.Options.System_Count,
         Radius_X           => Real (if RX = 0 then R else RX),
         Radius_Y           =>
           Real (if RY = 0 then (if RX = 0 then R else RX) else RY),
         Radius_Z           =>
           Real (if RZ = 0 then (if RX = 0 then R else RX) else RZ),
         Create_Coordinates =>
           Concorde.Configure.Galaxies.Random_Sphere_Distribution'Access,
         Names              => Name_Generator);

   end Load_Scenario;

end Concorde.Configure.Scenarios;
