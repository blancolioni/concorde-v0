with Tropos.Reader;

with Concorde.Color;

with Concorde.Db.Resource;
with Concorde.Db.Terrain;
with Concorde.Db.Terrain_Resource;

package body Concorde.Configure.Terrain is

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "terrain"),
         Extension => "terrain",
         Configure => Configure_Terrain'Access);
   end Configure_Terrain;

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is
      Color : constant Concorde.Color.Concorde_Color :=
                Concorde.Color.From_String
                  (Config.Get ("color", "#000"));
      Terrain : constant Concorde.Db.Terrain_Reference :=
                  Concorde.Db.Terrain.Create
                    (Tag      => Config.Config_Name,
                     Red      => Color.Red,
                     Green    => Color.Green,
                     Blue     => Color.Blue,
                     Hazard   => Get_Real (Config, "hazard") / 100.0,
                     Is_Water => Config.Get ("is_water"));
   begin
      for Resource_Config of Config.Child ("resource") loop
         declare
            use Concorde.Db;
            Resource : constant Resource_Reference :=
              Concorde.Db.Resource.Get_Reference_By_Tag
                (Resource_Config.Config_Name);
            Chance   : constant Natural :=
              Resource_Config.Value;
         begin
            if Resource = Null_Resource_Reference then
               raise Constraint_Error with
                 "in terrain " & Config.Config_Name
                 & ": unknown resource: " & Resource_Config.Config_Name;
            end if;

            Concorde.Db.Terrain_Resource.Create
              (Terrain  => Terrain,
               Resource => Resource,
               Chance   => Real (Chance) / 100.0);
         end;
      end loop;
   end Configure_Terrain;

end Concorde.Configure.Terrain;
