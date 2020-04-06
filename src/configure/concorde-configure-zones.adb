with Tropos.Reader;

with Concorde.Db.Sector_Use;
with Concorde.Db.Zone;

package body Concorde.Configure.Zones is

   ---------------------
   -- Configure_Zones --
   ---------------------

   procedure Configure_Zones
     (Scenario_Name : String)
   is

      Finished : Boolean := False;

      procedure Configure
        (Config : Tropos.Configuration);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Config : Tropos.Configuration)
      is
         use Concorde.Db;
         Tag : constant String := Config.Config_Name;
         Parent_Name : constant String := Config.Get ("parent", "");
         Parent_Use  : constant Sector_Use_Reference :=
           Sector_Use.Get_Reference_By_Tag (Parent_Name);
      begin
         if Zone.Get_Reference_By_Tag (Tag) /= Null_Zone_Reference then
            null;
         elsif Parent_Name = ""
           or else Parent_Use /= Null_Sector_Use_Reference
         then
            declare
               Sector_Use : constant Concorde.Db.Sector_Use_Reference :=
                 Concorde.Db.Sector_Use.Create
                   (Tag             => Tag,
                    Parent          => Parent_Use);
            begin
               for Zone_Config of Config.Child ("zones") loop
                  Concorde.Db.Zone.Create
                    (Tag             => Zone_Config.Config_Name,
                     Content         => Concorde.Db.Quantity,
                     Sector_Use      => Sector_Use);
               end loop;
            end;
         else
            Finished := False;
         end if;
      end Configure;

   begin
      while not Finished loop
         Finished := True;

         Tropos.Reader.Read_Config
           (Path      => Scenario_Directory (Scenario_Name, "zones"),
            Extension => "zone",
            Configure => Configure'Access);
      end loop;

   end Configure_Zones;

end Concorde.Configure.Zones;
