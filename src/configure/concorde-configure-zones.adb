with Tropos.Reader;

with Concorde.Configure.Commodities;

with Concorde.Db.Commodity_Group;
with Concorde.Db.Sector_Use;
with Concorde.Db.Zone;

package body Concorde.Configure.Zones is

   Zone_Class : Concorde.Db.Commodity_Group_Reference :=
     Concorde.Db.Null_Commodity_Group_Reference;

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
                     Index           =>
                       Concorde.Configure.Commodities.Next_Commodity_Index,
                     Commodity_Group => Zone_Class,
                     Sector_Use      => Sector_Use,
                     Initial_Price   =>
                       Concorde.Money.To_Price
                         (Real (Float'(Zone_Config.Get ("base-price")))),
                     Mass            => 1.0,
                     Density         => 1.0);
               end loop;
            end;
         else
            Finished := False;
         end if;
      end Configure;

   begin
      Zone_Class :=
        Concorde.Db.Commodity_Group.Create ("zone");

      while not Finished loop
         Finished := True;

         Tropos.Reader.Read_Config
           (Path      => Scenario_Directory (Scenario_Name, "zones"),
            Extension => "zone",
            Configure => Configure'Access);
      end loop;

   end Configure_Zones;

end Concorde.Configure.Zones;
