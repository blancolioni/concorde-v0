with Tropos.Reader;

with Concorde.Identifiers;

with Concorde.Handles.Economic_Sector;
with Concorde.Handles.Zone;

with Concorde.Db;

package body Concorde.Configure.Economy is

   --------------------------------
   -- Configure_Economic_Sectors --
   --------------------------------

   procedure Configure_Economic_Sectors (Scenario_Name : String) is
      Sector_Config : constant Tropos.Configuration :=
                        Tropos.Reader.Read_Config
                          (Scenario_File
                             (Scenario_Name  => Scenario_Name,
                              Directory_Name => "economy",
                              File_Name      => "economic-sectors.config"));
   begin
      for Config of Sector_Config loop
         Concorde.Handles.Economic_Sector.Create
           (Content    => Concorde.Db.Quantity,
            Identifier => Concorde.Identifiers.Next_Identifier,
            Tag        => Config.Config_Name,
            Zone       =>
              Concorde.Handles.Zone.Get_By_Tag
                (Config.Get ("zone")));
      end loop;
   end Configure_Economic_Sectors;

end Concorde.Configure.Economy;
