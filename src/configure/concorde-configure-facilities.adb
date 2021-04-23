with Tropos.Reader;

with Concorde.Db;

with Concorde.Handles.Facility;

package body Concorde.Configure.Facilities is

   procedure Configure_Facility
     (Facility_Config : Tropos.Configuration);

   --------------------------
   -- Configure_Facilities --
   --------------------------

   procedure Configure_Facilities
     (Scenario_Name : String)
   is
   begin
      for Facility_Config of
        Tropos.Reader.Read_Config
          (Path      =>
             Scenario_Directory (Scenario_Name, "facilities"),
           Extension => "facility")
      loop
         Configure_Facility (Facility_Config);
      end loop;
   end Configure_Facilities;

   ------------------------
   -- Configure_Facility --
   ------------------------

   procedure Configure_Facility
     (Facility_Config : Tropos.Configuration)
   is
   begin
      Concorde.Handles.Facility.Create
        (Tag      => Facility_Config.Config_Name,
         Category =>
           Concorde.Db.Facility_Category'Value
             (Facility_Config.Get ("class")),
         Power    =>
           Concorde.Quantities.To_Quantity
             (Facility_Config.Get ("power", 0.0)),
         Capacity =>
           Concorde.Quantities.To_Quantity
             (Facility_Config.Get ("capacity", 0.0)));
   end Configure_Facility;

end Concorde.Configure.Facilities;
