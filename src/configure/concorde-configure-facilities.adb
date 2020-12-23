with Tropos.Reader;

with Concorde.Facilities;

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
        (Tag            => Facility_Config.Config_Name,
         Facility_Class =>
           Concorde.Facilities.Facility_Class
             (Facility_Config.Get ("class")));
   end Configure_Facility;

end Concorde.Configure.Facilities;
