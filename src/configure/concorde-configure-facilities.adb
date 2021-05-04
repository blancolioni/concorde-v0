with Tropos.Reader;

with Concorde.Db;

with Concorde.Handles.Commodity;
with Concorde.Handles.Facility;
with Concorde.Handles.Facility_Production;
with Concorde.Handles.Facility_Service;
with Concorde.Handles.Facility_Worker;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Service_Commodity;

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
      Facility : constant Concorde.Handles.Facility.Facility_Handle :=
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
                          (Facility_Config.Get ("capacity", 0.0)),
                      Default_Manager =>
                        Facility_Config.Get
                          ("default-manager", "default-installation"));

   begin

      for Production_Config of Facility_Config.Child ("production") loop
         declare
            Commodity : constant Concorde.Handles.Commodity.Commodity_Class :=
                          Concorde.Handles.Commodity.Get_By_Tag
                            (Production_Config.Config_Name);
         begin
            if not Commodity.Has_Element then
               raise Constraint_Error with
                 "in configuration for " & Facility.Tag
                 & ": no such commodity: " & Production_Config.Config_Name;
            end if;

            Concorde.Handles.Facility_Production.Create
              (Facility  => Facility,
               Commodity => Commodity);
         end;
      end loop;

      for Service_Config of Facility_Config.Child ("service") loop
         declare
            Class : constant String := Service_Config.Config_Name;
            Quality : constant Positive :=
                        Positive'Value (Service_Config.Get ("quality"));
            Commodity : constant Concorde.Handles.Service_Commodity
              .Service_Commodity_Class :=
                Concorde.Handles.Service_Commodity.First_By_Quality_Class
                  (Concorde.Db.Quality_Type'Val (Quality - 1),
                   Concorde.Db.Service_Category'Value (Class));
         begin
            if not Commodity.Has_Element then
               raise Constraint_Error with
                 "in configuration for " & Facility.Tag
                 & ": no such service commodity: "
                 & Service_Config.Config_Name;
            end if;

            Concorde.Handles.Facility_Service.Create
              (Facility  => Facility,
               Service_Commodity => Commodity);

         end;
      end loop;

      for Worker_Config of
        Facility_Config.Child ("worker")
      loop
         declare
            Pop_Group : constant Concorde.Handles.Pop_Group.Pop_Group_Handle :=
                          Concorde.Handles.Pop_Group.Get_By_Tag
                            (Worker_Config.Config_Name);
         begin
            if not Pop_Group.Has_Element then
               raise Constraint_Error with
                 "in configuration for " & Facility.Tag
                 & ": no such pop group: " & Worker_Config.Config_Name;
            end if;

            Concorde.Handles.Facility_Worker.Create
              (Facility  => Facility,
               Pop_Group => Pop_Group,
               Quantity  =>
                 Concorde.Quantities.To_Quantity (Worker_Config.Value));
         end;
      end loop;
   end Configure_Facility;

end Concorde.Configure.Facilities;
