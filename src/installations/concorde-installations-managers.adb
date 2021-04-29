with Concorde.Agents;
with Concorde.Managers.Agents;
with Concorde.Markets;
with Concorde.Quantities;
with Concorde.Stock;

with Concorde.Handles.Facility;
with Concorde.Handles.Facility_Worker;
with Concorde.Handles.Installation;

package body Concorde.Installations.Managers is

   type Default_Installation_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Installation : Concorde.Handles.Installation.Installation_Handle;
         Facility     : Concorde.Handles.Facility.Facility_Handle;
      end record;

   type Manager_Access is access all Default_Installation_Manager'Class;

   overriding function Identifier
     (Manager : Default_Installation_Manager)
      return String
   is (Describe (Manager.Installation) & " default manager");

   overriding procedure Set_Requirements
     (Manager : in out Default_Installation_Manager);

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type
   is
      Installation : constant Concorde.Handles.Installation.Installation_Handle
        := Concorde.Handles.Installation.Get_From_Managed (Managed);
      Manager      : constant Manager_Access :=
                       new Default_Installation_Manager'
                         (Concorde.Managers.Agents.Root_Agent_Manager_Type with
                          Installation => Installation,
                          Facility     =>
                            Installation.Facility.To_Facility_Handle);
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using default manager");
      Manager.Initialize_Agent_Manager
        (Agent          => Installation,
         Market         =>
           Concorde.Markets.World_Market
             (Installation.World_Sector.World),
         Planning_Cycle => 10);
      return Concorde.Managers.Manager_Type (Manager);
   end Create_Default_Manager;

   ----------------------
   -- Set_Requirements --
   ----------------------

   overriding procedure Set_Requirements
     (Manager : in out Default_Installation_Manager)
   is
   begin
      for Facility_Worker of
        Concorde.Handles.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            use Concorde.Quantities;
            Require : constant Quantity_Type :=
                        Facility_Worker.Quantity;
            Have    : constant Quantity_Type :=
                        Concorde.Stock.Get_Quantity
                          (Manager.Installation, Facility_Worker.Pop_Group);
            Missing : constant Quantity_Type :=
                        (if Have < Require then Require - Have else Zero);
         begin
            Manager.Add_Requirement
              (Commodity => Facility_Worker.Pop_Group,
               Necessary => Missing,
               Desired   => Missing);
         end;
      end loop;
   end Set_Requirements;

end Concorde.Installations.Managers;
