with Concorde.Agents;
with Concorde.Managers.Agents;
with Concorde.Markets;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Stock;

with Concorde.Handles.Commodity;
with Concorde.Handles.Employment;
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

   overriding procedure Pay_Daily_Costs
     (Manager : in out Default_Installation_Manager);

   type Default_Outpost_Manager is new Default_Installation_Manager with
      record
         null;
      end record;

   type Outpost_Manager_Access is
     access all Default_Outpost_Manager'Class;

   overriding function Identifier
     (Manager : Default_Outpost_Manager)
      return String
   is (Describe (Manager.Installation) & " manager");

   overriding procedure Set_Sale_Stock
     (Manager : in out Default_Outpost_Manager);

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

   ----------------------------
   -- Create_Outpost_Manager --
   ----------------------------

   function Create_Outpost_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : constant Outpost_Manager_Access :=
                       new Default_Outpost_Manager'
                         (Concorde.Managers.Agents.Root_Agent_Manager_Type with
                          Installation => Installation,
                          Facility     =>
                            Installation.Facility.To_Facility_Handle);
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using outpost manager");
      Manager.Initialize_Agent_Manager
        (Agent          => Installation,
         Market         =>
           Concorde.Markets.World_Market
             (Installation.World_Sector.World),
         Planning_Cycle => 10);
      return Concorde.Managers.Manager_Type (Manager);
   end Create_Outpost_Manager;

   ---------------------
   -- Pay_Daily_Costs --
   ---------------------

   overriding procedure Pay_Daily_Costs
     (Manager : in out Default_Installation_Manager)
   is
   begin
      for Employment of
        Concorde.Handles.Employment.Select_By_Employer
          (Manager.Installation)
      loop
         declare
            Total : constant Concorde.Money.Money_Type :=
                      Concorde.Money.Total
                        (Employment.Salary, Employment.Quantity);
         begin
            Manager.Log
              ("pay salary of "
               & Concorde.Money.Show (Employment.Salary)
               & " to "
               & Concorde.Quantities.Show (Employment.Quantity)
               & " "
               & Employment.Pop.Pop_Group.Tag
               & " (total "
               & Concorde.Money.Show (Total)
               & ")");

            Manager.Spend (Total, "salary");
            Concorde.Agents.Add_Cash (Employment.Pop, Total, "salary");
         end;
      end loop;
   end Pay_Daily_Costs;

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
            if Missing > Zero then
               Manager.Add_Requirement
                 (Commodity => Facility_Worker.Pop_Group,
                  Necessary => Missing,
                  Desired   => Missing);
            end if;
         end;
      end loop;
   end Set_Requirements;

   --------------------
   -- Set_Sale_Stock --
   --------------------

   overriding procedure Set_Sale_Stock
     (Manager : in out Default_Outpost_Manager)
   is

      procedure Ask
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      ---------
      -- Ask --
      ---------

      procedure Ask
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
         use Concorde.Quantities;
      begin
         if not Commodity.Transient
           and then not Commodity.Employment
           and then Quantity > Zero
         then
            Manager.Add_Ask (Commodity, Quantity, Value);
         end if;
      end Ask;

   begin
      Concorde.Stock.Scan_Stock
        (Has_Stock => Manager.Installation,
         Process   => Ask'Access);
      Manager.Scan_Stock (Ask'Access);
   end Set_Sale_Stock;

end Concorde.Installations.Managers;
