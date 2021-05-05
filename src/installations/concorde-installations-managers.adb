with Concorde.Agents;
with Concorde.Managers.Agents;
with Concorde.Markets;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;
with Concorde.Sectors;
with Concorde.Stock;

with Concorde.Handles.Commodity;
with Concorde.Handles.Employment;
with Concorde.Handles.Facility;
with Concorde.Handles.Facility_Service;
with Concorde.Handles.Facility_Worker;
with Concorde.Handles.Installation;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Resource;
with Concorde.Handles.Service_Commodity;
with Concorde.Handles.World_Sector;

with Concorde.Db;

package body Concorde.Installations.Managers is

   type Default_Installation_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Installation : Concorde.Handles.Installation.Installation_Handle;
         Facility     : Concorde.Handles.Facility.Facility_Handle;
      end record;

   procedure Initialize_Manager
     (Manager : in out Default_Installation_Manager'Class;
      Installation : Concorde.Handles.Installation.Installation_Class);

   function Employees
     (Manager : Default_Installation_Manager'Class;
      Group   : Concorde.Handles.Pop_Group.Pop_Group_Class)
      return Concorde.Quantities.Quantity_Type;

   function Worker_Capacity
     (Manager : Default_Installation_Manager'Class)
      return Unit_Real;

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

   overriding function Identifier
     (Manager : Default_Outpost_Manager)
      return String
   is (Describe (Manager.Installation) & " manager");

   overriding procedure Set_Sale_Stock
     (Manager : in out Default_Outpost_Manager);

   type Default_Service_Manager is new Default_Installation_Manager with
      record
         null;
      end record;

   overriding function Identifier
     (Manager : Default_Service_Manager)
      return String
   is (Describe (Manager.Installation) & " service manager");

   overriding procedure Execute_Production
     (Manager : in out Default_Service_Manager);

   type Default_Farm_Manager is new Default_Installation_Manager with
      record
         null;
      end record;

   overriding function Identifier
     (Manager : Default_Farm_Manager)
      return String
   is (Describe (Manager.Installation) & " farm manager");

   overriding procedure Execute_Production
     (Manager : in out Default_Farm_Manager);

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : Default_Installation_Manager :=
                       Default_Installation_Manager'
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
      return Manager;
   end Create_Default_Manager;

   -------------------------
   -- Create_Farm_Manager --
   -------------------------

   function Create_Farm_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : Default_Farm_Manager;
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using farm manager");
      Initialize_Manager (Manager, Installation);
      return Manager;
   end Create_Farm_Manager;

   ----------------------------
   -- Create_Outpost_Manager --
   ----------------------------

   function Create_Outpost_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : Default_Outpost_Manager :=
                       Default_Outpost_Manager'
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
      return Manager;
   end Create_Outpost_Manager;

   ----------------------------
   -- Create_Service_Manager --
   ----------------------------

   function Create_Service_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : Default_Service_Manager;
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using service manager");
      Initialize_Manager (Manager, Installation);
      return Manager;
   end Create_Service_Manager;

   ---------------
   -- Employees --
   ---------------

   function Employees
     (Manager : Default_Installation_Manager'Class;
      Group   : Concorde.Handles.Pop_Group.Pop_Group_Class)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Quantities.Quantity_Type;
      Result : Concorde.Quantities.Quantity_Type :=
                 Concorde.Quantities.Zero;
   begin
      for Employment of
        Concorde.Handles.Employment.Select_By_Employer
          (Manager.Installation)
      loop
         if Employment.Pop.Pop_Group.Tag = Group.Tag then
            Result := Result + Employment.Quantity;
         end if;
      end loop;
      return Result;
   end Employees;

   ------------------------
   -- Execute_Production --
   ------------------------

   overriding procedure Execute_Production
     (Manager : in out Default_Service_Manager)
   is
      use Concorde.Money, Concorde.Quantities;
      Capacity : constant Unit_Real :=
                   Manager.Worker_Capacity;
      Cost     : Money_Type := Zero;
   begin

      for Employment of
        Concorde.Handles.Employment.Select_By_Employer
          (Manager.Installation)
      loop
         Cost := Cost + Total (Employment.Salary, Employment.Quantity);
      end loop;

      Default_Installation_Manager (Manager).Execute_Production;

      Manager.Log
        ("executing production: worker cost "
         & Show (Cost)
         & "; capacity "
         & Concorde.Real_Images.Approximate_Image (Capacity * 100.0)
         & "%");

      for Facility_Service of
        Concorde.Handles.Facility_Service.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            Service  : constant Concorde.Handles.Service_Commodity
              .Service_Commodity_Class
                := Facility_Service.Service_Commodity;
            Quality  : constant Positive :=
                         1 + Concorde.Db.Quality_Type'Pos
                           (Facility_Service.Service_Commodity.Quality);
            Quantity : constant Quantity_Type :=
                         Scale (Manager.Facility.Capacity,
                                Capacity / (1.0 + Real (Quality) ** 2));

         begin
            Manager.Remove_Stock (Service,
                                  Manager.Stock_Quantity (Service));
            if Quantity > Zero then
               Manager.Log
                 ("produced " & Show (Quantity) & " "
                  & Service.Tag
                  & " for " & Concorde.Money.Show (Cost)
                  & " ("
                  & Concorde.Money.Show (Concorde.Money.Price (Cost, Quantity))
                  & " each)");
               Manager.Add_Stock (Service, Quantity, Cost);
               Manager.Create_Ask
                 (Commodity => Service,
                  Quantity  => Quantity,
                  Price     => Price (Adjust (Cost, 2.0), Quantity));
            end if;
         end;
      end loop;
   end Execute_Production;

   ------------------------
   -- Execute_Production --
   ------------------------

   overriding procedure Execute_Production
     (Manager : in out Default_Farm_Manager)
   is
      use Concorde.Money, Concorde.Quantities;
      Installation  : constant Handles.Installation.Installation_Handle :=
                        Manager.Installation;
      Best_Resource : Concorde.Handles.Resource.Resource_Handle;
      Best_Yield    : Unit_Real := 0.0;
      Sector        : constant Handles.World_Sector.World_Sector_Class :=
                        Manager.Installation.World_Sector;
      Capacity      : constant Unit_Real :=
                        Manager.Worker_Capacity;
      Cost          : Money_Type := Zero;
   begin

      for Employment of
        Concorde.Handles.Employment.Select_By_Employer
          (Manager.Installation)
      loop
         Cost := Cost + Total (Employment.Salary, Employment.Quantity);
      end loop;

      Default_Installation_Manager (Manager).Execute_Production;

      Manager.Log
        ("executing production: worker cost "
         & Show (Cost)
         & "; capacity "
         & Concorde.Real_Images.Approximate_Image (Capacity * 100.0)
         & "%");

      if Empty_Queue (Manager.Installation) then
         for Commodity of
           Concorde.Handles.Resource.Select_By_Category
             (Concorde.Db.Organic)
         loop
            declare
               Yield : constant Unit_Real :=
                         Concorde.Sectors.Resource_Yield
                           (Sector, Commodity);
            begin
               if Yield > Best_Yield then
                  Best_Yield := Yield;
                  Best_Resource := Commodity.To_Resource_Handle;
               end if;
            end;
         end loop;

         if not Best_Resource.Has_Element then
            Manager.Log ("no possible production");
            return;
         end if;

         Manager.Log ("queuing: " & Best_Resource.Tag
                      & "; yield "
                      & Concorde.Real_Images.Approximate_Image
                        (Best_Yield * 100.0)
                      & "%");

         Queue_Production (Manager.Installation, Best_Resource,
                           Concorde.Quantities.To_Quantity
                             (1.0e6 * Best_Yield));
      end if;

      if not Empty_Queue (Manager.Installation) then
         Set_Production
           (Manager.Installation,
            First_Queued_Commodity (Manager.Installation));

         declare
            use Concorde.Handles.Commodity;
            Production : constant Commodity_Class :=
                           First_Queued_Commodity (Installation);
            Target     : constant Quantity_Type :=
                           First_Queued_Quantity (Installation);
            Yield      : constant Unit_Real :=
                           Concorde.Sectors.Resource_Yield
                             (Manager.Installation.World_Sector,
                              Concorde.Handles.Resource
                                .Get_From_Commodity (Production));
            Quantity   : constant Quantity_Type :=
                           Scale (Manager.Installation.Facility.Capacity,
                                  100.0
                                  * Yield * Capacity
                                  * (1.0 - Installation.Inefficiency));
         begin
            Manager.Log
              ("produced " & Show (Quantity) & " "
               & Production.Tag
               & " for " & Concorde.Money.Show (Cost)
               & " ("
               & Concorde.Money.Show (Concorde.Money.Price (Cost, Quantity))
               & " each)");
            Manager.Add_Stock (Production, Quantity, Cost);
            Manager.Create_Ask
              (Commodity => Production,
               Quantity  => Quantity,
               Price     => Price (Adjust (Cost, 2.0), Quantity));
            Update_Queue_First
              (Manager.Installation,
               Target - Min (Target, Quantity));
         end;
      end if;
   end Execute_Production;

   ------------------------
   -- Initialize_Manager --
   ------------------------

   procedure Initialize_Manager
     (Manager : in out Default_Installation_Manager'Class;
      Installation : Concorde.Handles.Installation.Installation_Class)
   is
   begin
      Manager.Initialize_Agent_Manager
        (Agent          => Installation,
         Market         =>
           Concorde.Markets.World_Market
             (Installation.World_Sector.World),
         Planning_Cycle => 10);
      Manager.Installation := Installation.To_Installation_Handle;
      Manager.Facility     := Installation.Facility.To_Facility_Handle;
   end Initialize_Manager;

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

   ---------------------
   -- Worker_Capacity --
   ---------------------

   function Worker_Capacity
     (Manager : Default_Installation_Manager'Class)
      return Unit_Real
   is
      Capacity : Unit_Real := 1.0;
   begin
      for Facility_Worker of
        Concorde.Handles.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            use Concorde.Quantities;
            Employees : constant Quantity_Type :=
                          Manager.Employees (Facility_Worker.Pop_Group);
         begin
            if Employees < Facility_Worker.Quantity then
               Capacity :=
                 Unit_Real'Min
                   (Capacity,
                    To_Real (Employees) / To_Real (Facility_Worker.Quantity));
            end if;
         end;
      end loop;

      return Capacity;

   end Worker_Capacity;

end Concorde.Installations.Managers;
