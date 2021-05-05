with WL.Random.Weighted_Random_Choices;

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
with Concorde.Handles.Facility_Production;
with Concorde.Handles.Facility_Service;
with Concorde.Handles.Facility_Worker;
with Concorde.Handles.Input_Commodity;
with Concorde.Handles.Installation;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Resource;
with Concorde.Handles.Service_Commodity;
with Concorde.Handles.World_Sector;

with Concorde.Db;

package body Concorde.Installations.Managers is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   type Default_Installation_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Installation : Concorde.Handles.Installation.Installation_Handle;
         Facility     : Concorde.Handles.Facility.Facility_Handle;
         Salary_Cost  : Concorde.Money.Money_Type;
         Input_Cost   : Concorde.Money.Money_Type;
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

   overriding procedure Create_Planning
     (Manager : in out Default_Farm_Manager);

   overriding procedure Execute_Production
     (Manager : in out Default_Farm_Manager);

   type Default_Factory_Manager is new Default_Installation_Manager with
      record
         null;
      end record;

   overriding function Identifier
     (Manager : Default_Factory_Manager)
      return String
   is (Describe (Manager.Installation) & " factory manager");

   overriding procedure Create_Planning
     (Manager : in out Default_Factory_Manager);

   overriding procedure Set_Requirements
     (Manager : in out Default_Factory_Manager);

   overriding procedure Execute_Production
     (Manager : in out Default_Factory_Manager);

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
      Manager      : Default_Installation_Manager;
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using default manager");
      Manager.Initialize_Manager (Installation);
      return Manager;
   end Create_Default_Manager;

   ----------------------------
   -- Create_Factory_Manager --
   ----------------------------

   function Create_Factory_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class
   is
      use Concorde.Handles.Installation;
      Installation : constant Installation_Handle :=
                       Get_From_Managed (Managed);
      Manager      : Default_Factory_Manager;
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using factory manager");
      Initialize_Manager (Manager, Installation);
      return Manager;
   end Create_Factory_Manager;

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
      Manager      : Default_Outpost_Manager;
   begin
      Concorde.Agents.Log_Agent
        (Installation,
         Describe (Installation),
         "using outpost manager");
      Manager.Initialize_Manager (Installation);
      return Manager;
   end Create_Outpost_Manager;

   ---------------------
   -- Create_Planning --
   ---------------------

   overriding procedure Create_Planning
     (Manager : in out Default_Factory_Manager)
   is
   begin

      Default_Installation_Manager (Manager).Create_Planning;

      if Empty_Queue (Manager.Installation) then
         for Production of
           Concorde.Handles.Facility_Production.Select_By_Facility
             (Manager.Facility)
         loop
            Queue_Capacity_Production
              (Installation => Manager.Installation,
               Commodity    => Production.Commodity);
         end loop;
      end if;

   end Create_Planning;

   ---------------------
   -- Create_Planning --
   ---------------------

   overriding procedure Create_Planning
     (Manager : in out Default_Farm_Manager)
   is
      use Concorde.Money, Concorde.Quantities;

      package Resource_Choices is
        new WL.Random.Weighted_Random_Choices
          (Concorde.Handles.Resource.Resource_Handle,
           Concorde.Handles.Resource."=");

      Choices : Resource_Choices.Weighted_Choice_Set;

      Sector        : constant Handles.World_Sector.World_Sector_Class :=
                        Manager.Installation.World_Sector;
   begin
      Default_Installation_Manager (Manager).Create_Planning;
      if not Empty_Queue (Manager.Installation) then
         return;
      end if;

      for Commodity of
        Concorde.Handles.Resource.Select_By_Category
          (Concorde.Db.Organic)
      loop
         declare
            Yield : constant Unit_Real :=
                      Concorde.Sectors.Resource_Yield
                        (Sector, Commodity);
            Price : constant Price_Type :=
                      Manager.Current_Bid_Price
                        (Commodity,
                         Scale
                           (Manager.Facility.Capacity,
                            Yield));
            Score : constant Natural :=
                      Natural (Yield * To_Real (Price) * 100.0);
         begin
            Manager.Log
              (Commodity.Tag
               & ": yield "
               & Image (Yield * 100.0) & "%"
               & "; price "
               & Show (Price)
               & "; score"
               & Score'Image);

            Choices.Insert (Commodity.To_Resource_Handle, Score);
         end;
      end loop;

      if Choices.Is_Empty then
         Manager.Log ("no possible production");
         return;
      end if;

      declare
         Resource : constant Concorde.Handles.Resource.Resource_Handle :=
                      Choices.Choose;
         Yield    : constant Unit_Real :=
                      Concorde.Sectors.Resource_Yield
                        (Sector, Resource);
      begin
         Manager.Log ("queuing: " & Resource.Tag
                      & "; yield "
                      & Image (Yield * 100.0)
                      & "%");

         Queue_Production (Manager.Installation, Resource,
                           Concorde.Quantities.To_Quantity
                             (1.0e6 * Yield));
      end;

   end Create_Planning;

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
      Manager.Initialize_Manager (Installation);
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
     (Manager : in out Default_Factory_Manager)
   is
      use Concorde.Money, Concorde.Quantities;
      Installation       : constant Handles.Installation.Installation_Handle :=
                             Manager.Installation;
      Capacity           : constant Unit_Real :=
                             Manager.Worker_Capacity;
      Remaining_Capacity : Quantity_Type :=
                             Scale (Manager.Facility.Capacity, Capacity);
   begin

      Default_Installation_Manager (Manager).Execute_Production;

      while Remaining_Capacity > Zero
        and then not Empty_Queue (Manager.Installation)
      loop

         Manager.Log
           ("executing production: worker cost "
            & Show (Manager.Salary_Cost)
            & "; capacity "
            & Concorde.Real_Images.Approximate_Image (Capacity * 100.0)
            & "%");

         Set_Production
           (Manager.Installation,
            First_Queued_Commodity (Manager.Installation));

         declare
            use Concorde.Handles.Commodity;
            Production    : constant Commodity_Class :=
                              First_Queued_Commodity (Installation);
            Target        : constant Quantity_Type :=
                              First_Queued_Quantity (Installation);
            Complexity    : constant Non_Negative_Real :=
                              Production.Complexity;
            Required_Cap  : constant Quantity_Type :=
                              (if Target = Zero
                               then Remaining_Capacity
                               else Scale
                                 (Target,
                                  Complexity / Installation.Inefficiency));
            Available_Cap : constant Quantity_Type :=
                              Remaining_Capacity;
            Used_Cap      : constant Quantity_Type :=
                              Min (Required_Cap, Available_Cap);
            Max_Quantity  : constant Quantity_Type :=
                              Scale
                                (Used_Cap,
                                 Installation.Inefficiency / Complexity);
            Quantity      : Quantity_Type := Max_Quantity;
         begin

            for Input of
              Concorde.Handles.Input_Commodity.Select_By_Commodity
                (Production)
            loop
               declare
                  Available : constant Quantity_Type :=
                                Concorde.Stock.Get_Quantity
                                  (Manager.Installation, Input.Input);
                  Required  : constant Quantity_Type :=
                                Max_Quantity * Input.Quantity;
               begin
                  if Available < Required then
                     Quantity := Min (Quantity, Available / Input.Quantity);
                  end if;
               end;
            end loop;

            for Input of
              Concorde.Handles.Input_Commodity.Select_By_Commodity
                (Production)
            loop
               declare
                  Required  : constant Quantity_Type :=
                                Quantity * Input.Quantity;
                  Value     : Money_Type;
               begin
                  Concorde.Stock.Remove_Stock
                    (Manager.Installation, Input.Input, Required, Value);
                  Manager.Input_Cost := Manager.Input_Cost + Value;
                  Manager.Log ("consuming "
                               & Show (Required)
                               & " "
                               & Input.Input.Tag
                               & " valued at "
                               & Show (Value));
               end;
            end loop;

            declare
               Cost : constant Money_Type :=
                        Manager.Salary_Cost + Manager.Input_Cost;
            begin
               Manager.Log
                 ("produced " & Show (Quantity) & " "
                  & Production.Tag
                  & " for " & Concorde.Money.Show (Cost)
                  & " ("
                  & Concorde.Money.Show (Concorde.Money.Price (Cost, Quantity))
                  & " each)");
               Manager.Add_Stock (Production, Quantity, Cost);
               Update_Queue_First
                 (Manager.Installation,
                  Target - Min (Target, Quantity));
            end;

            Remaining_Capacity := Remaining_Capacity - Used_Cap;

         end;
      end loop;

      declare

         procedure Add_Ask
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type);

         -------------
         -- Add_Ask --
         -------------

         procedure Add_Ask
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type)
         is
         begin
            if Concorde.Handles.Facility_Production
              .Is_Facility_Production (Manager.Facility, Commodity)
            then
               declare
                  Min_Price : constant Price_Type :=
                                Price (Adjust (Value, 1.05), Quantity);
                  Current_Price : constant Price_Type :=
                                    Adjust_Price
                                      (Manager.Current_Bid_Price
                                         (Commodity, Quantity),
                                       0.95);
               begin
                  Manager.Create_Ask
                    (Commodity => Commodity,
                     Quantity  => Quantity,
                     Price     => Max (Min_Price, Current_Price));
               end;
            end if;
         end Add_Ask;

      begin
         Concorde.Stock.Scan_Stock
           (Manager.Installation, Add_Ask'Access);
      end;

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
      Capacity      : constant Unit_Real :=
                        Manager.Worker_Capacity;
   begin

      Default_Installation_Manager (Manager).Execute_Production;

      Manager.Log
        ("executing production: worker cost "
         & Show (Manager.Salary_Cost)
         & "; capacity "
         & Concorde.Real_Images.Approximate_Image (Capacity * 100.0)
         & "%");

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
            Cost       : constant Money_Type :=
                           Manager.Salary_Cost;
         begin
            Manager.Log
              ("produced " & Show (Quantity) & " "
               & Production.Tag
               & " for " & Concorde.Money.Show (Cost)
               & " ("
               & Concorde.Money.Show (Concorde.Money.Price (Cost, Quantity))
               & " each)");
            Manager.Add_Stock (Production, Quantity, Cost);
            Update_Queue_First
              (Manager.Installation,
               Target - Min (Target, Quantity));
         end;
      end if;

      declare

         procedure Add_Ask
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type);

         -------------
         -- Add_Ask --
         -------------

         procedure Add_Ask
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type)
         is
         begin
            if not Commodity.Transient and then not Commodity.Employment then
               Manager.Create_Ask
                 (Commodity => Commodity,
                  Quantity  => Quantity,
                  Price     => Price (Adjust (Value, 2.0), Quantity));
            end if;
         end Add_Ask;

      begin
         Concorde.Stock.Scan_Stock
           (Manager.Installation, Add_Ask'Access);
      end;

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
      Manager.Salary_Cost := Concorde.Money.Zero;
      Manager.Input_Cost := Concorde.Money.Zero;
   end Initialize_Manager;

   ---------------------
   -- Pay_Daily_Costs --
   ---------------------

   overriding procedure Pay_Daily_Costs
     (Manager : in out Default_Installation_Manager)
   is
      use type Concorde.Money.Money_Type;
   begin
      Manager.Salary_Cost := Concorde.Money.Zero;
      Manager.Input_Cost := Concorde.Money.Zero;

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

            Manager.Salary_Cost := Manager.Salary_Cost + Total;
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

   ----------------------
   -- Set_Requirements --
   ----------------------

   overriding procedure Set_Requirements
     (Manager : in out Default_Factory_Manager)
   is
      procedure Add_Requirement
        (Commodity    : Concorde.Handles.Commodity.Commodity_Class;
         Quantity     : Concorde.Quantities.Quantity_Type);

      ---------------------
      -- Add_Requirement --
      ---------------------

      procedure Add_Requirement
        (Commodity    : Concorde.Handles.Commodity.Commodity_Class;
         Quantity     : Concorde.Quantities.Quantity_Type)
      is
         use Concorde.Quantities;
         use Concorde.Handles.Commodity;
         Installation : constant Handles.Installation.Installation_Handle :=
                          Manager.Installation;
         Production    : constant Commodity_Class :=
                           First_Queued_Commodity (Installation);
         Target        : constant Quantity_Type :=
                           First_Queued_Quantity (Installation);
         Complexity    : constant Non_Negative_Real :=
                           Production.Complexity;
         Required_Cap  : constant Quantity_Type :=
                           (if Target = Zero
                            then Installation.Facility.Capacity
                            else Scale
                              (Target,
                               Complexity / Installation.Inefficiency));
         Available_Cap : constant Quantity_Type :=
                           Installation.Facility.Capacity;
         Used_Cap      : constant Quantity_Type :=
                           Min (Required_Cap, Available_Cap);
         Max_Quantity  : constant Quantity_Type :=
                           Scale
                             (Used_Cap,
                              Installation.Inefficiency / Complexity);
         Q             : constant Quantity_Type :=
                           (if Target = Zero
                            then Max_Quantity
                            else Quantity);
      begin
         for Requirement of
           Concorde.Handles.Input_Commodity.Select_By_Commodity
             (Commodity)
         loop
            declare
               Required : constant Quantity_Type :=
                            Q * Requirement.Quantity;
               Available : constant Quantity_Type :=
                             Concorde.Stock.Get_Quantity
                               (Manager.Installation, Requirement.Input);
               Missing   : constant Quantity_Type :=
                             (if Available < Required
                              then Required - Available
                              else Zero);
            begin
               if Missing > Zero then
                  Manager.Add_Requirement
                    (Commodity => Requirement.Input,
                     Necessary => Missing,
                     Desired   => Missing);
               end if;
            end;
         end loop;
      end Add_Requirement;

   begin
      Default_Installation_Manager (Manager).Set_Requirements;
      Iterate_Queue (Manager.Installation, Add_Requirement'Access);
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
