with Ada.Text_IO;

with Concorde.Data_Series;
with Concorde.Logging;
with Concorde.Options;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;
with Concorde.Stock;
with Concorde.Weighted_Random_Choices;

with Concorde.Agents;
with Concorde.Worlds;

with Concorde.Db.Consumer_Good;
with Concorde.Db.Deposit;
with Concorde.Db.Facility_Worker;
with Concorde.Db.Factory;
with Concorde.Db.Generated_Resource;
with Concorde.Db.Installation;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Produced_Commodity;
with Concorde.Db.Recipe;
with Concorde.Db.Recipe_Input;
with Concorde.Db.Resource;
with Concorde.Db.Resource_Generator;

package body Concorde.Managers.Installations is

   type Resource_Generator_Manager is
     new Root_Installation_Manager with
      record
         Rgen : Concorde.Db.Resource_Generator_Reference;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Resource_Generator_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager);

   type Factory_Manager is
     new Root_Installation_Manager with
      record
         Factory    : Concorde.Db.Factory_Reference;
         Production : Concorde.Db.Commodity_Reference :=
                        Concorde.Db.Null_Commodity_Reference;
         Recipe     : Concorde.Db.Recipe_Reference :=
                        Concorde.Db.Null_Recipe_Reference;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Factory_Manager);

   overriding procedure Get_Required_Stock
     (Manager : Factory_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type);

   overriding procedure Get_Desired_Stock
     (Manager : Factory_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Factory_Manager);

   procedure Choose_Recipe
     (Manager : in out Factory_Manager'Class);

   type Hub_Manager is
     new Root_Installation_Manager with
      record
         Day_Tick  : Natural := 0;
         Log_State : Boolean := False;
      end record;

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager);

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Hub_Manager);

   -------------------
   -- Choose_Recipe --
   -------------------

   procedure Choose_Recipe
     (Manager : in out Factory_Manager'Class)
   is

      package Recipe_Choices is
        new Concorde.Weighted_Random_Choices
          (Concorde.Db.Recipe_Reference);

      Choices : Recipe_Choices.Weighted_Choice_Set;

      function Score_Production
        (Commodity : Concorde.Db.Commodity_Reference;
         Recipe    : Concorde.Db.Recipe_Reference)
         return Natural;

      ----------------------
      -- Score_Production --
      ----------------------

      function Score_Production
        (Commodity : Concorde.Db.Commodity_Reference;
         Recipe    : Concorde.Db.Recipe_Reference)
         return Natural
      is
         use Concorde.Money, Concorde.Quantities;
         Input_Cost  : Money_Type := Zero;
         Worker_Cost : Money_Type := Zero;
         Capacity    : constant Quantity_Type :=
                         Concorde.Db.Facility.Get (Manager.Facility).Capacity;
         Value : constant Money_Type :=
                         Total (Manager.Current_Market_Bid_Price (Commodity),
                                Capacity);
      begin
         for Input of Concorde.Db.Recipe_Input.Select_By_Recipe (Recipe) loop
            Input_Cost := Input_Cost
              + Total
              (Manager.Current_Market_Ask_Price (Input.Commodity),
               Input.Quantity);
         end loop;

         for Worker of
           Concorde.Db.Facility_Worker.Select_By_Facility
             (Manager.Facility)
         loop
            Worker_Cost := Worker_Cost
              + Total
              (Manager.Current_Market_Ask_Price
                 (Concorde.Db.Pop_Group.Get (Worker.Pop_Group)
                  .Get_Commodity_Reference),
               Worker.Quantity);
         end loop;

         declare
            use type Concorde.Db.Recipe_Reference;
            Input_Price : constant Price_Type :=
                            Price (Input_Cost, Unit);
            Total_Cost  : constant Money_Type :=
                            Total (Input_Price, Capacity) + Worker_Cost;
            Profit      : constant Real :=
                            (To_Real (Value) / To_Real (Total_Cost) - 1.0);
            Score       : constant Natural :=
                            (if Profit > 0.0
                             then Natural (Profit * 1000.0)
                             else 0);
         begin
            Concorde.Logging.Log
              (Actor    => Concorde.Db.To_String (Manager.Installation),
               Location => Concorde.Worlds.Name (Manager.World),
               Category => "production",
               Message  =>
                 Concorde.Commodities.Local_Name (Commodity)
               & ": input price " & Show (Input_Price)
               & "; total input cost " & Show (Total (Input_Price, Capacity))
               & "; worker cost " & Show (Worker_Cost)
               & "; expected earnings " & Show (Value)
               & (if Profit <= 0.0 then ""
                 else "; profit margin "
                 & Concorde.Real_Images.Approximate_Image (Profit * 100.0)
                 & "%")
               & "; score" & Score'Image);
            if Recipe = Manager.Recipe then
               return Score * 2;  --  we don't like change
            else
               return Score;
            end if;

         end;
      end Score_Production;

   begin
      for Produced of
        Concorde.Db.Produced_Commodity.Select_By_Factory
          (Manager.Factory)
      loop
         for Recipe of
           Concorde.Db.Recipe.Select_By_Commodity
             (Produced.Commodity)
         loop
            Choices.Insert
              (Recipe.Get_Recipe_Reference,
               Score_Production
                 (Produced.Commodity,
                  Recipe.Get_Recipe_Reference));
         end loop;
      end loop;

      if not Choices.Is_Empty then
         declare
            Choice : constant Concorde.Db.Recipe_Reference :=
                       Choices.Choose;
         begin
            Concorde.Logging.Log
              (Actor    => Concorde.Db.To_String (Manager.Installation),
               Location => Concorde.Worlds.Name (Manager.World),
               Category => "production",
               Message  =>
                 "producing: "
               & Concorde.Commodities.Local_Name
                 (Concorde.Db.Recipe.Get (Choice).Commodity));
            Manager.Recipe := Choice;
            Manager.Production :=
              Concorde.Db.Recipe.Get (Choice).Commodity;
            Concorde.Db.Installation.Get (Manager.Installation).Set_Production
              (Manager.Production);
         end;
      end if;
   end Choose_Recipe;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Concorde.Db.Installation.Installation_Type :=
                       Concorde.Db.Installation.Get_Installation
                         (Managed);
      Facility     : constant Concorde.Db.Facility.Facility_Type :=
                       Concorde.Db.Facility.Get
                         (Installation.Facility);
   begin
      case Facility.Top_Record is
         when Concorde.Db.R_Resource_Generator =>
            Concorde.Logging.Log
              (Actor    => Facility.Tag,
               Location => Concorde.Worlds.Name (Installation.World),
               Category => "manager",
               Message  => "starting");

            declare
               Rgen : constant Concorde.Db.Resource_Generator_Reference :=
                        Concorde.Db.Resource_Generator.Get_Resource_Generator
                          (Installation.Facility)
                          .Get_Resource_Generator_Reference;
               Manager : Resource_Generator_Manager :=
                           (Concorde.Managers.Agents.Root_Agent_Manager with
                            Installation =>
                              Installation.Get_Installation_Reference,
                            Employer        =>
                              Installation.Get_Employer_Reference,
                            Facility        => Installation.Facility,
                            Payroll         => Concorde.Money.Zero,
                            Rgen            => Rgen);
            begin
               Manager.Initialize_Agent_Manager
                 (Installation, Installation.World);
               return new Resource_Generator_Manager'(Manager);
            end;

         when Concorde.Db.R_Factory =>
            Concorde.Logging.Log
              (Actor    => Facility.Tag,
               Location => Concorde.Worlds.Name (Installation.World),
               Category => "manager",
               Message  => "starting");

            declare
               Factory : constant Concorde.Db.Factory_Reference :=
                           Concorde.Db.Factory.Get_Factory
                             (Installation.Facility)
                             .Get_Factory_Reference;
               Manager : Factory_Manager :=
                           (Concorde.Managers.Agents.Root_Agent_Manager with
                            Installation    =>
                              Installation.Get_Installation_Reference,
                            Employer        =>
                              Installation.Get_Employer_Reference,
                            Facility        => Installation.Facility,
                            Payroll         => Concorde.Money.Zero,
                            Factory         => Factory,
                            Production      => Installation.Production,
                            Recipe          =>
                              Concorde.Db.Recipe.First_Reference_By_Commodity
                                (Installation.Production));
            begin
               Manager.Initialize_Agent_Manager
                 (Installation, Installation.World);
               return new Factory_Manager'(Manager);
            end;

         when others =>
            Ada.Text_IO.Put_Line
              ("warning: "
               & "no manager for facility "
               & Facility.Tag);
            return null;
      end case;
   end Create_Default_Manager;

   ------------------------
   -- Create_Hub_Manager --
   ------------------------

   function Create_Hub_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type
   is
      Installation : constant Concorde.Db.Installation.Installation_Type :=
                       Concorde.Db.Installation.Get_Installation
                         (Managed);
      Manager      : Hub_Manager :=
                       Hub_Manager'
                         (Concorde.Managers.Agents.Root_Agent_Manager with
                          Installation =>
                            Installation.Get_Installation_Reference,
                          Employer        =>
                            Installation.Get_Employer_Reference,
                          Facility        => Installation.Facility,
                          Payroll         => Concorde.Money.Zero,
                          Log_State       => Concorde.Options.Log_Trade_Offers,
                          Day_Tick     => 0);
   begin
      Manager.Initialize_Agent_Manager
        (Installation, Installation.World);
      return new Hub_Manager'(Manager);
   end Create_Hub_Manager;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Resource_Generator_Manager)
   is
   begin
      Concorde.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;

      for Gen of
        Concorde.Db.Generated_Resource.Select_By_Resource_Generator
          (Manager.Rgen)
      loop
         declare
            use Concorde.Quantities;
            Commodity : constant Concorde.Db.Commodity_Reference :=
                          Concorde.Db.Resource.Get (Gen.Resource)
                          .Get_Commodity_Reference;
            Quantity  : constant Quantity_Type :=
                          Manager.Current_Stock (Commodity);
         begin
            if Quantity > Zero then
               Manager.Place_Ask
                 (Commodity => Commodity,
                  Quantity  => Quantity,
                  Price     => Manager.Current_Market_Bid_Price (Commodity));
            end if;
         end;
      end loop;

   end Create_Market_Offers;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Factory_Manager)
   is
      use Concorde.Db;

      procedure Add_Ask
        (Item     : Concorde.Db.Commodity_Reference;
         Quantity : Concorde.Quantities.Quantity_Type;
         Value    : Concorde.Money.Money_Type);

      -------------
      -- Add_Ask --
      -------------

      procedure Add_Ask
        (Item     : Concorde.Db.Commodity_Reference;
         Quantity : Concorde.Quantities.Quantity_Type;
         Value    : Concorde.Money.Money_Type)
      is
      begin
         if not Concorde.Commodities.Is_Pop_Group (Item)
           and then not Concorde.Db.Recipe_Input.Is_Recipe_Input
             (Manager.Recipe, Item)
         then
            declare
               Current_Price : constant Concorde.Money.Price_Type :=
                                 Concorde.Money.Adjust_Price
                                   (Manager.Current_Market_Bid_Price (Item),
                                    0.95);
               Cost_Price    : constant Concorde.Money.Price_Type :=
                                 Concorde.Money.Price (Value, Quantity);
               Minimum_Price : constant Concorde.Money.Price_Type :=
                                 Concorde.Money.Adjust_Price
                                   (Cost_Price, 1.1);
               Desired_Price : constant Concorde.Money.Price_Type :=
                                 Concorde.Money.Adjust_Price
                                   (Cost_Price, 2.5);
               Ask_Price     : constant Concorde.Money.Price_Type :=
                                 Concorde.Money.Min
                                   (Desired_Price,
                                    Concorde.Money.Max
                                      (Current_Price, Minimum_Price));
            begin
               Manager.Log
                 ("factory: creating ask for "
                  & Concorde.Commodities.Local_Name (Item)
                  & ": quantity "
                  & Concorde.Quantities.Show (Quantity)
                  & "; value "
                  & Concorde.Money.Show (Value)
                  & "; current price "
                  & Concorde.Money.Show (Current_Price)
                  & "; cost price "
                  & Concorde.Money.Show (Cost_Price)
                  & "; ask price "
                  & Concorde.Money.Show (Ask_Price));
               Manager.Place_Ask
                 (Item, Quantity, Ask_Price);
            end;
         end if;
      end Add_Ask;

   begin
      Concorde.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;

      if Manager.Recipe = Null_Recipe_Reference then
         Manager.Choose_Recipe;
      end if;

      Manager.Scan_Current_Stock (Add_Ask'Access);

   end Create_Market_Offers;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Hub_Manager)
   is
      Installation : constant Concorde.Db.Installation.Installation_Type :=
                       Concorde.Db.Installation.Get
                         (Manager.Installation);

      procedure Add_Ask
        (Item     : Concorde.Db.Commodity_Reference;
         Quantity : Concorde.Quantities.Quantity_Type;
         Value    : Concorde.Money.Money_Type);

      procedure Add_Bid
        (Item     : Concorde.Db.Commodity_Reference);

      -------------
      -- Add_Ask --
      -------------

      procedure Add_Ask
        (Item     : Concorde.Db.Commodity_Reference;
         Quantity : Concorde.Quantities.Quantity_Type;
         Value    : Concorde.Money.Money_Type)
      is
         pragma Unreferenced (Value);
      begin
         Manager.Place_Ask
           (Commodity => Item,
            Quantity  => Quantity,
            Price     =>
              Concorde.Money.Adjust_Price
                (Manager.Current_Agent_Stock_Price (Item), 1.1));
      end Add_Ask;

      -------------
      -- Add_Bid --
      -------------

      procedure Add_Bid
        (Item     : Concorde.Db.Commodity_Reference)
      is
         use Concorde.Quantities;
         Quantity : constant Quantity_Type :=
                      Manager.Current_Market_Ask_Quantity
                        (Item);
      begin
         if Quantity > Manager.Current_Stock (Item) then
            Manager.Place_Bid
              (Commodity => Item,
               Quantity  => Quantity - Manager.Current_Stock (Item),
               Price     =>
                 Concorde.Money.Adjust_Price
                   (Manager.Current_Agent_Stock_Price (Item),
                    0.9));
         end if;
      end Add_Bid;

   begin
      Create_Market_Offers (Root_Installation_Manager (Manager));

      Concorde.Logging.Log
        (Actor    => Installation.Identity,
         Location => Concorde.Worlds.Name (Installation.World),
         Category => "market",
         Message  => "scanning stock");

      Concorde.Stock.Scan_Stock
        (Installation, Add_Ask'Access);

      for Item of Concorde.Db.Consumer_Good.Scan_By_Tag loop
         Add_Bid (Item.Get_Commodity_Reference);
      end loop;
      for Item of Concorde.Db.Resource.Scan_By_Tag loop
         Add_Bid (Item.Get_Commodity_Reference);
      end loop;

   end Create_Market_Offers;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Installation_Manager)
   is
      use Concorde.Money;
   begin

      Manager.Payroll := Zero;

      for Workers of
        Concorde.Db.Pop.Select_By_Employer
          (Manager.Employer)
      loop

         declare
            This_Cost : constant Money_Type :=
                          Total (Workers.Salary, Workers.Size);
         begin
            Manager.Log
              ("salary for " & Concorde.Quantities.Show (Workers.Size)
               & " "
               & Concorde.Db.Pop_Group.Get (Workers.Pop_Group).Tag
               & " "
               & Show (Workers.Salary)
               & " total "
               & Show (This_Cost));

            Manager.Pay (This_Cost);
            Concorde.Agents.Add_Cash (Workers, This_Cost);
            Manager.Payroll := Manager.Payroll + This_Cost;
         end;
      end loop;

   end Execute_Agent_Tasks;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Resource_Generator_Manager)
   is
      Installation : constant Concorde.Db.Installation.Installation_Type :=
                       Concorde.Db.Installation.Get
                         (Manager.Installation);
      Gen          : constant Concorde.Db.Resource_Generator
        .Resource_Generator_Type :=
          Concorde.Db.Resource_Generator.Get
            (Manager.Rgen);

   begin

      Root_Installation_Manager (Manager).Execute_Agent_Tasks;

      for Deposit of
        Concorde.Db.Deposit.Select_By_World_Sector
          (Installation.World_Sector)
      loop
         declare
            Resource : constant Concorde.Db.Resource_Reference :=
                         Deposit.Resource;
         begin
            if Concorde.Db.Generated_Resource.Is_Generated_Resource
              (Gen.Get_Resource_Generator_Reference, Resource)
            then
               declare
                  use Concorde.Quantities;
                  Commodity : constant Concorde.Db.Commodity_Reference :=
                                Concorde.Db.Resource.Get (Resource)
                                .Get_Commodity_Reference;
                  Quantity : constant Quantity_Type :=
                               To_Quantity
                                 (Deposit.Accessibility * 20.0
                                  * (Concorde.Random.Unit_Random + 0.5));
                  Cost     : constant Concorde.Money.Money_Type :=
                               Concorde.Money.Total
                                 (Manager.Current_Market_Bid_Price
                                    (Commodity),
                                  Quantity);
               begin
                  Concorde.Stock.Add_Stock
                    (Installation, Commodity, Quantity, Cost);
               end;
            end if;
         end;
      end loop;

   end Execute_Agent_Tasks;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Factory_Manager)
   is
      Have      : Concorde.Commodities.Stock_Type;
   begin

      Root_Installation_Manager (Manager).Execute_Agent_Tasks;

      Manager.Current_Stock (Have);

      declare
         use Concorde.Money, Concorde.Quantities;
         Capacity  : constant Unit_Real :=
                       Manager.Calculate_Capacity
                         (Have);
         Full_Capacity : constant Quantity_Type :=
                           Concorde.Db.Facility.Get (Manager.Facility)
                           .Capacity;
         Current_Capacity : constant Quantity_Type :=
                              Scale (Full_Capacity, Capacity);
      begin
         Manager.Log
           ("Executing production; capacity: "
            & Concorde.Real_Images.Approximate_Image (Capacity));

         if Capacity > 0.0 then
            declare
               Production_Cost : Money_Type := Zero;

               procedure Consume_Input
                 (Commodity : Concorde.Db.Commodity_Reference;
                  Quantity  : Concorde.Quantities.Quantity_Type;
                  Value     : Concorde.Money.Money_Type);

               -------------------
               -- Consume_Input --
               -------------------

               procedure Consume_Input
                 (Commodity : Concorde.Db.Commodity_Reference;
                  Quantity  : Concorde.Quantities.Quantity_Type;
                  Value     : Concorde.Money.Money_Type)
               is
               begin
                  Manager.Log
                    ("Consuming " & Show (Quantity)
                     & " "
                     & Concorde.Commodities.Local_Name (Commodity)
                     & " value "
                     & Show (Value));
                  Manager.Remove_Stock (Commodity, Quantity);
                  Production_Cost := Production_Cost + Value;
               end Consume_Input;

            begin

               Manager.Log ("payroll cost: "
                            & Concorde.Money.Show (Manager.Payroll));
               Production_Cost := Manager.Payroll;

               for Input of
                 Concorde.Db.Recipe_Input.Select_By_Recipe (Manager.Recipe)
               loop
                  declare
                     Quantity : constant Quantity_Type :=
                                  To_Quantity
                                    (To_Real (Input.Quantity)
                                     * To_Real (Current_Capacity));
                     Price    : constant Price_Type :=
                                  Manager.Current_Agent_Stock_Price
                                    (Input.Commodity);
                     Value    : constant Money_Type :=
                                  Total (Price, Quantity);
                  begin
                     Consume_Input
                       (Commodity => Input.Commodity,
                        Quantity  => Quantity,
                        Value     => Value);
                  end;
               end loop;

               Manager.Log ("producing " & Show (Current_Capacity)
                            & " "
                            & Concorde.Commodities.Local_Name
                              (Manager.Production)
                               & " for " & Show (Production_Cost));

               Manager.Add_Stock
                 (Manager.Production, Current_Capacity, Production_Cost);
            end;
         end if;
      end;
   end Execute_Agent_Tasks;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Hub_Manager)
   is

      Trend_Length : constant := 7;

      procedure Check_Trend (Commodity : Concorde.Db.Commodity_Reference);

      -----------------
      -- Check_Trend --
      -----------------

      procedure Check_Trend (Commodity : Concorde.Db.Commodity_Reference) is

         Series : Concorde.Data_Series.Series;

         procedure Add_Data_Point
           (Date     : Concorde.Calendar.Time;
            Quantity : Concorde.Quantities.Quantity_Type);

         --------------------
         -- Add_Data_Point --
         --------------------

         procedure Add_Data_Point
           (Date     : Concorde.Calendar.Time;
            Quantity : Concorde.Quantities.Quantity_Type)
         is
         begin
            Concorde.Data_Series.Add_Point
              (Series,
               Concorde.Calendar.To_Real (Date),
               Concorde.Quantities.To_Real (Quantity));
         end Add_Data_Point;

      begin
         Manager.Scan_Historical_Stock
           (Commodity, Non_Negative_Real (Trend_Length),
            Add_Data_Point'Access);

         Add_Data_Point (Concorde.Calendar.Clock,
                         Manager.Current_Stock (Commodity));

         declare
            Trend : constant Concorde.Data_Series.Regression :=
                      Concorde.Data_Series.Simple_Linear_Regression
                        (Series);
            Gradient : constant Real :=
                         Concorde.Data_Series.Gradient (Trend);
         begin
            if abs Gradient > 0.001 then
               declare
                  use Concorde.Calendar;
                  Zero_Date : constant Concorde.Calendar.Time :=
                                Concorde.Calendar.To_Time
                                  (Concorde.Data_Series.X_Intercept (Trend));
                  Now       : constant Concorde.Calendar.Time :=
                                Concorde.Calendar.Clock;
                  Max_Increase : constant Concorde.Calendar.Time :=
                                   Now + Days (7.0);
                  Min_Increase : constant Concorde.Calendar.Time :=
                                   Now + Days (70.0);
                  Adjustment   : Non_Negative_Real := 1.0;
                  Old_Price    : constant Concorde.Money.Price_Type :=
                                   Manager.Current_Agent_Stock_Price
                                     (Commodity);
               begin
                  if Zero_Date > Now then
                     if Zero_Date < Max_Increase then
                        Adjustment := 1.5;
                     elsif Zero_Date < Min_Increase then
                        Adjustment :=
                          1.0 +
                            Real (Min_Increase - Zero_Date)
                          / Real (Days (1))
                          / 500.0;
                     end if;

                     if Adjustment /= 1.0 then
                        Manager.Set_Agent_Stock_Price
                          (Commodity,
                           Concorde.Money.Adjust_Price
                             (Manager.Current_Agent_Stock_Price (Commodity),
                              Adjustment));

                        Concorde.Logging.Log
                          (Actor    =>
                             Concorde.Db.Facility.Get
                               (Manager.Facility).Tag,
                           Location =>
                             Concorde.Worlds.Name (Manager.World),
                           Category =>
                             Concorde.Commodities.Local_Name (Commodity),
                           Message  =>
                             "stock exhausted within "
                           & Concorde.Real_Images.Approximate_Image
                             (Real (Zero_Date - Now) / Real (Days (1)))
                           & " days on "
                           & Concorde.Calendar.Image (Zero_Date)
                           & "; adjustment +"
                           & Concorde.Real_Images.Approximate_Image
                             ((Adjustment - 1.0) * 100.0)
                           & "%"
                           & "; old price "
                           & Concorde.Money.Show (Old_Price)
                           & "; new price "
                           & Concorde.Money.Show
                             (Manager.Current_Agent_Stock_Price
                                  (Commodity)));
                     end if;
                  elsif Concorde.Data_Series.Gradient (Trend) > 0.5 then
                     declare
                        D : constant Unit_Real :=
                              1.0 - Real'Min (Gradient, 5.0) / 50.0;
                     begin
                        Manager.Set_Agent_Stock_Price
                          (Commodity,
                           Concorde.Money.Adjust_Price
                             (Manager.Current_Agent_Stock_Price (Commodity),
                              D));
                        Concorde.Logging.Log
                          (Actor    =>
                             Concorde.Db.Facility.Get
                               (Manager.Facility).Tag,
                           Location =>
                             Concorde.Worlds.Name (Manager.World),
                           Category =>
                             Concorde.Commodities.Local_Name (Commodity),
                           Message  =>
                             "stock growing at "
                           & Concorde.Real_Images.Approximate_Image (Gradient)
                           & "; adjustment -"
                           & Concorde.Real_Images.Approximate_Image
                             ((1.0 - D) * 100.0)
                           & "%"
                           & "; old price "
                           & Concorde.Money.Show (Old_Price)
                           & "; new price "
                           & Concorde.Money.Show
                             (Manager.Current_Agent_Stock_Price (Commodity)));
                     end;
                  end if;
               end;
            end if;
         end;
      end Check_Trend;

   begin

      Root_Installation_Manager (Manager).Execute_Agent_Tasks;

      Manager.Day_Tick := Manager.Day_Tick + 1;
      if Manager.Day_Tick mod Trend_Length = 0
        or else Manager.Day_Tick in 2 .. Trend_Length - 1
      then
         for Item of Concorde.Db.Consumer_Good.Scan_By_Tag loop
            Check_Trend (Item.Get_Commodity_Reference);
         end loop;
         for Item of Concorde.Db.Resource.Scan_By_Tag loop
            Check_Trend (Item.Get_Commodity_Reference);
         end loop;
      end if;

      if Manager.Log_State then
         Manager.Log_Market_State;
      end if;

   end Execute_Agent_Tasks;

   -----------------------
   -- Get_Desired_Stock --
   -----------------------

   overriding procedure Get_Desired_Stock
     (Manager : Factory_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type)
   is
      use Concorde.Quantities;
      Capacity : constant Quantity_Type :=
                   Concorde.Db.Factory.Get (Manager.Factory).Capacity;
   begin
      Root_Installation_Manager (Manager).Get_Required_Stock (Stock);
      for Input of
        Concorde.Db.Recipe_Input.Select_By_Recipe
          (Manager.Recipe)
      loop
         declare
            Quantity         : constant Quantity_Type := Input.Quantity;
            Desired_Quantity : constant Quantity_Type :=
                                 To_Quantity
                                   (To_Real (Capacity)
                                    * To_Real (Quantity)
                                    * 4.0);
         begin
            Stock.Set_Quantity
              (Commodity => Input.Commodity,
               Quantity  => Desired_Quantity,
               Price_Per =>
                 Manager.Current_Market_Ask_Price (Input.Commodity));
         end;
      end loop;
   end Get_Desired_Stock;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Installation_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type)
   is
   begin
      for Employee of
        Concorde.Db.Facility_Worker.Select_By_Facility
          (Manager.Facility)
      loop
         declare
            Commodity : constant Concorde.Db.Commodity_Reference :=
                          Concorde.Db.Pop_Group.Get (Employee.Pop_Group)
                          .Get_Commodity_Reference;
            Quantity  : constant Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Scale
                            (Employee.Quantity, Manager.Capacity);
            Price     : constant Concorde.Money.Price_Type :=
                          Manager.Current_Market_Ask_Price (Commodity);
         begin
            Stock.Set_Quantity
              (Commodity, Quantity, Concorde.Money.Total (Price, Quantity));
         end;
      end loop;
   end Get_Required_Stock;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Factory_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type)
   is
   begin
      Root_Installation_Manager (Manager).Get_Required_Stock (Stock);
      for Input of
        Concorde.Db.Recipe_Input.Select_By_Recipe
          (Manager.Recipe)
      loop
         declare
            use Concorde.Quantities;
            Capacity : constant Quantity_Type :=
                         Concorde.Db.Factory.Get (Manager.Factory).Capacity;
            Quantity : constant Quantity_Type := Input.Quantity;
            Required_Quantity : constant Quantity_Type :=
                                  To_Quantity
                                    (To_Real (Capacity)
                                     * To_Real (Quantity)
                                     * Manager.Capacity);
         begin
            Stock.Set_Quantity
              (Commodity => Input.Commodity,
               Quantity  => Required_Quantity,
               Price_Per =>
                 Manager.Current_Market_Ask_Price (Input.Commodity));
         end;
      end loop;
   end Get_Required_Stock;

end Concorde.Managers.Installations;
