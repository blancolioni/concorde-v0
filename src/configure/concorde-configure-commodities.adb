with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.String_Maps;

with Tropos.Reader;

with Concorde.Configure.Metrics;

with Concorde.Commodities;
with Concorde.Identifiers;
with Concorde.Random;

with Concorde.Db.Calculation;
with Concorde.Db.Commodity;
with Concorde.Db.Construction_Input;
with Concorde.Db.Consumer_Commodity;
with Concorde.Db.Derived_Metric;
with Concorde.Db.Fuzzy_Set;
with Concorde.Db.Input_Commodity;
with Concorde.Db.Manufactured;
with Concorde.Db.Metric;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Resource;
with Concorde.Db.Resource_Constraint;
with Concorde.Db.Resource_Sphere;
with Concorde.Db.Stock_Item;
with Concorde.Db.Supply_Input;

package body Concorde.Configure.Commodities is

   type Resource_Availability is
      record
         Reference : Concorde.Db.Resource_Reference;
         Frequency : Unit_Real;
      end record;

   type Available_Resources is
     array (Positive range <>) of Resource_Availability;

   procedure Configure_Resources
     (Config         : Tropos.Configuration);

   procedure Configure_Resource
     (Config         : Tropos.Configuration);

   procedure Create_Frequency_Constraint
     (Resource : Concorde.Db.Resource_Reference;
      Config   : Tropos.Configuration);

   procedure Create_Sphere_Constraint
     (Resource : Concorde.Db.Resource_Reference;
      Config   : Tropos.Configuration);

   procedure Configure_Non_Resources
     (Commodity_Config : Tropos.Configuration);

   procedure Configure_Resource_Spheres
     (Config    : Tropos.Configuration;
      Available : Available_Resources)
     with Unreferenced;

   procedure Configure_Commodity_Metrics
     (Commodity_Tag  : String;
      Coefficients   : Tropos.Configuration;
      Metrics_Config : Tropos.Configuration);

   procedure Create_Standard_Nodes
     (Commodity_Tag : String);

   procedure Add_Calculation
     (Commodity_Tag   : String;
      Calculation_Tag : String;
      Content         : Concorde.Db.Node_Value_Type;
      Expr            : String);

   type Frequency_Type is (Unlimited, Abundant, Common, Uncommon, Rare);

   type Normal_Value is
      record
         Mean    : Real;
         Std_Dev : Real;
      end record;

   Standard_Frequencies : constant array (Frequency_Type) of Normal_Value :=
                            (Unlimited => (0.0, 0.0),
                             Abundant  => (1.0, 0.1),
                             Common    => (0.5, 0.05),
                             Uncommon  => (0.1, 0.01),
                             Rare      => (0.01, 0.001));

   type Constraint_Application is access
     procedure (Constraint : Concorde.Db.Resource_Constraint_Reference;
                Config     : Tropos.Configuration);

   package Constraint_Argument_Maps is
     new WL.String_Maps (Constraint_Application);

   Constraint_Argument_Map : Constraint_Argument_Maps.Map;

   procedure Initialize_Constraint_Arguments;

   procedure Constrain_Composition
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Hydrosphere
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Life
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Minimum_Age
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Mass
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Zone
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   ---------------------
   -- Add_Calculation --
   ---------------------

   procedure Add_Calculation
     (Commodity_Tag   : String;
      Calculation_Tag : String;
      Content         : Concorde.Db.Node_Value_Type;
      Expr            : String)
   is
      Metric  : constant Concorde.Db.Derived_Metric_Reference :=
                  Concorde.Db.Derived_Metric.Create
                    (Content     => Content,
                     Tag         =>
                       Commodity_Tag & "-" & Calculation_Tag,
                     Calculation =>
                       Concorde.Db.Null_Calculation_Reference);

      Node    : constant Concorde.Db.Node_Reference :=
                  Concorde.Db.Derived_Metric.Get (Metric)
                  .Get_Node_Reference;

      Expression  : constant String :=
                      To_Single_Line (Expr);

      Calculation : constant Concorde.Db.Calculation_Reference :=
                      Concorde.Db.Calculation.Create
                        (Identifier => Identifiers.Next_Identifier,
                         Node       => Node,
                         Expression => Expression);
   begin
      Concorde.Db.Derived_Metric.Update_Derived_Metric (Metric)
        .Set_Calculation (Calculation)
        .Done;
      Ada.Text_IO.Put_Line
        (Commodity_Tag & ": " & Calculation_Tag
         & " = " & Expr);
   end Add_Calculation;

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities (Scenario_Name : String) is
   begin
      Configure_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "resource.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "refined.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "manufactured.commodity")));
   end Configure_Commodities;

   ---------------------------------
   -- Configure_Commodity_Metrics --
   ---------------------------------

   procedure Configure_Commodity_Metrics
     (Commodity_Tag  : String;
      Coefficients   : Tropos.Configuration;
      Metrics_Config : Tropos.Configuration)
   is
      procedure Add_Auto_Calculation
        (Tag     : String;
         Content : Concorde.Db.Node_Value_Type;
         Expr    : String);

      --------------------------
      -- Add_Auto_Calculation --
      --------------------------

      procedure Add_Auto_Calculation
        (Tag     : String;
         Content : Concorde.Db.Node_Value_Type;
         Expr    : String)
      is
         Node_Tag : constant String := Commodity_Tag & "-" & Tag;
      begin
         if not Concorde.Db.Node.Get_By_Tag (Node_Tag).Has_Element then
            Add_Calculation
              (Commodity_Tag   => Commodity_Tag,
               Calculation_Tag => Tag,
               Content         => Content,
               Expr            => Expr);
         end if;
      end Add_Auto_Calculation;

   begin

      for Metric_Config of Metrics_Config loop
         declare
            Content : constant Concorde.Db.Node_Value_Type :=
                        Concorde.Db.Node_Value_Type'Value
                          (Metric_Config.Get ("content", "rating"));
         begin
            Add_Calculation
              (Commodity_Tag   => Commodity_Tag,
               Calculation_Tag => Metric_Config.Config_Name,
               Content         => Content,
               Expr            => Metric_Config.Get ("value"));
         end;
      end loop;

      declare
         function P (Name, Default : String) return String
         is (Coefficients.Get (Name, Default));

         function T (Tag : String) return String
         is (Commodity_Tag & "-" & Tag);

      begin
         Add_Auto_Calculation
           (Tag     => "price",
            Content => Concorde.Db.Rating,
            Expr    =>
              "delay "
            & P ("price-pressure-delay", "14")
            & " " & T ("p-price"));

         Add_Auto_Calculation
           (Tag     => "production",
            Content => Concorde.Db.Quantity,
            Expr    =>
              P ("production-sector", "service")
            & " * "
            & T ("share")
            & " * "
            & P ("supply-coefficient", "1000"));

         Concorde.Configure.Metrics.Update_Metric
           (Commodity_Tag & "-supply",
            T ("production"));

         Add_Auto_Calculation
           (Tag     => "share",
            Content => Concorde.Db.Setting,
            Expr    =>
              "smooth " & P ("market-share-self-smoothing", "14")
            & " " & T ("share")
            & " + delay " & P ("market-share-pressure-delay", "14")
            & " (" & T ("p-prod") & " * "
            & P ("market-share-pressure-factor", "0.01")
            & ")");

         Add_Auto_Calculation
           (Tag     => "availability",
            Content => Concorde.Db.Rating,
            Expr    =>
              "smooth "
            & P ("pressure-smoothing", "14")
            & " (" & T ("supply")
            & " / max 1 " & T ("demand") & " - 1)");

         declare
            use Ada.Strings.Unbounded;
            Part_Count    : Natural := 0;
            Part_Pressure : Unbounded_String :=
                              To_Unbounded_String ("0");
            Sell_Price    : constant String :=
                              "smooth " & P ("pressure-price-smoothing", "14")
                            & " " & T ("price");
         begin
            for Part_Config of Coefficients.Child ("parts") loop
               Concorde.Configure.Metrics.Update_Metric
                 (Metric_Tag  => Part_Config.Config_Name & "-demand",
                  Calculation =>
                    Part_Config.Value & " * "
                  & T ("production"));
               Part_Count := Part_Count + Part_Config.Value;
            end loop;

            for Part_Config of Coefficients.Child ("parts") loop
               Part_Pressure := Part_Pressure
                 & " + "
                 & Part_Config.Config_Name & "-price * "
                 & Part_Config.Value;
            end loop;

            Add_Auto_Calculation
              (Tag     => "p-prod",
               Content => Concorde.Db.Rating,
               Expr    =>
                 "((" & Sell_Price & ")"
               & (if Part_Count = 0 then ") / 2"
                 else " - (" & To_String (Part_Pressure) & ")"
                 & " /" & Positive'Image (Part_Count)
                 & ") / 3"));

            Add_Auto_Calculation
              (Tag     => "p-price",
               Content => Concorde.Db.Rating,
               Expr    =>
                 "-1 * delay 14 (smooth 14 " & T ("availability") & ")");
         end;
      end;

   end Configure_Commodity_Metrics;

   ---------------------------
   -- Configure_Constructed --
   ---------------------------

   procedure Configure_Constructed
     (Constructed : Concorde.Db.Constructed_Reference;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
                       (if Config.Contains ("build")
                        then Config.Child ("build")
                        else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Db.Construction_Input.Create
                 (Constructed => Constructed,
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Constructed;

   -----------------------------
   -- Configure_Non_Resources --
   -----------------------------

   procedure Configure_Non_Resources
     (Commodity_Config  : Tropos.Configuration)
   is
      function Get (Config  : Tropos.Configuration;
                    Name    : String;
                    Default : Real)
                    return Concorde.Money.Price_Type
      is (Concorde.Money.To_Price
          (Real (Long_Float'(Config.Get (Name, Default)))));

   begin
      for Item_Config of Commodity_Config loop
         Concorde.Db.Manufactured.Create
           (Enabled_By => Concorde.Db.Null_Technology_Reference,
            Tag        => Item_Config.Config_Name,
            Base_Price => Get (Item_Config, "base-price", 10.0),
            Mass       => Item_Config.Get ("mass", 1.0));
         Create_Standard_Nodes (Item_Config.Config_Name);
         Configure_Commodity_Metrics
           (Item_Config.Config_Name, Item_Config,
            Item_Config.Child ("metrics"));
      end loop;

      for Item_Config of Commodity_Config loop
         declare
            Item : constant Concorde.Db.Manufactured_Reference :=
                     Concorde.Db.Manufactured.Get_Reference_By_Tag
                       (Item_Config.Config_Name);
         begin
            for Input_Config of Item_Config.Child ("parts") loop
               declare
                  use Concorde.Commodities;
                  Tag      : constant String := Input_Config.Config_Name;
                  Input    : constant Commodity_Reference :=
                               (if Exists (Tag)
                                then Get (Tag)
                                else raise Constraint_Error with
                                Item_Config.Config_Name
                                & ": no such input commodity: " & Tag);
                  Quantity : constant Concorde.Quantities.Quantity_Type :=
                               Concorde.Quantities.To_Quantity
                                 (Input_Config.Value);
               begin
                  Concorde.Db.Input_Commodity.Create
                    (Manufactured => Item,
                     Commodity    => Input,
                     Quantity     => Quantity);
               end;
            end loop;
         end;
      end loop;

      for Item_Config of Commodity_Config loop
         declare
            Item : constant Concorde.Db.Commodity_Reference :=
                     Concorde.Db.Commodity.Get_Reference_By_Tag
                       (Item_Config.Config_Name);
         begin
            if Item_Config.Contains ("per-pop") then
               Concorde.Db.Consumer_Commodity.Create
                 (Commodity    => Item,
                  Pop_Per_Item =>
                    Concorde.Quantities.To_Quantity
                      (Item_Config.Get ("per-pop")));
            end if;
         end;
      end loop;

   end Configure_Non_Resources;

   ------------------------
   -- Configure_Resource --
   ------------------------

   procedure Configure_Resource
     (Config         : Tropos.Configuration)
   is
      function Get
        (Name    : String;
         Default : Long_Float)
         return Concorde.Money.Price_Type
      is (Concorde.Money.To_Price
          (Real (Long_Float'(Config.Get (Name, Default)))));

      Resource : constant Concorde.Db.Resource_Reference :=
                   Concorde.Db.Resource.Create
                     (Mass            => 1.0,
                      Tag             => Config.Config_Name,
                      Base_Price      => Get ("base-price", 1.0),
                      Is_Raw_Resource => False);
   begin

      Create_Standard_Nodes (Config.Config_Name);

      Concorde.Db.Metric.Create
        (Content => Concorde.Db.Quantity,
         Tag     => Config.Config_Name & "-production");

      Concorde.Configure.Metrics.Update_Metric
        (Config.Config_Name & "-demand", "0");

      Configure_Commodity_Metrics
        (Config.Config_Name, Config, Config.Child ("metrics"));

      for Deposit_Config of Config.Child ("deposits") loop
         if Deposit_Config.Config_Name = "sphere" then
            Create_Sphere_Constraint (Resource, Deposit_Config);
         else
            Create_Frequency_Constraint (Resource, Deposit_Config);
         end if;
      end loop;
   end Configure_Resource;

   --------------------------------
   -- Configure_Resource_Spheres --
   --------------------------------

   procedure Configure_Resource_Spheres
     (Config    : Tropos.Configuration;
      Available : Available_Resources)
   is
      Radius : constant Tropos.Configuration := Config.Child ("radius");
      RX     : constant Real := Real (Float'(Radius.Get (1)));
      RY     : constant Real := Real (Float'(Radius.Get (2)));
      RZ     : constant Real := Real (Float'(Radius.Get (3)));
      S_Min  : constant Real :=
                 Config.Child ("strength").Get (1);
      S_Max  : constant Real :=
                 Config.Child ("strength").Get (2);
      A_Min  : constant Real :=
                 Config.Child ("attenuation").Get (1);
      A_Max  : constant Real :=
                 Config.Child ("attenuation").Get (2);
      Total  : Non_Negative_Real := 0.0;
   begin
      for R of Available loop
         Total := Total + R.Frequency;
      end loop;

      for I in 1 .. Config.Get ("count") loop
         declare
            X     : constant Real :=
                      RX * (Concorde.Random.Unit_Random * 2.0 - 1.0);
            Y     : constant Real :=
                      RY * (Concorde.Random.Unit_Random * 2.0 - 1.0);
            Z     : constant Real :=
                      RZ * (Concorde.Random.Unit_Random * 2.0 - 1.0);
            S     : constant Real :=
                      S_Min + Concorde.Random.Unit_Random * (S_Max - S_Min);
            R     : constant Real :=
                      Real'Max
                        (Concorde.Random.Normal_Random (0.1) * S, RX / 20.0);
            A     : constant Real :=
                      A_Min + Concorde.Random.Unit_Random * (A_Max - A_Min);
            F     : Non_Negative_Real :=
                      Concorde.Random.Unit_Random * Total;
            Index : Positive := 1;
         begin
            while F > Available (Index).Frequency loop
               F := F - Available (Index).Frequency;
               Index := Index + 1;
            end loop;

            Concorde.Db.Resource_Sphere.Create
              (Resource    => Available (Index).Reference,
               Centre_X    => X,
               Centre_Y    => Y,
               Centre_Z    => Z,
               Strength    => S * Available (Index).Frequency,
               Radius      => R,
               Attenuation => A);
         end;
      end loop;
   end Configure_Resource_Spheres;

   -------------------------
   -- Configure_Resources --
   -------------------------

   procedure Configure_Resources
     (Config         : Tropos.Configuration)
   is
      package Name_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);
      Names : Name_Vectors.Vector;
   begin
      for Resource_Config of Config loop
         Names.Append (Resource_Config.Config_Name);
         Configure_Resource (Resource_Config);
      end loop;
   end Configure_Resources;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock.Has_Stock_Type;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
   begin
      Configure_Stock (Has_Stock.Get_Has_Stock_Reference, Config, Factor);
   end Configure_Stock;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
                       (if Config.Contains ("stock")
                        then Config.Child ("stock")
                        else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Db.Stock_Item.Create
                 (Has_Stock => Has_Stock,
                  Commodity => Commodity,
                  Quantity  => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Stock;

   ------------------------
   -- Configure_Supplied --
   ------------------------

   procedure Configure_Supplied
     (Supplied  : Concorde.Db.Supplied_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
                       (if Config.Contains ("supply")
                        then Config.Child ("supply")
                        else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Db.Supply_Input.Create
                 (Supplied    => Supplied,
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Supplied;

   ---------------------------
   -- Constrain_Composition --
   ---------------------------

   procedure Constrain_Composition
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Composition_Constraint (True)
        .Set_Composition (Concorde.Db.World_Composition'Value (Config.Value))
          .Done;
   end Constrain_Composition;

   ---------------------------
   -- Constrain_Hydrosphere --
   ---------------------------

   procedure Constrain_Hydrosphere
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Hydrosphere_Constraint (True)
        .Set_Min_Hydrosphere (Real (Long_Float'(Config.Value)))
        .Done;
   end Constrain_Hydrosphere;

   --------------------
   -- Constrain_Life --
   --------------------

   procedure Constrain_Life
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Life_Constraint (True)
        .Set_Min_Lifeforms (Config.Value)
        .Done;
   end Constrain_Life;

   --------------------
   -- Constrain_Mass --
   --------------------

   procedure Constrain_Mass
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
      type Mass_Constraint is (Small, Medium, Large);
      type Mass_Value is array (1 .. 4) of Real;

      function Get (Index : Positive) return Real
      is (Real (Long_Float'(Config.Get (Index))));

      Mass_Range       : constant array (Mass_Constraint) of Mass_Value :=
                           (Small  => (0.0, 0.01, 0.2, 0.8),
                            Medium => (0.6, 0.9, 1.5, 2.0),
                            Large  => (0.6, 0.9, 1.5, 2.0));
      Constraint_Name  : constant Mass_Constraint :=
                           (if Config.Child_Count = 1
                            then Mass_Constraint'Value (Config.Value)
                            else Medium);
      Constraint_Range : constant Mass_Value :=
                           (if Config.Child_Count = 1
                            then Mass_Range (Constraint_Name)
                            else (Get (1), Get (2), Get (3), Get (4)));
      Fuzzy_Ref        : constant Concorde.Db.Fuzzy_Set_Reference :=
                           Concorde.Db.Fuzzy_Set.Create
                             (Constraint_Range (1),
                              Constraint_Range (2),
                              Constraint_Range (3),
                              Constraint_Range (4));

   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Mass_Constraint (True)
        .Set_Mass (Fuzzy_Ref)
        .Done;
   end Constrain_Mass;

   ---------------------------
   -- Constrain_Minimum_Age --
   ---------------------------

   procedure Constrain_Minimum_Age
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Age_Constraint (True)
        .Set_Min_Age (Real (Long_Float'(Config.Value)))
        .Done;
   end Constrain_Minimum_Age;

   --------------------
   -- Constrain_Zone --
   --------------------

   procedure Constrain_Zone
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration)
   is
   begin
      Concorde.Db.Resource_Constraint.Update_Resource_Constraint (Constraint)
        .Set_Zone_Constraint (True)
        .Set_Zone (Concorde.Db.Stellar_Orbit_Zone'Value (Config.Value))
          .Done;
   end Constrain_Zone;

   ---------------------------------
   -- Create_Frequency_Constraint --
   ---------------------------------

   procedure Create_Frequency_Constraint
     (Resource : Concorde.Db.Resource_Reference;
      Config   : Tropos.Configuration)
   is
      Freq_Name  : constant Frequency_Type :=
                     Frequency_Type'Value (Config.Config_Name);
      Freq       : constant Normal_Value :=
                     Standard_Frequencies (Freq_Name);
      Constraint : constant Concorde.Db.Resource_Constraint_Reference :=
                     Concorde.Db.Resource_Constraint.Create
                       (Resource               => Resource,
                        Mass_Constraint        => False,
                        Zone_Constraint        => False,
                        Life_Constraint        => False,
                        Age_Constraint         => False,
                        Hydrosphere_Constraint => False,
                        Composition_Constraint => False,
                        Sphere_Constraint      => False,
                        Mass                   =>
                          Concorde.Db.Null_Fuzzy_Set_Reference,
                        Zone                   =>
                          Concorde.Db.Black,
                        Composition            => Concorde.Db.Rock_Iron,
                        Min_Lifeforms          => 0,
                        Min_Age                => 0.0,
                        Min_Hydrosphere        => 0.0,
                        Sphere_Frequency       => 0.0,
                        Sphere_Rx              => 0.0,
                        Sphere_Ry              => 0.0,
                        Sphere_Rz              => 0.0,
                        Attenuation_Min        => 0.0,
                        Attenuation_Max        => 0.0,
                        Unlimited              => Freq_Name = Unlimited,
                        Mean                   => Freq.Mean,
                        Standard_Deviation     => Freq.Std_Dev);
   begin

      if Constraint_Argument_Map.Is_Empty then
         Initialize_Constraint_Arguments;
      end if;

      for Child_Config of Config loop
         if Constraint_Argument_Map.Contains (Child_Config.Config_Name) then
            Constraint_Argument_Map.Element (Child_Config.Config_Name)
              (Constraint, Child_Config);
         else
            raise Constraint_Error with
              "no such constraint argument " & Child_Config.Config_Name
              & " in resource constraint for "
              & Concorde.Db.Resource.Get (Resource).Tag;
         end if;
      end loop;
   end Create_Frequency_Constraint;

   ------------------------------
   -- Create_Sphere_Constraint --
   ------------------------------

   procedure Create_Sphere_Constraint
     (Resource : Concorde.Db.Resource_Reference;
      Config   : Tropos.Configuration)
   is
      function Get (Field : String;
                    Index : Natural := 0)
                    return Real
      is (if Index = 0
          then Real (Long_Float'(Config.Get (Field)))
          else Real (Long_Float'(Config.Child (Field).Get (Index))));

   begin
      Concorde.Db.Resource_Constraint.Create
        (Resource               => Resource,
         Mass_Constraint        => False,
         Zone_Constraint        => False,
         Life_Constraint        => False,
         Age_Constraint         => False,
         Hydrosphere_Constraint => False,
         Composition_Constraint => False,
         Sphere_Constraint      => True,
         Mass                   => Concorde.Db.Null_Fuzzy_Set_Reference,
         Zone                   => Concorde.Db.Black,
         Composition            => Concorde.Db.Hydrogen,
         Min_Lifeforms          => 0,
         Min_Age                => 0.0,
         Min_Hydrosphere        => 0.0,
         Sphere_Frequency       => Get ("frequency"),
         Sphere_Rx              => Get ("radius", 1),
         Sphere_Ry              => Get ("radius", 2),
         Sphere_Rz              => Get ("radius", 3),
         Attenuation_Min        => Get ("attenuation", 1),
         Attenuation_Max        => Get ("attenuation", 2),
         Unlimited              => False,
         Mean                   => Get ("strength", 1),
         Standard_Deviation     => Get ("strength", 2));
   end Create_Sphere_Constraint;

   ---------------------------
   -- Create_Standard_Nodes --
   ---------------------------

   procedure Create_Standard_Nodes
     (Commodity_Tag : String)
   is
      function T (Suffix : String) return String
      is (Commodity_Tag & "-" & Suffix);

   begin
      Concorde.Db.Metric.Create
        (Content => Concorde.Db.Quantity,
         Tag     => T ("stockpile"));

      Add_Calculation
        (Commodity_Tag   => Commodity_Tag,
         Calculation_Tag => "sold",
         Content         => Concorde.Db.Quantity,
         Expr            =>
           "min (max (" & T ("demand") & " - " & T ("supply") & ") 0) "
         & "(" & T ("stockpile") & " * " & T ("sell-stock") & ")");

      Add_Calculation
        (Commodity_Tag   => Commodity_Tag,
         Calculation_Tag => "bought",
         Content         => Concorde.Db.Quantity,
         Expr            =>
           "min (max (" & T ("supply") & " - " & T ("demand") & ") 0) "
         & "(max (" & T ("min-stock") & " - " & T ("stockpile") & ") 0)");

      Concorde.Configure.Metrics.Update_Metric
        (T ("supply"),
         T ("sold"));

      Concorde.Configure.Metrics.Update_Metric
        (T ("demand"),
         T ("bought"));

      Concorde.Db.Metric.Create
        (Content => Concorde.Db.Money,
         Tag     => T ("base-price"));

      declare
         Revenue       : constant String :=
                           T ("base-price")
                         & " * (1 + " & T ("price") & ")"
                         & " * " & T ("sold");
         Expense       : constant String :=
                           T ("base-price")
                         & " * (1 + " & T ("price") & ")"
                         & " * " & T ("bought");

         Revenue_Calc  : constant Concorde.Db.Calculation_Reference :=
                           Concorde.Db.Calculation.Create
                             (Concorde.Identifiers.Next_Identifier,
                              Concorde.Db.Null_Node_Reference,
                              Revenue);
         Expense_Calc  : constant Concorde.Db.Calculation_Reference :=
                           Concorde.Db.Calculation.Create
                             (Concorde.Identifiers.Next_Identifier,
                              Concorde.Db.Null_Node_Reference,
                              Expense);
         Sell_Policy   : constant Concorde.Db.Policy_Reference :=
                           Concorde.Db.Policy.Create
                             (Content => Concorde.Db.Setting,
                              Tag     => T ("sell-stock"),
                              Expense =>
                                Concorde.Db.Null_Calculation_Reference,
                              Revenue => Revenue_Calc);
         Min_Stockpile    : constant Concorde.Db.Policy_Reference :=
                              Concorde.Db.Policy.Create
                                (Content => Concorde.Db.Quantity,
                                 Tag     => T ("min-stock"),
                                 Expense => Expense_Calc,
                                 Revenue =>
                                   Concorde.Db.Null_Calculation_Reference);
      begin
         Concorde.Db.Calculation.Update_Calculation
           (Revenue_Calc)
           .Set_Node (Concorde.Db.Policy.Get (Sell_Policy).Get_Node_Reference)
             .Done;
         Concorde.Db.Calculation.Update_Calculation
           (Expense_Calc)
           .Set_Node
             (Concorde.Db.Policy.Get (Min_Stockpile).Get_Node_Reference)
               .Done;
      end;
   end Create_Standard_Nodes;

   -------------------------------------
   -- Initialize_Constraint_Arguments --
   -------------------------------------

   procedure Initialize_Constraint_Arguments is

      procedure Add (Name : String;
                     Proc : Constraint_Application);

      ---------
      -- Add --
      ---------

      procedure Add (Name : String;
                     Proc : Constraint_Application)
      is
      begin
         Constraint_Argument_Map.Insert (Name, Proc);
      end Add;

   begin
      Add ("composition", Constrain_Composition'Access);
      Add ("hydrosphere", Constrain_Hydrosphere'Access);
      Add ("life-bearing", Constrain_Life'Access);
      Add ("mass", Constrain_Mass'Access);
      Add ("minimum-age", Constrain_Minimum_Age'Access);
      Add ("zone", Constrain_Zone'Access);
   end Initialize_Constraint_Arguments;

end Concorde.Configure.Commodities;
