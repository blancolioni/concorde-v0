with Ada.Containers.Indefinite_Vectors;

with WL.String_Maps;

with Tropos.Reader;

with Concorde.Commodities;
with Concorde.Random;

with Concorde.Db.Commodity;
with Concorde.Db.Construction_Input;
with Concorde.Db.Consumer_Commodity;
with Concorde.Db.Fuzzy_Set;
with Concorde.Db.Input_Commodity;
with Concorde.Db.Manufactured;
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
      Config   : Tropos.Configuration)
   is null;

   procedure Configure_Non_Resources
     (Commodity_Config : Tropos.Configuration);

   procedure Configure_Resource_Spheres
     (Config    : Tropos.Configuration;
      Available : Available_Resources)
     with Unreferenced;

   type Frequency_Type is (Common, Uncommon, Rare, Ultrarare);

   type Normal_Value is
      record
         Mean    : Real;
         Std_Dev : Real;
      end record;

   Standard_Frequencies : constant array (Frequency_Type) of Normal_Value :=
                            (Common    => (1.0, 0.1),
                             Uncommon  => (0.2, 0.02),
                             Rare      => (0.05, 0.005),
                             Ultrarare => (0.01, 0.001));

   type Constraint_Application is access
     procedure (Constraint : Concorde.Db.Resource_Constraint_Reference;
                Config     : Tropos.Configuration);

   package Constraint_Argument_Maps is
     new WL.String_Maps (Constraint_Application);

   Constraint_Argument_Map : Constraint_Argument_Maps.Map;

   procedure Initialize_Constraint_Arguments;

   procedure Constrain_Life
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Minimum_Age
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

   procedure Constrain_Mass
     (Constraint : Concorde.Db.Resource_Constraint_Reference;
      Config     : Tropos.Configuration);

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
                (Scenario_Name, "commodities", "water.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "power.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "refined.commodity")));
      Configure_Non_Resources
        (Tropos.Reader.Read_Config
           (Scenario_File
                (Scenario_Name, "commodities", "manufactured.commodity")));
   end Configure_Commodities;

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
                 Concorde.Commodities.Get (Item_Config.Config_Name);
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
   begin
      for Item_Config of Commodity_Config loop
         Concorde.Db.Manufactured.Create
           (Enabled_By => Concorde.Db.Null_Technology_Reference,
            Tag        => Item_Config.Config_Name,
            Mass       => Item_Config.Get ("mass", 1.0));
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
      Resource : constant Concorde.Db.Resource_Reference :=
                   Concorde.Db.Resource.Create
                     (Mass            => 1.0,
                      Tag             => Config.Config_Name,
                      Is_Raw_Resource => False);
   begin
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

      Concorde.Db.Commodity.Create
        ("raw-resources", Mass => 1.0);

      for Resource_Config of Config loop
         Names.Append (Resource_Config.Config_Name);
         Configure_Resource (Resource_Config);
      end loop;

--        declare
--           Refs : Available_Resources (1 .. Names.Last_Index);
--           Freq : Unit_Real := 1.0;
--        begin
--           for I in Refs'Range loop
--              Refs (I) :=
--                Resource_Availability'
--                  (Reference =>
--                     Concorde.Db.Resource.Create
--                       (Tag             => Names.Element (I),
--                        Name            => Names.Element (I),
--                        Mass            => 1.0,
--                        Is_Raw_Resource => False),
--                   Frequency => Freq);
--              Freq := Freq * 0.9;
--           end loop;
--
--           Configure_Resource_Spheres (Config.Child ("spheres"), Refs);
--        end;

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
                 Concorde.Commodities.Get (Item_Config.Config_Name);
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
     (Supplied : Concorde.Db.Supplied_Reference;
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
                 Concorde.Commodities.Get (Item_Config.Config_Name);
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

      Mass_Range : constant array (Mass_Constraint) of Mass_Value :=
                     (Small  => (0.0, 0.01, 0.2, 0.8),
                      Medium => (0.6, 0.9, 1.5, 2.0),
                      Large  => (0.6, 0.9, 1.5, 2.0));
      Constraint_Name : constant Mass_Constraint :=
                          (if Config.Child_Count = 1
                           then Mass_Constraint'Value (Config.Value)
                           else Medium);
      Constraint_Range : constant Mass_Value :=
                           (if Config.Child_Count = 1
                            then Mass_Range (Constraint_Name)
                            else (Get (1), Get (2), Get (3), Get (4)));
      Fuzzy_Ref : constant Concorde.Db.Fuzzy_Set_Reference :=
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

   ---------------------------------
   -- Create_Frequency_Constraint --
   ---------------------------------

   procedure Create_Frequency_Constraint
     (Resource : Concorde.Db.Resource_Reference;
      Config   : Tropos.Configuration)
   is
      Freq_Name : constant Frequency_Type :=
                    Frequency_Type'Value (Config.Config_Name);
      Freq      : constant Normal_Value :=
                    Standard_Frequencies (Freq_Name);
      Constraint : constant Concorde.Db.Resource_Constraint_Reference :=
                     Concorde.Db.Resource_Constraint.Create
                       (Resource            => Resource,
                        Mass_Constraint     => False,
                        Zone_Constraint     => False,
                        Life_Constraint     => False,
                        Age_Constraint      => False,
                        Sphere_Constraint   => False,
                        Mass                =>
                          Concorde.Db.Null_Fuzzy_Set_Reference,
                        Zone                =>
                          Concorde.Db.Black,
                        Min_Lifeforms       => 0,
                        Min_Age             => 0.0,
                        Resource_Sphere     =>
                          Concorde.Db.Null_Resource_Sphere_Reference,
                        Mean                => Freq.Mean,
                        Standard_Deviation  => Freq.Std_Dev);
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
      Add ("life-bearing", Constrain_Life'Access);
      Add ("mass", Constrain_Mass'Access);
      Add ("minimum-age", Constrain_Minimum_Age'Access);
   end Initialize_Constraint_Arguments;

end Concorde.Configure.Commodities;
