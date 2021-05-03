with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.String_Maps;

with Tropos.Reader;

with Concorde.Commodities;
with Concorde.Logging;

with Concorde.Handles.Building_Module;
with Concorde.Handles.Commodity;
with Concorde.Handles.Construction_Input;
with Concorde.Handles.Consumer_Commodity;
with Concorde.Handles.Fuzzy_Set;
with Concorde.Handles.Industrial_Commodity;
with Concorde.Handles.Input_Commodity;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Resource;
with Concorde.Handles.Resource_Constraint;
with Concorde.Handles.Service_Commodity;
with Concorde.Handles.Stock_Item;
with Concorde.Handles.Supply_Input;

with Concorde.Db;

package body Concorde.Configure.Commodities is

   use Concorde.Handles.Commodity;
   use Concorde.Db;

   type Happiness_Rating is range 0 .. 16;

   function Happiness_Level
     (Class : Consumer_Class)
      return Happiness_Rating
   is (case Class is
          when Food => 3,
          when Drink => 2,
          when Intoxicant => 1,
          when Clothing   => 2);

   function Happiness_Level
     (Category : Service_Category)
      return Happiness_Rating
   is (case Category is
          when Education => 1,
          when Fitness => 1,
          when Medical => 2,
          when Entertainment => 3);

   type Commodity_Creator is access
     function (Config : Tropos.Configuration)
               return Commodity_Handle;

   package Creator_Maps is
     new WL.String_Maps (Commodity_Creator);

   Creator_Map : Creator_Maps.Map;

   procedure Initialize_Creator_Map;

   function Create_Building_Module
     (Config : Tropos.Configuration)
      return Commodity_Handle;

   function Create_Consumer_Good
     (Config : Tropos.Configuration)
      return Commodity_Handle;

   function Create_Service_Commodity
     (Config : Tropos.Configuration)
      return Commodity_Handle;

   function Create_Industrial_Good
     (Config : Tropos.Configuration)
      return Commodity_Handle;

   function Create_Resource
     (Config : Tropos.Configuration)
      return Commodity_Handle;

   procedure Create_Frequency_Constraint
     (Resource : Concorde.Handles.Resource.Resource_Handle;
      Config   : Tropos.Configuration);

   procedure Create_Sphere_Constraint
     (Resource : Concorde.Handles.Resource.Resource_Handle;
      Config   : Tropos.Configuration);

   function Get_Price
     (Config : Tropos.Configuration)
      return Concorde.Money.Price_Type
   is (Concorde.Money.To_Price (Config.Get ("npc-price")));

   function Get_Mass
     (Config : Tropos.Configuration)
      return Non_Negative_Real
   is (Config.Get ("mass"));

   procedure Create_Commodity
     (Config : Tropos.Configuration);

   procedure Create_Pop_Groups
     (Config : Tropos.Configuration);

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

   subtype Resource_Constraint_Handle is
     Concorde.Handles.Resource_Constraint.Resource_Constraint_Handle;

   type Constraint_Application is access
     procedure (Constraint : Resource_Constraint_Handle;
                Config     : Tropos.Configuration);

   package Constraint_Argument_Maps is
     new WL.String_Maps (Constraint_Application);

   Constraint_Argument_Map : Constraint_Argument_Maps.Map;

   procedure Initialize_Constraint_Arguments;

   procedure Constrain_Composition
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   procedure Constrain_Hydrosphere
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   procedure Constrain_Life
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   procedure Constrain_Minimum_Age
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   procedure Constrain_Mass
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   procedure Constrain_Zone
     (Constraint : Resource_Constraint_Handle;
      Config     : Tropos.Configuration);

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities (Scenario_Name : String) is
      Config : constant Tropos.Configuration :=
        Tropos.Reader.Read_Config
          (Path      => Scenario_Directory (Scenario_Name, "commodities"),
           Extension => "commodity");
   begin
      for Commodity_Config of Config loop
         begin
            Create_Commodity (Commodity_Config);
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

      Create_Pop_Groups
        (Tropos.Reader.Read_Config
           (Path      => Scenario_Directory (Scenario_Name, "pops"),
            Extension => "pop"));

   end Configure_Commodities;

   --------------------------
   -- Configure_Components --
   --------------------------

   procedure Configure_Components
     (Scenario_Name : String)
   is
      Config : constant Tropos.Configuration :=
        Tropos.Reader.Read_Config
          (Path      => Scenario_Directory (Scenario_Name, "commodities"),
           Extension => "commodity");
   begin
      for Commodity_Config of Config loop
         declare
            Commodity : constant Commodity_Class :=
                          Get_By_Tag
                            (Commodity_Config.Config_Name);
         begin
            pragma Assert (Commodity.Has_Element);

            for Component_Config of Commodity_Config.Child ("component") loop
               declare
                  Input : constant Commodity_Class :=
                            Get_By_Tag (Component_Config.Config_Name);
                  Quantity : constant Non_Negative_Real :=
                               Component_Config.Value;
               begin
                  if not Input.Has_Element then
                     raise Constraint_Error with
                       "no such commodity " & Component_Config.Config_Name
                       & " in components for commodity "
                       & Commodity.Tag;
                  end if;

                  Concorde.Handles.Input_Commodity.Create
                    (Commodity => Commodity,
                     Input     => Input,
                     Quantity  => Concorde.Quantities.To_Quantity (Quantity));
               end;
            end loop;
         end;
      end loop;

   end Configure_Components;

   ---------------------------
   -- Configure_Constructed --
   ---------------------------

   procedure Configure_Constructed
     (Constructed : Concorde.Handles.Constructed.Constructed_Class;
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
               Commodity : constant Handles.Commodity.Commodity_Class :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Handles.Construction_Input.Create
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

   ------------------------------
   -- Configure_Initial_Prices --
   ------------------------------

   procedure Configure_Initial_Prices is
      use Concorde.Money;
      Changed : Boolean := True;
      Missing : Boolean := False;
   begin
      while Changed loop
         Changed := False;
         Missing := False;

         for Commodity of Concorde.Handles.Commodity.Scan_By_Top_Record loop
            if Commodity.Base_Price = Zero then
               declare
                  Component_Cost : Money_Type := Zero;
                  Valid          : Boolean := True;
               begin
                  for Component of
                    Concorde.Handles.Input_Commodity.Select_By_Commodity
                      (Commodity)
                  loop
                     if Component.Input.Base_Price = Zero then
                        Missing := True;
                        Valid   := False;
                        exit;
                     else
                        Component_Cost := Component_Cost
                          + Total (Component.Input.Base_Price,
                                   Component.Quantity);
                     end if;
                  end loop;

                  if Valid then
                     Commodity.Update_Commodity
                       .Set_Base_Price
                         (Price
                            (Adjust (Component_Cost, 2.0),
                             Concorde.Quantities.Unit))
                       .Done;
                     Changed := True;
                  end if;
               end;
            end if;
         end loop;
      end loop;

      if Missing then
         Concorde.Logging.Log
           ("error", "unable to determine initial commodity prices");
         for Commodity of Concorde.Handles.Commodity.Scan_By_Top_Record loop
            if Commodity.Base_Price = Zero then
               Concorde.Logging.Log ("error", Commodity.Tag);
            end if;
         end loop;
         raise Constraint_Error with
           "unable to determine initial commodity prices";
      end if;

   end Configure_Initial_Prices;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
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
               Commodity : constant Concorde.Commodities.Commodity_Class :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Handles.Stock_Item.Create
                 (Has_Stock => Has_Stock,
                  Commodity => Commodity,
                  Quantity  => Quantity,
                  Value     =>
                    Concorde.Money.Total (Commodity.Base_Price, Quantity));

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
     (Supplied : Concorde.Handles.Supplied.Supplied_Class;
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
               Commodity : constant Concorde.Commodities.Commodity_Class :=
                             Concorde.Commodities.Get
                               (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                             Concorde.Quantities.Scale
                               (Concorde.Quantities.To_Quantity
                                  (Real (Float'(Item_Config.Value))),
                                Factor);
            begin
               Concorde.Handles.Supply_Input.Create
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
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
      Config     : Tropos.Configuration)
   is
   begin
      Constraint.Update_Resource_Constraint
        .Set_Composition_Constraint (True)
        .Set_Composition
          (Concorde.Db.World_Composition'Value (Config.Value))
          .Done;
   end Constrain_Composition;

   ---------------------------
   -- Constrain_Hydrosphere --
   ---------------------------

   procedure Constrain_Hydrosphere
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
      Config     : Tropos.Configuration)
   is
   begin
      Constraint.Update_Resource_Constraint
        .Set_Hydrosphere_Constraint (True)
        .Set_Min_Hydrosphere (Real (Long_Float'(Config.Value)))
        .Done;
   end Constrain_Hydrosphere;

   --------------------
   -- Constrain_Life --
   --------------------

   procedure Constrain_Life
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
      Config     : Tropos.Configuration)
   is
   begin
      Constraint.Update_Resource_Constraint
        .Set_Life_Constraint (True)
        .Set_Min_Lifeforms
          (Concorde.Db.Life_Complexity_Type'Value
             (Config.Value))
        .Done;
   end Constrain_Life;

   --------------------
   -- Constrain_Mass --
   --------------------

   procedure Constrain_Mass
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
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
      Fuzzy_Ref         : constant Concorde.Handles.Fuzzy_Set
        .Fuzzy_Set_Handle :=
                           Concorde.Handles.Fuzzy_Set.Create
                             (Constraint_Range (1),
                              Constraint_Range (2),
                              Constraint_Range (3),
                              Constraint_Range (4));

   begin
      Constraint.Update_Resource_Constraint
        .Set_Mass_Constraint (True)
        .Set_Mass (Fuzzy_Ref)
        .Done;
   end Constrain_Mass;

   ---------------------------
   -- Constrain_Minimum_Age --
   ---------------------------

   procedure Constrain_Minimum_Age
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
      Config     : Tropos.Configuration)
   is
   begin
      Constraint.Update_Resource_Constraint
        .Set_Age_Constraint (True)
        .Set_Min_Age (Real (Long_Float'(Config.Value)))
        .Done;
   end Constrain_Minimum_Age;

   --------------------
   -- Constrain_Zone --
   --------------------

   procedure Constrain_Zone
     (Constraint : Concorde.Handles.Resource_Constraint
      .Resource_Constraint_Handle;
      Config     : Tropos.Configuration)
   is
   begin
      Constraint.Update_Resource_Constraint
        .Set_Zone_Constraint (True)
        .Set_Zone (Concorde.Db.Stellar_Orbit_Zone'Value (Config.Value))
          .Done;
   end Constrain_Zone;

   ----------------------------
   -- Create_Building_Module --
   ----------------------------

   function Create_Building_Module
     (Config : Tropos.Configuration)
      return Commodity_Handle
   is
      function Module_Class return Building_Module_Category;

      ------------------
      -- Module_Class --
      ------------------

      function Module_Class return Building_Module_Category is
         use Ada.Characters.Handling;
      begin
         for Class in Building_Module_Category loop
            if Config.Get (To_Lower (Class'Image)) then
               return Class;
            end if;
         end loop;
         raise Constraint_Error with
           "building module '" & Config.Config_Name & "': "
           & "no building module class found";
      end Module_Class;

      Module : constant Handles.Building_Module.Building_Module_Handle :=
                 Concorde.Handles.Building_Module.Create
                   (Mass       => Get_Mass (Config),
                    Base_Price => Get_Price (Config),
                    Transient  => False,
                    Tag        => Config.Config_Name,
                    Category   => Module_Class);

   begin
      return Module.To_Commodity_Handle;
   end Create_Building_Module;

   ----------------------
   -- Create_Commodity --
   ----------------------

   procedure Create_Commodity
     (Config : Tropos.Configuration)
   is
      Class_Name : constant String :=
        Config.Get ("class", "no class field");

   begin
      if Creator_Map.Is_Empty then
         Initialize_Creator_Map;
      end if;

      if not Creator_Map.Contains (Class_Name) then
         raise Constraint_Error with
           "don't know how to create commodity '"
           & Config.Config_Name
           & "'"
           & " with class '"
           & Class_Name
           & "'";
      end if;

      declare
         Commodity : constant Commodity_Handle :=
                       Creator_Map.Element (Class_Name) (Config);
         --  Reference : Commodity_Reference;
      begin
         pragma Unreferenced (Commodity);
         --  Commodity_Vector.Append (Commodity, Reference);
         --  Current.Insert (Commodity.Tag, Reference);
      end;

   end Create_Commodity;

   -----------------------
   -- Create_Components --
   -----------------------

   --  procedure Create_Components
   --    (Config : Tropos.Configuration)
   --  is
   --     Tag              : constant String := Config.Config_Name;
   --     Commodity        : Commodity_Handle'Class renames
   --       Current.Element (Tag);
   --     Component_Count : Natural := 0;
   --  begin
   --     for Component_Config of Config.Child ("component") loop
   --        declare
   --           Component : constant Commodity_Handle'Class :=
   --             Current.Element (Component_Config.Config_Name);
   --           Quantity  : constant Concorde.Quantities.Quantity_Type :=
   --             Concorde.Quantities.To_Quantity (Component_Config.Value);
   --        begin
   --           if Component = null then
   --              raise Constraint_Error with
   --                "component " & Component_Config.Config_Name
   --                & " not found in configuration for "
   --                & Config.Config_Name;
   --           end if;
   --
   --           Commodity.Components.Append
   --             ((Commodity_Type (Component), Quantity));
   --
   --           Component_Count := Component_Count + 1;
   --        end;
   --     end loop;
   --
   --     declare
   --        use Concorde.Money;
   --     begin
   --        if Commodity.Components.Is_Empty
   --          and then Commodity.Price = Zero
   --        then
   --           raise Constraint_Error with
   --             "commodity '" & Config.Config_Name
   --             & "' has neither price nor components";
   --        end if;
   --     end;
   --
   --  end Create_Components;

   --------------------------
   -- Create_Consumer_Good --
   --------------------------

   function Create_Consumer_Good
     (Config : Tropos.Configuration)
      return Commodity_Handle
   is
      function Get_Consumer_Class return Consumer_Class;

      ------------------------
      -- Get_Consumer_Class --
      ------------------------

      function Get_Consumer_Class return Consumer_Class is
         use Ada.Characters.Handling;
      begin
         for Class in Consumer_Class loop
            if Config.Get (To_Lower (Class'Image)) then
               return Class;
            end if;
         end loop;
         raise Constraint_Error with
           "consumer good '" & Config.Config_Name & "': "
           & "no consumer class found";
      end Get_Consumer_Class;

      Class : constant Consumer_Class := Get_Consumer_Class;

      use Concorde.Handles.Consumer_Commodity;

      Commodity : constant Consumer_Commodity_Handle :=
                    Create
                      (Mass       => Get_Mass (Config),
                       Base_Price => Get_Price (Config),
                       Transient  => False,
                       Tag        => Config.Config_Name,
                       Quality    =>
                         Quality_Type'Val (Config.Get ("quality") - 1),
                       Class      => Class,
                       Happiness  => Real (Happiness_Level (Class))
                       / Real (Happiness_Rating'Last),
                       Consumption =>
                         (case Class is
                             when Food       => 1.0,
                             when Drink      => 1.0,
                             when Intoxicant => 0.1,
                             when Clothing   => 0.05));
   begin
      return Commodity.To_Commodity_Handle;
   end Create_Consumer_Good;

   ---------------------------------
   -- Create_Frequency_Constraint --
   ---------------------------------

   procedure Create_Frequency_Constraint
     (Resource : Concorde.Handles.Resource.Resource_Handle;
      Config   : Tropos.Configuration)
   is
      Freq_Name  : constant Frequency_Type :=
                     Frequency_Type'Value (Config.Config_Name);
      Freq       : constant Normal_Value :=
                     Standard_Frequencies (Freq_Name);
      Constraint : constant Concorde.Handles.Resource_Constraint
        .Resource_Constraint_Handle :=
                     Concorde.Handles.Resource_Constraint.Create
                       (Resource               => Resource,
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
              & " in resource constraint for " & Resource.Tag;
         end if;
      end loop;
   end Create_Frequency_Constraint;

   ----------------------------
   -- Create_Industrial_Good --
   ----------------------------

   function Create_Industrial_Good
     (Config : Tropos.Configuration)
      return Commodity_Handle
   is
      use Concorde.Handles.Industrial_Commodity;

      function Get_Industrial_Class return Industrial_Class;

      ------------------------
      -- Get_Industrial_Class --
      ------------------------

      function Get_Industrial_Class return Industrial_Class is
         use Ada.Characters.Handling;
      begin
         for Class in Industrial_Class loop
            if Config.Get (To_Lower (Class'Image)) then
               return Class;
            end if;
         end loop;
         raise Constraint_Error with
           "industrial good '" & Config.Config_Name & "': "
           & "no industrial class found";
      end Get_Industrial_Class;

      Class : constant Industrial_Class := Get_Industrial_Class;

      Commodity  : constant Industrial_Commodity_Handle :=
                     Create
                       (Mass       => Get_Mass (Config),
                        Base_Price => Get_Price (Config),
                        Transient  => False,
                        Tag        => Config.Config_Name,
                        Class      => Class);
   begin
      return Commodity.To_Commodity_Handle;
   end Create_Industrial_Good;

   -----------------------
   -- Create_Pop_Groups --
   -----------------------

   procedure Create_Pop_Groups
     (Config : Tropos.Configuration)
   is
      function To_Quality_Type
        (Rank : Positive)
         return Concorde.Db.Quality_Type
      is (Concorde.Db.Quality_Type'Val (Rank - 1));

   begin
      for Pop_Group_Config of Config loop
         Concorde.Handles.Pop_Group.Create
           (Tag             => Pop_Group_Config.Config_Name,
            Mass            => 0.0,
            Base_Price      =>
              Concorde.Money.To_Price (Pop_Group_Config.Get ("salary")),
            Transient       => True,
            Consumer_Demand =>
              To_Quality_Type (Pop_Group_Config.Get ("consumer-quality")),
            Service_Demand  =>
              To_Quality_Type (Pop_Group_Config.Get ("service-quality")));
      end loop;
   end Create_Pop_Groups;

   ---------------------
   -- Create_Resource --
   ---------------------

   function Create_Resource
     (Config : Tropos.Configuration)
      return Commodity_Handle
   is
      function Get_Resource_Class return Resource_Category;

      ------------------------
      -- Get_Resource_Class --
      ------------------------

      function Get_Resource_Class return Resource_Category is
         use Ada.Characters.Handling;
      begin
         for Class in Resource_Category loop
            if Config.Get (To_Lower (Class'Image)) then
               return Class;
            end if;
         end loop;
         raise Constraint_Error with
           "resource '" & Config.Config_Name & "': "
           & "no resource category found";
      end Get_Resource_Class;

      Class : constant Resource_Category := Get_Resource_Class;

      Commodity  : constant Handles.Resource.Resource_Handle :=
                     Handles.Resource.Create
                       (Mass       => Get_Mass (Config),
                        Base_Price => Get_Price (Config),
                        Transient  => False,
                        Tag        => Config.Config_Name,
                        Category   => Class);
   begin

      for Deposit_Config of Config.Child ("deposits") loop
         if Deposit_Config.Config_Name = "sphere" then
            Create_Sphere_Constraint (Commodity, Deposit_Config);
         else
            Create_Frequency_Constraint (Commodity, Deposit_Config);
         end if;
      end loop;

      return Commodity.To_Commodity_Handle;
   end Create_Resource;

   ------------------------------
   -- Create_Service_Commodity --
   ------------------------------

   function Create_Service_Commodity
     (Config : Tropos.Configuration)
      return Commodity_Handle
   is
      use Concorde.Handles.Service_Commodity;

      function Get_Service_Class return Service_Category;

      ------------------------
      -- Get_Service_Class --
      ------------------------

      function Get_Service_Class return Service_Category is
         use Ada.Characters.Handling;
      begin
         for Class in Service_Category loop
            if Config.Get (To_Lower (Class'Image)) then
               return Class;
            end if;
         end loop;
         raise Constraint_Error with
           "service '" & Config.Config_Name & "': "
           & "no service category found";
      end Get_Service_Class;

      Class : constant Service_Category := Get_Service_Class;

      Commodity : constant Service_Commodity_Handle :=
                    Create
                      (Mass       => Get_Mass (Config),
                       Base_Price => Get_Price (Config),
                       Transient  => True,
                       Tag        => Config.Config_Name,
                       Quality    =>
                         Quality_Type'Val (Config.Get ("quality") - 1),
                       Class      => Class,
                       Happiness  => Real (Happiness_Level (Class))
                       / Real (Happiness_Rating'Last));
   begin
      return Commodity.To_Commodity_Handle;
   end Create_Service_Commodity;

   ------------------------------
   -- Create_Sphere_Constraint --
   ------------------------------

   procedure Create_Sphere_Constraint
     (Resource : Concorde.Handles.Resource.Resource_Handle;
      Config   : Tropos.Configuration)
   is
      function Get (Field : String;
                    Index : Natural := 0)
                    return Real
      is (if Index = 0
          then Real (Long_Float'(Config.Get (Field)))
          else Real (Long_Float'(Config.Child (Field).Get (Index))));

   begin
      Concorde.Handles.Resource_Constraint.Create
        (Resource               => Resource,
         Sphere_Constraint      => True,
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
      Add ("life", Constrain_Life'Access);
      Add ("mass", Constrain_Mass'Access);
      Add ("minimum-age", Constrain_Minimum_Age'Access);
      Add ("zone", Constrain_Zone'Access);
   end Initialize_Constraint_Arguments;

   ----------------------------
   -- Initialize_Creator_Map --
   ----------------------------

   procedure Initialize_Creator_Map is

      procedure Add
        (Name : String;
         Fn   : Commodity_Creator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name : String;
         Fn   : Commodity_Creator)
      is
      begin
         Creator_Map.Insert (Name, Fn);
      end Add;

   begin
      Add ("building-module", Create_Building_Module'Access);
      Add ("consumer-good", Create_Consumer_Good'Access);
      Add ("industrial-good", Create_Industrial_Good'Access);
      Add ("resource", Create_Resource'Access);
      Add ("service", Create_Service_Commodity'Access);
   end Initialize_Creator_Map;

end Concorde.Configure.Commodities;
