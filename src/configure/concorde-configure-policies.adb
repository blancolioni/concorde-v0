with Tropos.Reader;

with Concorde.Db.Calculation;
with Concorde.Db.Cost_Multiplier;
with Concorde.Db.Effect;
with Concorde.Db.Node;
with Concorde.Db.Policy;

package body Concorde.Configure.Policies is

   procedure Configure_Policy (Policy_Config : Tropos.Configuration);

   procedure New_Calculation
     (Ref    : Concorde.Db.Calculation_Reference;
      Config : Tropos.Configuration);

   ------------------------
   -- Configure_Policies --
   ------------------------

   procedure Configure_Policies (Scenario_Name : String) is
      Policies_Config : constant Tropos.Configuration :=
                          Tropos.Reader.Read_Config
                            (Scenario_File
                               (Scenario_Name  => Scenario_Name,
                                Directory_Name => "economy",
                                File_Name      => "policies.config"));
   begin
      for Policy_Config of Policies_Config loop
         Configure_Policy (Policy_Config);
      end loop;
   end Configure_Policies;

   ----------------------
   -- Configure_Policy --
   ----------------------

   procedure Configure_Policy (Policy_Config : Tropos.Configuration) is
      Cost_Config : constant Tropos.Configuration :=
                      Policy_Config.Child ("cost");

      Ref : constant Concorde.Db.Policy_Reference :=
              Concorde.Db.Policy.Create
                (Tag     => Policy_Config.Config_Name,
                 Content => Concorde.Db.Rating,
                 Min_Cost =>
                   Real (Long_Float'(Cost_Config.Get ("low", 0.0))),
                 Max_Cost =>
                   Real (Long_Float'(Cost_Config.Get ("high", 0.0))));

      Node_Ref : constant Concorde.Db.Node_Reference :=
                   Concorde.Db.Policy.Get (Ref).Get_Node_Reference;
   begin
      for Multiplier_Config of Cost_Config.Child ("multipliers") loop
         declare
            use Concorde.Db;
            Multiplier : constant Cost_Multiplier_Reference :=
                           Cost_Multiplier.Create
                             (Add      => 0.0,
                              Multiply => 0.0,
                              Exponent => 0.0,
                              Inertia  => 0.0,
                              Node     =>
                                Concorde.Db.Node.Get_Reference_By_Tag
                                  (Multiplier_Config.Config_Name),
                              Policy   => Ref);
         begin
            New_Calculation
              (Cost_Multiplier.Get (Multiplier).Get_Calculation_Reference,
               Multiplier_Config);
         end;
      end loop;

      for Effect_Config of Policy_Config.Child ("effect") loop

         declare
            use type Concorde.Db.Node_Reference;
            To             : constant Concorde.Db.Node_Reference :=
                               Concorde.Db.Node.Get_Reference_By_Tag
                                 (Effect_Config.Config_Name);
            Add      : Real := 0.0;
            Multiply : Real := 0.0;
            Exponent : Real := 1.0;
            Inertia  : Real := 0.0;

            function Get (Name : String) return Real
            is (Real (Float'(Effect_Config.Get (Name, 0.0))));

            procedure Get (X, Y, Z, W : in out Real);

            ---------
            -- Get --
            ---------

            procedure Get (X, Y, Z, W : in out Real) is

               procedure Get
                 (Index : Positive;
                  Value : in out Real);

               ---------
               -- Get --
               ---------

               procedure Get
                 (Index : Positive;
                  Value : in out Real)
               is
               begin
                  if Index <= Effect_Config.Child_Count then
                     Value := Real (Long_Float'(Effect_Config.Get (Index)));
                  end if;
               end Get;

            begin
               Get (1, X);
               Get (2, Y);
               Get (3, Z);
               Get (4, W);
            end Get;

         begin
            if To = Concorde.Db.Null_Node_Reference then
               raise Constraint_Error with
                 "in policy " & Policy_Config.Config_Name
                 & ": no such node: " & Effect_Config.Config_Name;
            end if;

            if Effect_Config.Child_Count = 1 then
               Multiply := Real (Float'(Effect_Config.Value));
            elsif Effect_Config.Contains ("add")
              or else Effect_Config.Contains ("multiply")
              or else Effect_Config.Contains ("exponent")
              or else Effect_Config.Contains ("inertia")
            then
               Add      := Get ("add");
               Multiply := Get ("multiply");
               Exponent := Get ("exponent");
               Inertia  := Get ("inertia");
            else
               Get (Add, Multiply, Exponent, Inertia);
            end if;

            Concorde.Db.Effect.Create
              (To             => To,
               Node           => Node_Ref,
               Add            => Add,
               Multiply       => Multiply,
               Exponent       => Exponent,
               Inertia        => Inertia);
         end;
      end loop;

   end Configure_Policy;

   ---------------------
   -- New_Calculation --
   ---------------------

   procedure New_Calculation
     (Ref    : Concorde.Db.Calculation_Reference;
      Config : Tropos.Configuration)
   is
      Add      : Real := 0.0;
      Multiply : Real := 1.0;
      Exponent : Real := 1.0;
      Inertia  : Real := 0.0;

      function Get (Name : String;
                    Default : Real := 0.0)
                    return Real
      is (Real (Config.Get (Name, Long_Float (Default))));

      procedure Get (X, Y, Z, W : in out Real);

      ---------
      -- Get --
      ---------

      procedure Get (X, Y, Z, W : in out Real) is

         procedure Get
           (Index : Positive;
            Value : in out Real);

         ---------
         -- Get --
         ---------

         procedure Get
           (Index : Positive;
            Value : in out Real)
         is
         begin
            if Index <= Config.Child_Count then
               Value := Real (Long_Float'(Config.Get (Index)));
            end if;
         end Get;

      begin
         Get (1, X);
         Get (2, Y);
         Get (3, Z);
         Get (4, W);
      end Get;

   begin
      if Config.Contains ("add")
        or else Config.Contains ("multiply")
        or else Config.Contains ("exponent")
        or else Config.Contains ("inertia")
      then
         Add      := Get ("add", 0.0);
         Multiply := Get ("multiply", 1.0);
         Exponent := Get ("exponent", 1.0);
         Inertia  := Get ("inertia", 0.0);
      else
         Get (Add, Multiply, Exponent, Inertia);
      end if;

      Concorde.Db.Calculation.Update_Calculation (Ref)
        .Set_Add (Add)
        .Set_Multiply (Multiply)
        .Set_Exponent (Exponent)
        .Set_Inertia (Inertia)
        .Done;
   end New_Calculation;

end Concorde.Configure.Policies;
