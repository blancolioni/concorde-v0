with Ada.Exceptions;

with Tropos.Reader;

with Concorde.Db.Calculation;
with Concorde.Db.Child_Calculation;
with Concorde.Db.Cost_Multiplier;
with Concorde.Db.Effect;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Value_Multiplier;

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

      Content : constant Concorde.Db.Node_Value_Type :=
                  Concorde.Db.Node_Value_Type'Value
                    (Policy_Config.Get ("content", "rating"));
      Ref : constant Concorde.Db.Policy_Reference :=
              Concorde.Db.Policy.Create
                (Tag      => Policy_Config.Config_Name,
                 Content  => Content,
                 Internal => Policy_Config.Get ("internal"),
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
            Value_Node : constant Node_Reference :=
                           Concorde.Db.Node.Get_Reference_By_Tag
                             (Multiplier_Config.Config_Name);

            Multiplier : constant Cost_Multiplier_Reference :=
                           Cost_Multiplier.Create
                             (Is_Sum   => False,
                              Add      => 0.0,
                              Multiply => 1.0,
                              Exponent => 1.0,
                              Inertia  => 0.0,
                              Node     => Value_Node,
                              Policy   => Ref);
         begin
            if Value_Node = Null_Node_Reference then
               raise Constraint_Error with
                 "in configuration for policy "
                 & Policy_Config.Config_Name
                 & " cost multipliers: no such node: "
                 & Multiplier_Config.Config_Name;
            end if;

            New_Calculation
              (Cost_Multiplier.Get (Multiplier).Get_Calculation_Reference,
               Multiplier_Config);
         end;
      end loop;

      for Effect_Config of Policy_Config.Child ("effect") loop

         declare
            use Concorde.Db;
            To             : constant Concorde.Db.Node_Reference :=
                               Concorde.Db.Node.Get_Reference_By_Tag
                                 (Effect_Config.Config_Name);
            Effect         : constant Effect_Reference :=
                               Concorde.Db.Effect.Create
                                 (Is_Sum   => False,
                                  Add      => 0.0,
                                  Multiply => 1.0,
                                  Exponent => 1.0,
                                  Inertia  => 0.0,
                                  Node     => Node_Ref,
                                  To       => To);
         begin
            if To = Null_Node_Reference then
               raise Constraint_Error with
                 "in configuration for policy "
                 & Policy_Config.Config_Name
                 & " effects: no such node: "
                 & Effect_Config.Config_Name;
            end if;
            New_Calculation
              (Concorde.Db.Effect.Get (Effect).Get_Calculation_Reference,
               Effect_Config);
         end;
      end loop;

      for Value_Config of Policy_Config.Child ("value") loop
         declare
            use Concorde.Db;
            Value : constant Value_Multiplier_Reference :=
                      Value_Multiplier.Create
                        (Is_Sum   => False,
                         Add      => 0.0,
                         Multiply => 1.0,
                         Exponent => 1.0,
                         Inertia  => 0.0,
                         Node     => Concorde.Db.Null_Node_Reference,
                         Policy   => Ref);
         begin
            New_Calculation
              (Value_Multiplier.Get (Value).Get_Calculation_Reference,
               Value_Config);
         end;
      end loop;

   exception

      when E : others =>
         raise Constraint_Error with
           "in configuration for policy "
           & Policy_Config.Config_Name
           & ": "
           & Ada.Exceptions.Exception_Message (E);

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
      if Config.Config_Name = "add"
        or else Config.Config_Name = "multiply"
      then
         for Child of Config loop
            declare
               use Concorde.Db, Concorde.Db.Calculation;
               Calculation : constant Calculation_Reference :=
                               Create
                                 (Node     => Concorde.Db.Null_Node_Reference,
                                  Is_Sum   => False,
                                  Add      => 0.0,
                                  Multiply => 1.0,
                                  Exponent => 1.0,
                                  Inertia  => 0.0);
            begin
               New_Calculation (Calculation, Child);
               Concorde.Db.Child_Calculation.Create
                 (Ref, Calculation);
            end;
         end loop;

         if Config.Config_Name = "add" then
            Concorde.Db.Calculation.Update_Calculation (Ref)
              .Set_Is_Sum (True)
              .Done;
         end if;
      else
         declare
            use Concorde.Db;
            Value_Node : constant Node_Reference :=
                           Concorde.Db.Node.Get_Reference_By_Tag
                             (Config.Config_Name);
         begin

            if Value_Node = Null_Node_Reference then
               raise Constraint_Error with
                 "no such node: "
                 & Config.Config_Name;
            end if;

            if Config.Contains ("add")
              or else Config.Contains ("multiply")
              or else Config.Contains ("exponent")
              or else Config.Contains ("inertia")
            then
               Add      := Get ("add", 0.0);
               Multiply := Get ("multiply", 1.0);
               Exponent := Get ("exponent", 1.0);
               Inertia  := Get ("inertia", 0.0);
            elsif Config.Child_Count = 1 then
               Multiply := Real (Long_Float'(Config.Value));
            elsif Config.Child_Count = 2 then
               Add := Real (Long_Float'(Config.Get (1)));
               Multiply := Real (Long_Float'(Config.Get (2)));
            else
               Get (Add, Multiply, Exponent, Inertia);
            end if;

            Concorde.Db.Calculation.Update_Calculation (Ref)
              .Set_Node (Value_Node)
              .Set_Add (Add)
              .Set_Multiply (Multiply)
              .Set_Exponent (Exponent)
              .Set_Inertia (Inertia)
              .Done;
         end;
      end if;
   end New_Calculation;

end Concorde.Configure.Policies;
