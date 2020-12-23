with Ada.Exceptions;

with Tropos.Reader;
with Concorde.Identifiers;

with Concorde.Handles.Calculation;
with Concorde.Handles.Effect;
with Concorde.Handles.Node;
with Concorde.Handles.Policy;

with Concorde.Db;

package body Concorde.Configure.Policies is

   procedure Configure_Policy (Policy_Config : Tropos.Configuration);

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
      Content : constant Concorde.Db.Node_Value_Type :=
                  Concorde.Db.Node_Value_Type'Value
                    (Policy_Config.Get ("content", "setting"));
      Expense_Expr : constant String :=
                       Policy_Config.Get ("cost", "");
      Revenue_Expr : constant String :=
                       Policy_Config.Get ("revenue", "");

      Policy : constant Concorde.Handles.Policy.Policy_Handle :=
                 Concorde.Handles.Policy.Create
                   (Tag        => Policy_Config.Config_Name,
                    Identifier => Concorde.Identifiers.Next_Identifier,
                    Content    => Content,
                    Expense    => Concorde.Handles.Calculation.Empty_Handle,
                    Revenue    => Concorde.Handles.Calculation.Empty_Handle);
      Expense : constant Concorde.Handles.Calculation.Calculation_Handle :=
                  (if Expense_Expr = ""
                   then Concorde.Handles.Calculation.Empty_Handle
                   else Concorde.Handles.Calculation.Create
                     (Identifier => Concorde.Identifiers.Next_Identifier,
                      Node       => Policy,
                      Expression => Expense_Expr));
      Revenue : constant Concorde.Handles.Calculation.Calculation_Handle :=
                  (if Revenue_Expr = ""
                   then Concorde.Handles.Calculation.Empty_Handle
                   else Concorde.Handles.Calculation.Create
                     (Identifier => Concorde.Identifiers.Next_Identifier,
                      Node       => Policy,
                      Expression => Revenue_Expr));
   begin

      Policy.Update_Policy
        .Set_Expense (Expense)
        .Set_Revenue (Revenue)
        .Done;

      for Effect_Config of Policy_Config.Child ("effect") loop

         declare
            To             : constant Concorde.Handles.Node.Node_Class :=
                               Concorde.Handles.Node.Get_By_Tag
                                 (Effect_Config.Config_Name);
         begin
            if not To.Has_Element then
               raise Constraint_Error with
                 "in configuration for policy "
                 & Policy_Config.Config_Name
                 & " effects: no such node: "
                 & Effect_Config.Config_Name;
            end if;

            Concorde.Handles.Calculation.Create
              (Identifier => Concorde.Identifiers.Next_Identifier,
               Node       => To,
               Expression => Effect_Config.Value);

            Concorde.Handles.Effect.Create
              (Identifier => Identifiers.Next_Identifier,
               Expression => Effect_Config.Value,
               Node       => Policy,
               To         => To);
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

end Concorde.Configure.Policies;
