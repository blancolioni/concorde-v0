with Ada.Exceptions;

with Tropos.Reader;
with Concorde.Identifiers;

with Concorde.Db.Calculation;
with Concorde.Db.Effect;
with Concorde.Db.Node;
with Concorde.Db.Policy;

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
                    (Policy_Config.Get ("content", "rating"));
      Expense_Expr : constant String :=
                       Policy_Config.Get ("cost", "");
      Revenue_Expr : constant String :=
                       Policy_Config.Get ("revenue", "");

      Ref : constant Concorde.Db.Policy_Reference :=
              Concorde.Db.Policy.Create
                (Tag      => Policy_Config.Config_Name,
                 Content  => Content,
                 Expense  => Concorde.Db.Null_Calculation_Reference,
                 Revenue  => Concorde.Db.Null_Calculation_Reference);
      Node_Ref : constant Concorde.Db.Node_Reference :=
                   Concorde.Db.Policy.Get (Ref).Get_Node_Reference;

      Expense : constant Concorde.Db.Calculation_Reference :=
                  (if Expense_Expr = ""
                   then Concorde.Db.Null_Calculation_Reference
                   else Concorde.Db.Calculation.Create
                     (Identifier => Concorde.Identifiers.Next_Identifier,
                      Node       => Node_Ref,
                      Expression => Expense_Expr));
      Revenue : constant Concorde.Db.Calculation_Reference :=
                  (if Revenue_Expr = ""
                   then Concorde.Db.Null_Calculation_Reference
                   else Concorde.Db.Calculation.Create
                     (Identifier => Concorde.Identifiers.Next_Identifier,
                      Node       => Node_Ref,
                      Expression => Revenue_Expr));
   begin

      Concorde.Db.Policy.Update_Policy (Ref)
        .Set_Expense (Expense)
        .Set_Revenue (Revenue)
        .Done;

      for Effect_Config of Policy_Config.Child ("effect") loop

         declare
            use Concorde.Db;
            To             : constant Concorde.Db.Node_Reference :=
                               Concorde.Db.Node.Get_Reference_By_Tag
                                 (Effect_Config.Config_Name);
         begin
            if To = Null_Node_Reference then
               raise Constraint_Error with
                 "in configuration for policy "
                 & Policy_Config.Config_Name
                 & " effects: no such node: "
                 & Effect_Config.Config_Name;
            end if;
            Concorde.Db.Effect.Create
              (Identifier => Identifiers.Next_Identifier,
               Expression => Effect_Config.Value,
               Node       => Node_Ref,
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
