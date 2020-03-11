with Tropos.Reader;

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

   procedure Configure_Policy (Policy_Config : Tropos.Configuration) is
      Ref : constant Concorde.Db.Policy_Reference :=
              Concorde.Db.Policy.Create
                (Tag     => Policy_Config.Config_Name,
                 Content => Concorde.Db.Rating);
      Node_Ref : constant Concorde.Db.Node_Reference :=
                   Concorde.Db.Policy.Get (Ref).Get_Node_Reference;
   begin
      for Effect_Config of Policy_Config loop
         declare
            use type Concorde.Db.Node_Reference;
            To             : constant Concorde.Db.Node_Reference :=
                               Concorde.Db.Node.Get_Reference_By_Tag
                                 (Effect_Config.Config_Name);
            Add            : Real := 0.0;
            Multiply       : Real := 0.0;
            Exponent       : Real := 0.0;
            Implementation : Real := 0.0;

            function Get (Name : String) return Real
            is (Real (Float'(Effect_Config.Get (Name, 0.0))));

         begin
            if To = Concorde.Db.Null_Node_Reference then
               raise Constraint_Error with
                 "in policy " & Policy_Config.Config_Name
                 & ": no such node: " & Effect_Config.Config_Name;
            end if;

            if Effect_Config.Child_Count = 1 then
               Multiply := Real (Float'(Effect_Config.Value));
            else
               Add := Get ("add");
               Multiply := Get ("multiply");
               Exponent := Get ("exponent");
               Implementation := Get ("delay");
            end if;

            Concorde.Db.Effect.Create
              (To             => To,
               From           => Node_Ref,
               Add            => Add,
               Multiply       => Multiply,
               Exponent       => Exponent,
               Implementation => Implementation);
         end;
      end loop;

   end Configure_Policy;

end Concorde.Configure.Policies;
