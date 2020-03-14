with Concorde.Elementary_Functions;
with Concorde.Money;
with Concorde.Real_Images;

with Concorde.Agents;
with Concorde.Factions;
with Concorde.Worlds;

with Concorde.Network;

with Concorde.Db.Colony;
with Concorde.Db.Colony_Pop_Group;
with Concorde.Db.Cost_Multiplier;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Pop_Group;

with Concorde.Logging;

package body Concorde.Colonies is

   Detailed_Costs : constant Boolean := False;

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   --------------------------
   -- Daily_Policy_Expense --
   --------------------------

   procedure Daily_Policy_Expense
     (Colony  : Concorde.Db.Colony_Reference;
      Policy  : Concorde.Db.Policy_Reference;
      Value   : Unit_Real)
   is
      Policy_Rec : constant Concorde.Db.Policy.Policy_Type :=
                     Concorde.Db.Policy.Get (Policy);
      Colony_Rec : constant Concorde.Db.Colony.Colony_Type :=
                     Concorde.Db.Colony.Get (Colony);
      Policy_Tag : constant String := Policy_Rec.Tag;
      Network : constant Concorde.Db.Network_Reference :=
                     Colony_Rec.Get_Network_Reference;
      Expense : Real :=
                  (Policy_Rec.Max_Cost - Policy_Rec.Min_Cost)
                  * Value
                  + Policy_Rec.Min_Cost;
   begin
      for Multiplier of
        Concorde.Db.Cost_Multiplier.Select_By_Policy (Policy)
      loop
         declare
            use Concorde.Elementary_Functions;
            M_Tag : constant String :=
                      Concorde.Db.Node.Get (Multiplier.Node).Tag;
            X     : constant Real :=
                      Concorde.Network.Inertial_Value
                        (Network, M_Tag, Multiplier.Inertia);
            M     : constant Real :=
                      (X ** Multiplier.Exponent) * Multiplier.Multiply
                      + Multiplier.Add;
         begin

            if Detailed_Costs then
               Concorde.Logging.Log
                 (Actor    =>
                    Concorde.Factions.Name (Colony_Rec.Faction),
                  Location =>
                    Concorde.Worlds.Name (Colony_Rec.World),
                  Category => "expense",
                  Message  =>
                    Policy_Tag
                  & ": multiplier node: "
                  & M_Tag
                  & ": add=" & Image (Multiplier.Add)
                  & "; mul=" & Image (Multiplier.Multiply)
                  & "; exp=" & Image (Multiplier.Exponent)
                  & "; value=" & Image (X)
                  & "; multiplier=" & Image (M)
                  & "; old expense=" & Image (Expense)
                  & "; new expense=" & Image (Expense * (1.0 + M)));
            end if;

            Expense := Expense * (1.0 + M);
         end;
      end loop;

      if Expense > 0.0 then
         declare
            Amount : constant Concorde.Money.Money_Type :=
                       Concorde.Money.To_Money (Expense);

         begin
            Concorde.Logging.Log
              (Actor    =>
                 Concorde.Factions.Name (Colony_Rec.Faction),
               Location =>
                 Concorde.Worlds.Name (Colony_Rec.World),
               Category => "expense",
               Message  => Policy_Tag & " costs "
               & Concorde.Money.Show (Amount));

            Concorde.Agents.Remove_Cash
              (Colony_Rec, Amount, Policy_Tag);
         end;
      end if;
   end Daily_Policy_Expense;

   -----------------------
   -- Daily_Tax_Revenue --
   -----------------------

   procedure Daily_Tax_Revenue
     (Colony : Concorde.Db.Colony_Reference;
      Group  : Concorde.Db.Pop_Group_Reference;
      Rate   : Unit_Real;
      Income  : Signed_Unit_Real;
      Evasion : Unit_Real)
   is
      use Concorde.Db;

      function Image (X : Real) return String
                      renames Concorde.Real_Images.Approximate_Image;

      Col_Group : constant Colony_Pop_Group.Colony_Pop_Group_Type :=
                    Colony_Pop_Group.Get_By_Colony_Pop_Group
                      (Colony, Group);
      Revenue   : constant Concorde.Money.Money_Type :=
                    Concorde.Money.Adjust
                      (Col_Group.Income,
                       Real (Col_Group.Size)
                       * Rate * (Income + 1.0) * (1.0 - Evasion));
      Rec       : constant Concorde.Db.Colony.Colony_Type :=
                    Concorde.Db.Colony.Get (Colony);
   begin
      Concorde.Logging.Log
        (Actor    =>
           Concorde.Factions.Name (Rec.Faction),
         Location =>
           Concorde.Worlds.Name (Rec.World),
         Category => "tax",
         Message  => "group=" & Concorde.Db.Pop_Group.Get (Group).Tag
         & "; size=" & Col_Group.Size'Image
         & "; rate=" & Image (Rate * 100.0) & "%"
         & "; income=" & Concorde.Money.Show (Col_Group.Income)
         & " " & Image ((Income + 1.0) * 100.0) & "%"
         & "; evasion=" & Image (Evasion * 100.0) & "%"
         & "; revenue=" & Concorde.Money.Show (Revenue));

      Concorde.Agents.Add_Cash
        (Concorde.Db.Colony.Get (Colony),
         Revenue, "income-tax");
   end Daily_Tax_Revenue;

end Concorde.Colonies;
