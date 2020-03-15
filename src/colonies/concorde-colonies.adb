with Concorde.Elementary_Functions;
with Concorde.Money;
with Concorde.Real_Images;

with Concorde.Agents;
with Concorde.Factions;
with Concorde.Worlds;

with Concorde.Network;

with Concorde.Db.Colony;
with Concorde.Db.Colony_Policy;
with Concorde.Db.Cost_Multiplier;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Pop_Group_Member;

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

            Concorde.Db.Colony_Policy.Update_Colony_Policy
              (Concorde.Db.Colony_Policy.Get_Reference_By_Colony_Policy
                 (Colony, Policy))
                .Set_Expense (Amount)
              .Done;
            Concorde.Agents.Remove_Cash
              (Colony_Rec, Amount, Policy_Tag);
         end;
      end if;
   end Daily_Policy_Expense;

   -----------------------
   -- Daily_Tax_Revenue --
   -----------------------

   procedure Daily_Tax_Revenue
     (Colony  : Concorde.Db.Colony_Reference;
      Pop     : Concorde.Db.Pop_Reference)
   is

      function Image (X : Real) return String
                      renames Concorde.Real_Images.Approximate_Image;

      Pop_Rec   : constant Concorde.Db.Pop.Pop_Type :=
                    Concorde.Db.Pop.Get (Pop);
      Rec       : constant Concorde.Db.Colony.Colony_Type :=
                    Concorde.Db.Colony.Get (Colony);
      Network   : constant Concorde.Db.Network_Reference :=
                    Rec.Get_Network_Reference;

      Wealth_Group : constant Concorde.Db.Pop_Group.Pop_Group_Type :=
                       Concorde.Db.Pop_Group.Get (Pop_Rec.Wealth_Group);

      function Current (Suffix : String) return Real
      is (Concorde.Network.Current_Value
          (Network,
           Wealth_Group.Tag
           & (if Suffix = "" then "" else "-" & Suffix)));

      function Income_Adjustments return Real;

      ------------------------
      -- Income_Adjustments --
      ------------------------

      function Income_Adjustments return Real is
      begin
         return Adj : Real := 0.0 do
            for Membership of
              Concorde.Db.Pop_Group_Member.Select_By_Pop (Pop)
            loop
               Adj := Adj
                 + Concorde.Network.Current_Value
                 (Network,
                  Concorde.Db.Pop_Group.Get (Membership.Pop_Group).Tag
                  & "-income");
            end loop;
         end return;
      end Income_Adjustments;

      Base_Income : constant Real := Current ("base-income");
      Wages     : constant Signed_Unit_Real :=
                    Concorde.Network.Current_Value (Network, "wages");
      Final_Income : constant Real :=
                       Base_Income * (1.0 + Wages + Income_Adjustments);
      Total_Taxable : constant Concorde.Money.Money_Type :=
                        Concorde.Money.To_Money
                          (Final_Income * Pop_Rec.Size);
      Rate      : constant Unit_Real :=
                    Current ("income-tax-rate");
      Evasion : constant Unit_Real := Current ("tax-evasion");
      Revenue   : constant Concorde.Money.Money_Type :=
                    Concorde.Money.Adjust
                      (Total_Taxable,
                       Rate * (1.0 - Evasion));
   begin
      Concorde.Logging.Log
        (Actor    =>
           Concorde.Factions.Name (Rec.Faction),
         Location =>
           Concorde.Worlds.Name (Rec.World),
         Category => "tax",
         Message  => "group="
         & Wealth_Group.Tag
         & "; size=" & Image (Pop_Rec.Size)
         & "; rate=" & Image (Rate * 100.0) & "%"
         & "; income="
         & Concorde.Money.Show
           (Concorde.Money.To_Money (Final_Income))
         & "; evasion=" & Image (Evasion * 100.0) & "%"
         & "; revenue=" & Concorde.Money.Show (Revenue));

      Concorde.Agents.Add_Cash
        (Concorde.Db.Colony.Get (Colony),
         Revenue, "income-tax");

      declare
         use Concorde.Money;
         use Concorde.Db.Colony_Policy;
         Policy : constant Colony_Policy_Type :=
                    Get_By_Colony_Policy
                      (Colony,
                       Concorde.Db.Policy.Get_Reference_By_Tag
                         (Wealth_Group.Tag & "-income-tax-rate"));
      begin
         Update_Colony_Policy (Policy.Get_Colony_Policy_Reference)
           .Set_Revenue (Policy.Revenue + Revenue)
           .Done;
      end;

   end Daily_Tax_Revenue;

end Concorde.Colonies;
