with WL.String_Maps;

with Concorde.Money;
with Concorde.Real_Images;

with Concorde.Agents;

with Concorde.Network;
with Concorde.Nodes;

with Concorde.Handles.Pop_Group_Member;

with Concorde.Handles.Colony_Policy;
with Concorde.Handles.Pop_Group;

with Concorde.Logging;

package body Concorde.Colonies is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image
     with Unreferenced;

   package Value_Handle_Maps is
     new WL.String_Maps (Concorde.Nodes.Value_Handle, Concorde.Nodes."=");

   protected Value_Handle_Store is

      procedure Get (Colony : Concorde.Handles.Colony.Colony_Class;
                     Tag    : String;
                     Value  : out Real);

   private
      Map : Value_Handle_Maps.Map;
   end Value_Handle_Store;

   function Current_Value
     (Colony : Concorde.Handles.Colony.Colony_Class;
      Tag    : String)
      return Real;

   function Current_Value
     (Colony : Concorde.Handles.Colony.Colony_Class;
      Tag    : String)
      return Real
   is
      Value : Real;
   begin
      Value_Handle_Store.Get (Colony, Tag, Value);
      return Value;
   end Current_Value;

   -----------------------
   -- Daily_Tax_Revenue --
   -----------------------

   procedure Daily_Tax_Revenue
     (Colony  : Concorde.Handles.Colony.Colony_Class;
      Pop     : Concorde.Handles.Pop.Pop_Class)
   is

      Wealth_Group : constant Concorde.Handles.Pop_Group.Pop_Group_Class :=
                       Pop.Wealth_Group;

      function Current (Suffix : String) return Real
      is (Current_Value
          (Colony,
           Wealth_Group.Tag
           & (if Suffix = "" then "" else "-" & Suffix)));

      function Income_Adjustments return Real;

      ------------------------
      -- Income_Adjustments --
      ------------------------

      function Income_Adjustments return Real is
         use Concorde.Handles.Pop_Group_Member;
      begin
         return Adj : Real := 0.0 do
            for Membership of
              Concorde.Handles.Pop_Group_Member.Select_By_Pop (Pop)
            loop
               declare
                  Tag : constant String :=
                          Membership.Pop_Group.Tag & "-income";
                  Income : constant Real :=
                             Current_Value (Colony, Tag);
               begin
                  Adj := Adj + Income;
               end;
            end loop;
         end return;
      end Income_Adjustments;

      Base_Income : constant Real := Current ("base-income");
      Wages       : constant Signed_Unit_Real :=
                      Current_Value (Colony, "wages");
      Final_Income : constant Real :=
                       Base_Income * (1.0 + Wages + Income_Adjustments);
      Total_Taxable : constant Concorde.Money.Money_Type :=
                        Concorde.Money.To_Money
                          (Final_Income * Pop.Size);
      Rate      : constant Unit_Real :=
                    Current ("income-tax-rate");
      Evasion : constant Unit_Real := Current ("tax-evasion");
      Revenue   : constant Concorde.Money.Money_Type :=
                    Concorde.Money.Adjust
                      (Total_Taxable,
                       Rate * (1.0 - Evasion));
   begin

      Concorde.Agents.Add_Cash
        (Colony, Revenue, "income-tax");

      declare
         use Concorde.Money;
         use Concorde.Handles.Colony_Policy;
         Policy : constant Colony_Policy_Handle :=
                    Get_By_Colony_Policy
                      (Colony,
                       Concorde.Handles.Policy.Get_By_Tag
                         (Wealth_Group.Tag & "-income-tax-rate"));
      begin
         Policy.Update_Colony_Policy
           .Set_Revenue (Policy.Revenue + Revenue)
           .Done;
      end;

   end Daily_Tax_Revenue;

   --------------------------
   -- Execute_Daily_Policy --
   --------------------------

   procedure Execute_Daily_Policy
     (Colony  : Concorde.Handles.Colony.Colony_Class;
      Policy  : Concorde.Handles.Policy.Policy_Class)
   is
      Policy_Tag : constant String := Policy.Tag;
      Expense    : constant Real :=
                     (if not Policy.Expense.Has_Element
                      then 0.0
                      else Concorde.Network.Evaluate
                        (Network     => Colony,
                         Calculation => Policy.Expense));
      Revenue    : constant Real :=
                     (if not Policy.Revenue.Has_Element
                      then 0.0
                      else Concorde.Network.Evaluate
                        (Network     => Colony,
                         Calculation => Policy.Revenue));
   begin
      if Expense > 0.0 then
         declare
            Amount : constant Concorde.Money.Money_Type :=
                       Concorde.Money.To_Money (Expense);

         begin
            Concorde.Logging.Log
              (Actor    => Colony.Faction.Name,
               Location => Colony.World.Name,
               Category => "expense",
               Message  => Policy_Tag & " costs "
               & Concorde.Money.Show (Amount));

            Concorde.Handles.Colony_Policy.Update_Colony_Policy
              (Concorde.Handles.Colony_Policy.Get_By_Colony_Policy
                 (Colony, Policy))
                .Set_Expense (Amount)
              .Done;
            Concorde.Agents.Remove_Cash
              (Colony, Amount, Policy_Tag);
         end;
      end if;
      if Revenue > 0.0 then
         declare
            Amount : constant Concorde.Money.Money_Type :=
                       Concorde.Money.To_Money (Revenue);

         begin
            Concorde.Logging.Log
              (Actor    => Colony.Faction.Name,
               Location => Colony.World.Name,
               Category => "revenue",
               Message  => Policy_Tag & " earns "
               & Concorde.Money.Show (Amount));

            Concorde.Handles.Colony_Policy.Update_Colony_Policy
              (Concorde.Handles.Colony_Policy.Get_By_Colony_Policy
                 (Colony, Policy))
                .Set_Revenue (Amount)
              .Done;
            Concorde.Agents.Add_Cash
              (Colony, Amount, Policy_Tag);
         end;
      end if;
   end Execute_Daily_Policy;

   ------------------------
   -- Value_Handle_Store --
   ------------------------

   protected body Value_Handle_Store is

      ---------
      -- Get --
      ---------

      procedure Get (Colony : Concorde.Handles.Colony.Colony_Class;
                     Tag    : String;
                     Value  : out Real)
      is
         use Value_Handle_Maps;
         Key      : constant String :=
                      Colony.Identifier & "-" & Tag;
         Position : Cursor := Map.Find (Key);
      begin
         if not Has_Element (Position) then
            declare
               Handle   : constant Concorde.Nodes.Value_Handle :=
                            Concorde.Network.Get_Handle (Colony, Tag);
               Inserted : Boolean;
            begin
               Map.Insert
                 (Key      => Key,
                  New_Item => Handle,
                  Position => Position,
                  Inserted => Inserted);
               pragma Assert (Inserted);
            end;
         end if;

         pragma Assert (Has_Element (Position));

         Value := Concorde.Nodes.Current_Value (Element (Position));

      end Get;

   end Value_Handle_Store;

end Concorde.Colonies;
