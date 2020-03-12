with Ada.Text_IO;

with Concorde.Money;
with Concorde.Real_Images;

with Concorde.Agents;

with Concorde.Db.Colony;
with Concorde.Db.Colony_Pop_Group;
with Concorde.Db.Pop_Group;

package body Concorde.Colonies is

   ---------------
   -- Daily_Tax --
   ---------------

   procedure Daily_Tax
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
   begin
      Ada.Text_IO.Put_Line
        ("tax: group=" & Concorde.Db.Pop_Group.Get (Group).Tag
         & "; size=" & Col_Group.Size'Image
         & "; rate=" & Image (Rate * 100.0) & "%"
         & "; income=" & Concorde.Money.Show (Col_Group.Income)
         & " " & Image ((Income + 1.0) * 100.0) & "%"
         & "; evasion=" & Image (Evasion * 100.0) & "%"
         & "; revenue=" & Concorde.Money.Show (Revenue));
      Concorde.Agents.Add_Cash
        (Concorde.Db.Colony.Get (Colony),
         Revenue, "income-tax");
   end Daily_Tax;

end Concorde.Colonies;
