with Concorde.Db;

package Concorde.Colonies is

   procedure Daily_Policy_Expense
     (Colony  : Concorde.Db.Colony_Reference;
      Policy  : Concorde.Db.Policy_Reference;
      Value   : Unit_Real);

   procedure Daily_Tax_Revenue
     (Colony  : Concorde.Db.Colony_Reference;
      Group   : Concorde.Db.Pop_Group_Reference;
      Rate    : Unit_Real;
      Income  : Signed_Unit_Real;
      Evasion : Unit_Real);

end Concorde.Colonies;
