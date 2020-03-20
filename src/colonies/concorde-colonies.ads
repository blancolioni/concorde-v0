with Concorde.Db;

package Concorde.Colonies is

   procedure Daily_Policy_Expense
     (Colony  : Concorde.Db.Colony_Reference;
      Policy  : Concorde.Db.Policy_Reference);

   procedure Daily_Tax_Revenue
     (Colony  : Concorde.Db.Colony_Reference;
      Pop     : Concorde.Db.Pop_Reference);

end Concorde.Colonies;
