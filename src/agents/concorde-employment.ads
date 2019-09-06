with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db;

package Concorde.Employment is

   procedure Create_Employment_Contract
     (Employer : Concorde.Db.Agent_Reference;
      Employee : Concorde.Db.Agent_Reference;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type);

end Concorde.Employment;
