with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db;

package Concorde.Employment is

   procedure Create_Employment_Contract
     (Employer : Concorde.Handles.Agent.Agent_Class;
      Employee : Concorde.Handles.Agent.Agent_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type);

end Concorde.Employment;
