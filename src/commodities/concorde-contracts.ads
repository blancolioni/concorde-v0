with Concorde.Commodities;
with Concorde.Money;

with Concorde.Db;

package Concorde.Contracts is

   type Contract_Type is private;


   function Find_Contract
     (Test : not null access
        function (Contract : Contract_Type) return Boolean)
      return Contract_Type;

   function Create_Rental_Contract
     (Rental_Item : Concorde.Commodities.Commodity_Reference;
      Rental_Days : Non_Negative_Real;
      Rental_Cost : Concorde.Money.Money_Type)
      return Contract_Type;

   procedure Create_Agreement
     (Contract : Contract_Type;
      Supplier : Concorde.Handles.Agent_Reference;
      User     : Concorde.Handles.Agent_Reference);

private

   type Contract_Type is
     new Concorde.Handles.Contract_Reference;

end Concorde.Contracts;
