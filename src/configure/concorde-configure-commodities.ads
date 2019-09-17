with Tropos;

with Concorde.Db.Has_Stock;

package Concorde.Configure.Commodities is

   procedure Configure_Commodities
     (Scenario_Name : String);

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock.Has_Stock_Type;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   function Next_Commodity_Index return Positive;

end Concorde.Configure.Commodities;
