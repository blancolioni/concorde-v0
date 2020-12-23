with Tropos;

with Concorde.Handles.Constructed;
with Concorde.Handles.Has_Stock;
with Concorde.Handles.Supplied;

package Concorde.Configure.Commodities is

   procedure Configure_Commodities
     (Scenario_Name : String);

   procedure Configure_Stock
     (Has_Stock : Concorde.Handles.Has_Stock.Has_Stock_Class;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   procedure Configure_Constructed
     (Constructed : Concorde.Handles.Constructed.Constructed_Class;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0);

   procedure Configure_Supplied
     (Supplied : Concorde.Handles.Supplied.Supplied_Class;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

end Concorde.Configure.Commodities;
