with Concorde.Db;

package Concorde.Configure.Metrics is

   procedure Configure_Metrics
     (Scenario_Name : String);

   procedure Update_Metric
     (Metric_Tag    : String;
      Calculation   : String);

   procedure Save_Metrics;

   procedure Add_Calculation
     (Tag        : String;
      Content    : Concorde.Db.Node_Value_Type;
      Expression : String);

end Concorde.Configure.Metrics;
