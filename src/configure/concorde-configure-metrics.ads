package Concorde.Configure.Metrics is

   procedure Configure_Metrics
     (Scenario_Name : String);

   procedure Update_Metric
     (Metric_Tag    : String;
      Calculation   : String);

   procedure Save_Metrics;

end Concorde.Configure.Metrics;
