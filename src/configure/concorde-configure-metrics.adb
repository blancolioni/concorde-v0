with Tropos.Reader;

with Concorde.Db.Metric;

package body Concorde.Configure.Metrics is

   -----------------------
   -- Configure_Metrics --
   -----------------------

   procedure Configure_Metrics (Scenario_Name : String) is
      Metrics_Config : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config
                           (Scenario_File
                              (Scenario_Name  => Scenario_Name,
                               Directory_Name => "economy",
                               File_Name      => "metrics.config"));
   begin
      for Rating_Config of Metrics_Config.Child ("rating") loop
         Concorde.Db.Metric.Create
           (Tag     => Rating_Config.Config_Name,
            Content => Concorde.Db.Rating);
      end loop;
      for Quantity_Config of Metrics_Config.Child ("quantity") loop
         Concorde.Db.Metric.Create
           (Tag     => Quantity_Config.Config_Name,
            Content => Concorde.Db.Quantity);
      end loop;
      for Group_Config of Metrics_Config.Child ("group") loop
         Concorde.Db.Metric.Create
           (Tag     => Group_Config.Config_Name,
            Content => Concorde.Db.Rating);
      end loop;
   end Configure_Metrics;

end Concorde.Configure.Metrics;
