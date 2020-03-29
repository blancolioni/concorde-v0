with Concorde.Configure.Metrics;

with Concorde.Db.Group_Influence;
with Concorde.Db.Metric;
with Concorde.Db.Pop_Group;

package body Concorde.Configure.Pop_Groups is

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration);

   procedure Configure_Pop_Group_Metric
     (Pop_Group_Tag : String;
      Metric_Tag    : String;
      Commodity_Tag : String;
      Expression    : String);

   procedure Configure_Influence
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Influence --
   -------------------------

   procedure Configure_Influence
     (Config : Tropos.Configuration)
   is
      From : constant Concorde.Db.Pop_Group_Reference :=
               Concorde.Db.Pop_Group.Get_Reference_By_Tag
                 (Config.Config_Name);
   begin
      for Infl_Config of Config.Child ("influences") loop
         declare
            use Concorde.Db;
            To : constant Pop_Group_Reference :=
                   Pop_Group.Get_Reference_By_Tag (Infl_Config.Config_Name);
            Value : constant Real :=
                      Real (Float'(Infl_Config.Value));
         begin
            Concorde.Db.Group_Influence.Create
              (From, To, Value);
         end;
      end loop;
   end Configure_Influence;

   -------------------------
   -- Configure_Pop_Group --
   -------------------------

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration)
   is
      procedure Metric
        (Suffix : String;
         Value  : Concorde.Db.Node_Value_Type := Concorde.Db.Rating);

      ------------
      -- Metric --
      ------------

      procedure Metric
        (Suffix : String;
         Value  : Concorde.Db.Node_Value_Type := Concorde.Db.Rating)
      is
         Tag : constant String :=
                 Config.Config_Name
                 & (if Suffix = "" then "" else "-" & Suffix);
      begin
         Concorde.Db.Metric.Create
           (Content => Value,
            Tag     => Tag);
      end Metric;

      Is_Wealth_Group : constant Boolean :=
                          Config.Get ("wealth-group");
   begin
      Concorde.Db.Pop_Group.Create
        (Tag             => Config.Config_Name,
         Is_Wealth_Group => Is_Wealth_Group,
         Proportion      =>
           Real (Long_Float'(Config.Get ("proportion", 0.0))));

      Metric ("population", Concorde.Db.Quantity);
      Metric ("proportion");
      Metric ("income");
      Metric ("");

      if Is_Wealth_Group then
         Metric ("base-income", Concorde.Db.Money);
      end if;

      for Demand_Config of Config.Child ("demand") loop
         Configure_Pop_Group_Metric
           (Pop_Group_Tag => Config.Config_Name,
            Metric_Tag    => "demand",
            Commodity_Tag => Demand_Config.Config_Name,
            Expression    => Demand_Config.Value);
      end loop;

   end Configure_Pop_Group;

   --------------------------------
   -- Configure_Pop_Group_Metric --
   --------------------------------

   procedure Configure_Pop_Group_Metric
     (Pop_Group_Tag : String;
      Metric_Tag    : String;
      Commodity_Tag : String;
      Expression    : String)
   is
      pragma Unreferenced (Pop_Group_Tag);
   begin
      Concorde.Configure.Metrics.Update_Metric
        (Metric_Tag    => Commodity_Tag & "-" & Metric_Tag,
         Calculation   => Expression);
   end Configure_Pop_Group_Metric;

   --------------------------
   -- Configure_Pop_Groups --
   --------------------------

   procedure Configure_Pop_Groups
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "pop-groups",
         File_Class_Name => "group",
         Process         => Configure_Pop_Group'Access);
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "pop-groups",
         File_Class_Name => "group",
         Process         => Configure_Influence'Access);
   end Configure_Pop_Groups;

end Concorde.Configure.Pop_Groups;
