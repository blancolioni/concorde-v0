with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Tropos.Reader;

with Concorde.Identifiers;

with Concorde.Handles.Calculation;
with Concorde.Handles.Derived_Metric;
with Concorde.Handles.Metric;
with Concorde.Handles.Node;

package body Concorde.Configure.Metrics is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_List_Maps is
     new WL.String_Maps (String_Lists.List, String_Lists."=");

   Metric_Map : String_List_Maps.Map;

   ---------------------
   -- Add_Calculation --
   ---------------------

   procedure Add_Calculation
     (Tag        : String;
      Content    : Concorde.Db.Node_Value_Type;
      Expression : String)
   is
      Calculation : constant Concorde.Handles.Calculation.Calculation_Handle :=
                      Concorde.Handles.Calculation.Create
                        (Identifier => Concorde.Identifiers.Next_Identifier,
                         Node       => Concorde.Handles.Node.Empty_Handle,
                         Expression => Expression);
      Metric                 : constant Concorde.Handles.Derived_Metric
        .Derived_Metric_Handle :=
                      Concorde.Handles.Derived_Metric.Create
                        (Content     => Content,
                         Identifier  => Concorde.Identifiers.Next_Identifier,
                         Tag         => Tag,
                         Calculation => Calculation);
   begin
      Calculation.Update_Calculation
        .Set_Node (Metric)
        .Done;
   end Add_Calculation;

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
         Concorde.Handles.Metric.Create
           (Tag     => Rating_Config.Config_Name,
            Identifier => Concorde.Identifiers.Next_Identifier,
            Content => Concorde.Db.Rating);
      end loop;
      for Quantity_Config of Metrics_Config.Child ("quantity") loop
         Concorde.Handles.Metric.Create
           (Tag     => Quantity_Config.Config_Name,
            Identifier => Concorde.Identifiers.Next_Identifier,
            Content => Concorde.Db.Quantity);
      end loop;
      for Group_Config of Metrics_Config.Child ("group") loop
         Concorde.Handles.Metric.Create
           (Tag     => Group_Config.Config_Name,
            Identifier => Concorde.Identifiers.Next_Identifier,
            Content => Concorde.Db.Rating);
      end loop;
   end Configure_Metrics;

   ------------------
   -- Save_Metrics --
   ------------------

   procedure Save_Metrics is
      use Ada.Strings.Unbounded;
   begin
      for Position in Metric_Map.Iterate loop
         declare
            Metric_Tag : constant String :=
                           String_List_Maps.Key (Position);
            List       : constant String_Lists.List :=
                           String_List_Maps.Element (Position);
            Expression : Unbounded_String;
         begin
            for Expr of List loop
               if Expression = Null_Unbounded_String then
                  Expression := "(" & To_Unbounded_String (Expr) & ")";
               else
                  Expression := Expression & " + (" & Expr & ")";
               end if;
            end loop;

            Add_Calculation
              (Metric_Tag, Concorde.Db.Quantity,
               To_Single_Line (To_String (Expression)));

         end;
      end loop;
   end Save_Metrics;

   -------------------
   -- Update_Metric --
   -------------------

   procedure Update_Metric
     (Metric_Tag    : String;
      Calculation   : String)
   is
   begin
      if not Metric_Map.Contains (Metric_Tag) then
         Metric_Map.Insert (Metric_Tag, String_Lists.Empty_List);
      end if;

      Metric_Map (Metric_Tag).Append (Calculation);
   end Update_Metric;

end Concorde.Configure.Metrics;
