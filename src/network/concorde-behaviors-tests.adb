package body Concorde.Behaviors.Tests is

   ------------------
   -- Test_Network --
   ------------------

   function Test_Network return Concorde.Behaviors.Networks.Network_Type is
      use Concorde.Behaviors.Networks;
      Network          : Root_Network_Type;
      Income_Id        : constant Behavior_Id :=
                           Network.New_Behavior ("income", 100.0);
      Pop_Id           : constant Behavior_Id :=
                           Network.New_Behavior ("population", 1000.0);
      Tax_Rate_Id      : constant Behavior_Id :=
                           Network.New_Behavior ("tax-rate", 0.2);
      Tax_Avoidance_Id : constant Behavior_Id :=
                           Network.New_Behavior ("tax-avoidance", 0.0);
      Income_Tax_Id    : constant Behavior_Id :=
                           Network.New_Behavior ("income-tax", 0.0);
   begin
      return new Root_Network_Type'(Network);
   end Test_Network;

end Concorde.Behaviors.Tests;
