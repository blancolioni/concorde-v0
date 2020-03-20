package Concorde.Network.Conditions is

   function Check_Condition
     (Condition : String;
      Network   : Concorde.Db.Network_Reference;
      Node      : Concorde.Db.Node_Reference;
      Inertia   : Non_Negative_Real)
      return Boolean;

end Concorde.Network.Conditions;
