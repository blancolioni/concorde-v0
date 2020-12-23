package Concorde.Network.Conditions is

   function Check_Condition
     (Condition : String;
      Network   : Concorde.Handles.Network.Network_Class;
      Node      : Concorde.Handles.Node.Node_Handle;
      Inertia   : Non_Negative_Real)
      return Boolean;

end Concorde.Network.Conditions;
