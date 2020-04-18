package Concorde.Behaviors.Sines is

   function Sine_Behavior
     (Frequency : Non_Negative_Real;
      Amplitude : Non_Negative_Real;
      Phase     : Non_Negative_Real)
      return Behavior_Type'Class;

end Concorde.Behaviors.Sines;
