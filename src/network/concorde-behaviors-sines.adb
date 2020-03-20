with Ada.Numerics;

with Concorde.Elementary_Functions;

package body Concorde.Behaviors.Sines is

   type Sine_Behavior_Type is new Behavior_Type with
      record
         Frequency : Non_Negative_Real;
         Amplitude : Non_Negative_Real;
         Phase     : Non_Negative_Real;
      end record;

   overriding function Sample
     (Behavior : Sine_Behavior_Type;
      Time   : Real_Time)
      return Real;

   ------------
   -- Sample --
   ------------

   overriding function Sample
     (Behavior : Sine_Behavior_Type;
      Time   : Real_Time)
      return Real
   is
      use Concorde.Elementary_Functions;
      Pi : constant := Ada.Numerics.Pi;
   begin
      return Sin (Real (Time) * Behavior.Frequency
                  * (2.0 * Pi) + Behavior.Phase)
        * Behavior.Amplitude;
   end Sample;

   -----------------
   -- Sine_Behavior --
   -----------------

   function Sine_Behavior
     (Frequency : Non_Negative_Real;
      Amplitude : Non_Negative_Real;
      Phase     : Non_Negative_Real)
      return Behavior_Type'Class
   is
   begin
      return Behavior : constant Sine_Behavior_Type := Sine_Behavior_Type'
        (Frequency => Frequency,
         Amplitude => Amplitude,
         Phase     => Phase);
   end Sine_Behavior;

end Concorde.Behaviors.Sines;
