with Concorde.Elementary_Functions;
with Concorde.Behaviors.Holders;

package body Concorde.Behaviors is

   type Added_Behavior_Type is
     new Behavior_Type with
      record
         Left, Right : Holders.Holder;
      end record;

   overriding function Sample
     (Behavior : Added_Behavior_Type;
      Time   : Real_Time)
      return Real;

   type Scaled_Behavior_Type is
     new Behavior_Type with
      record
         Behavior : Holders.Holder;
         Scale  : Real;
      end record;

   overriding function Sample
     (Behavior : Scaled_Behavior_Type;
      Time   : Real_Time)
      return Real;

   type Linear_Behavior_Type is
     new Behavior_Type with
      record
         Gradient : Real;
         Start    : Real;
      end record;

   overriding function Sample
     (Behavior : Linear_Behavior_Type;
      Time   : Real_Time)
      return Real
   is (Real (Time) * Behavior.Gradient + Behavior.Start);

   type Power_Behavior_Type is
     new Behavior_Type with
      record
         Behavior : Holders.Holder;
         Power  : Real;
      end record;

   overriding function Sample
     (Behavior : Power_Behavior_Type;
      Time   : Real_Time)
      return Real
   is (Concorde.Elementary_Functions."**"
       (Behavior.Behavior.Element.Sample (Time),
        Behavior.Power));

   type Function_Behavior_Type is
     new Behavior_Type with
      record
         Fn : Behavior_Function;
      end record;

   overriding function Sample
     (Behavior : Function_Behavior_Type;
      Time   : Real_Time)
      return Real
   is (Behavior.Fn (Time));

   type Change_Behavior_Type is
     new Behavior_Type with
      record
         Change_Time : Real_Time;
         Before      : Holders.Holder;
         After       : Holders.Holder;
      end record;

   overriding function Sample
     (Behavior : Change_Behavior_Type;
      Time   : Real_Time)
      return Real
   is (if Time < Behavior.Change_Time
       then Behavior.Before.Element.Sample (Time)
       else Behavior.After.Element.Sample (Time));

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Real;
      Right : Behavior_Type'Class)
      return Behavior_Type'Class
   is
   begin
      return Result : constant Scaled_Behavior_Type := Scaled_Behavior_Type'
        (Behavior => Holders.To_Holder (Right),
         Scale  => Left);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class
   is
   begin
      return Right * Left;
   end "*";

   ----------
   -- "**" --
   ----------

   function "**"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class
   is
   begin
      return Result : constant Power_Behavior_Type := Power_Behavior_Type'
        (Behavior => Holders.To_Holder (Left),
         Power  => Right);
   end "**";

   ---------
   -- "+" --
   ---------

   function "+"
     (Left, Right : Behavior_Type'Class)
      return Behavior_Type'Class
   is
   begin
      return Result : constant Added_Behavior_Type := Added_Behavior_Type'
        (Left  => Holders.To_Holder (Left),
         Right => Holders.To_Holder (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left, Right : Behavior_Type'Class)
      return Behavior_Type'Class
   is
   begin
      return Left + (-1.0) * Right;
   end "-";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class
   is
   begin
      return Left * (1.0 / Right);
   end "/";

   ---------------
   -- Change_To --
   ---------------

   function Change_To
     (Old_Behavior : Behavior_Type'Class;
      New_Behavior : Behavior_Type'Class;
      Time       : Real_Time)
      return Behavior_Type'Class
   is
   begin
      return Change_Behavior_Type'
        (Change_Time => Time,
         Before      => Holders.To_Holder (Old_Behavior),
         After       => Holders.To_Holder (New_Behavior));
   end Change_To;

   -------------------
   -- Linear_Behavior --
   -------------------

   function Linear_Behavior
     (Gradient : Real;
      Start    : Real)
      return Behavior_Type'Class
   is
   begin
      return Result : constant Linear_Behavior_Type := Linear_Behavior_Type'
        (Gradient => Gradient,
         Start    => Start);
   end Linear_Behavior;

   ------------
   -- Sample --
   ------------

   overriding function Sample
     (Behavior : Added_Behavior_Type;
      Time   : Real_Time)
      return Real
   is
   begin
      return Behavior.Left.Element.Sample (Time)
        + Behavior.Right.Element.Sample (Time);
   end Sample;

   ------------
   -- Sample --
   ------------

   overriding function Sample
     (Behavior : Scaled_Behavior_Type;
      Time   : Real_Time)
      return Real
   is
   begin
      return Behavior.Behavior.Element.Sample (Time) * Behavior.Scale;
   end Sample;

   ---------------
   -- To_Behavior --
   ---------------

   function To_Behavior
     (Fn : Behavior_Function)
      return Behavior_Type'Class
   is
   begin
      return Behavior : constant Function_Behavior_Type :=
        Function_Behavior_Type'
          (Fn => Fn);
   end To_Behavior;

end Concorde.Behaviors;
