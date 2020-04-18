package Concorde.Behaviors is

   subtype Real_Time is Real;

   type Behavior_Type is abstract tagged private;

   function Sample
     (Behavior : Behavior_Type;
      Time     : Real_Time)
      return Real
      is abstract;

   function "+" (Left, Right : Behavior_Type'Class)
                 return Behavior_Type'Class;

   function "-" (Left, Right : Behavior_Type'Class)
                 return Behavior_Type'Class;

   function "*"
     (Left  : Real;
      Right : Behavior_Type'Class)
      return Behavior_Type'Class;

   function "*"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class;

   function "/"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class;

   function "**"
     (Left  : Behavior_Type'Class;
      Right : Real)
      return Behavior_Type'Class;

   function Constant_Behavior
     (Value : Real)
      return Behavior_Type'Class;

   function Linear_Behavior
     (Gradient : Real;
      Start    : Real)
      return Behavior_Type'Class;

   type Behavior_Function_Interface is interface;

   type Behavior_Function is access
     function (Time : Real_Time) return Real;

   function To_Behavior
     (Fn : Behavior_Function)
      return Behavior_Type'Class;

   function Change_To
     (Old_Behavior : Behavior_Type'Class;
      New_Behavior : Behavior_Type'Class;
      Time       : Real_Time)
      return Behavior_Type'Class;

--
--     function Clamp
--       (Behavior : Behavior_Type'Class;
--        Low    : Real;
--        High   : Real)
--        return Behavior_Type'Class;

   --     function Linear_Regression
--       (Behavior       : Behavior_Type'Class;
--        From, To     : Real_Time;
--        Sample_Count : Positive := 10)
--        return Behavior_Type'Class;
--

private

   type Behavior_Type is abstract tagged null record;

   function Constant_Behavior
     (Value : Real)
      return Behavior_Type'Class
   is (Linear_Behavior (0.0, Value));

end Concorde.Behaviors;
