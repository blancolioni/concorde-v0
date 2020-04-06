package Concorde.Primitives.Aggregates is

   function Minimum return Primitive_Interface'Class;
   function Maximum return Primitive_Interface'Class;
   function Sum return Primitive_Interface'Class;
   function Product return Primitive_Interface'Class;

private

   type Aggregate_Primitive is
     abstract new Primitive_Interface with null record;

   function Base
     (Primitive   : Aggregate_Primitive)
      return Real
      is abstract;

   function Operate
     (Primitive   : Aggregate_Primitive;
      Left, Right : Real)
      return Real
      is abstract;

   overriding function Evaluate
     (Primitive   : Aggregate_Primitive;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class;

   overriding function Minimum_Argument_Count
     (Primitive : Aggregate_Primitive)
      return Natural
   is (0);

   overriding function Maximum_Argument_Count
     (Primitive : Aggregate_Primitive)
      return Natural
      is (Natural'Last);

end Concorde.Primitives.Aggregates;
