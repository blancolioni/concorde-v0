package body Concorde.Primitives.Aggregates is

   type Aggregation_Operator is (Minimum, Maximum, Sum, Product);

   type Aggregation_Primitive is
     new Aggregate_Primitive with
      record
         Agg : Aggregation_Operator;
      end record;

   overriding function Base
     (Primitive   : Aggregation_Primitive)
      return Real
   is (case Primitive.Agg is
          when Minimum => Real'Last,
          when Maximum => Real'First,
          when Sum     => 0.0,
          when Product => 1.0);

   overriding function Operate
     (Primitive   : Aggregation_Primitive;
      Left, Right : Real)
      return Real
   is (case Primitive.Agg is
          when Minimum => Real'Min (Left, Right),
          when Maximum => Real'Max (Left, Right),
          when Sum     => Left + Right,
          when Product => Left * Right);

   function Aggregate
     (Operator : Aggregation_Operator)
      return Aggregation_Primitive'Class
   is (Aggregation_Primitive'(Agg => Operator));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Primitive : Aggregate_Primitive;
      Arguments : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class
   is
      Agg : Aggregate_Primitive'Class renames
              Aggregate_Primitive'Class (Primitive);
      Result : Real := Agg.Base;
   begin
      for Arg of Arguments loop
         Result := Agg.Operate (Result, Arg.To_Real);
      end loop;
      return Concorde.Values.Constant_Value (Result);
   end Evaluate;

   -------------
   -- Maximum --
   -------------

   function Maximum return Primitive_Interface'Class is
   begin
      return Aggregate (Maximum);
   end Maximum;

   -------------
   -- Minimum --
   -------------

   function Minimum return Primitive_Interface'Class is
   begin
      return Aggregate (Minimum);
   end Minimum;

   -------------
   -- Product --
   -------------

   function Product return Primitive_Interface'Class is
   begin
      return Aggregate (Product);
   end Product;

   ---------
   -- Sum --
   ---------

   function Sum return Primitive_Interface'Class is
   begin
      return Aggregate (Sum);
   end Sum;

end Concorde.Primitives.Aggregates;
