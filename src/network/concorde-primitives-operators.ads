package Concorde.Primitives.Operators is

   function Max return Primitive_Interface'Class;
   function Min return Primitive_Interface'Class;

private

   type Real_Operator_Primitive is
     abstract new Binary_Primitive with null record;

   function Operate (Primitive   : Real_Operator_Primitive;
                     Left, Right : Real)
                     return Real
                     is abstract;

   overriding function Evaluate
     (Primitive   : Real_Operator_Primitive;
      Left, Right : Concorde.Values.Value_Interface'Class)
      return Concorde.Values.Value_Interface'Class;

end Concorde.Primitives.Operators;
