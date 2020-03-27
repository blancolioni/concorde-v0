package body Concorde.Primitives.Operators is

   type Min_Max_Primitive is
     new Real_Operator_Primitive with
      record
         Max : Boolean;
      end record;

   overriding function Operate
     (Primitive : Min_Max_Primitive;
      Left, Right : Real)
      return Real
   is (if Primitive.Max
       then Real'Max (Left, Right)
       else Real'Min (Left, Right));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Primitive   : Real_Operator_Primitive;
      Left, Right : Concorde.Values.Value_Interface'Class)
      return Concorde.Values.Value_Interface'Class
   is
   begin
      return Concorde.Values.Constant_Value
        (Real_Operator_Primitive'Class (Primitive).Operate
         (Left.To_Real, Right.To_Real));
   end Evaluate;

   ---------
   -- Max --
   ---------

   function Max return Primitive_Interface'Class is
   begin
      return Min_Max_Primitive'
        (Max => True);
   end Max;

   ---------
   -- Min --
   ---------

   function Min return Primitive_Interface'Class is
   begin
      return Min_Max_Primitive'
        (Max => False);
   end Min;

end Concorde.Primitives.Operators;
