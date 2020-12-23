with Concorde.Elementary_Functions;

package body Concorde.Primitives.Operators is

   type Min_Max_Primitive is
     new Real_Operator_Primitive with
      record
         Max : Boolean;
      end record;

   overriding function Name
     (Primitive : Min_Max_Primitive)
      return String
   is (if Primitive.Max then "max" else "min");

   overriding function Operate
     (Primitive : Min_Max_Primitive;
      Left, Right : Real)
      return Real
   is (if Primitive.Max
       then Real'Max (Left, Right)
       else Real'Min (Left, Right));

   type Operator_Primitive is
     new Real_Operator_Primitive with
      record
         Op : Operator_Type;
      end record;

   overriding function Name
     (Primitive : Operator_Primitive)
      return String
   is (Primitive.Op'Image);

   overriding function Operate
     (Primitive : Operator_Primitive;
      Left, Right : Real)
      return Real;

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

   -------------
   -- Operate --
   -------------

   overriding function Operate
     (Primitive : Operator_Primitive;
      Left, Right : Real)
      return Real
   is
   begin
      case Primitive.Op is
         when '+' =>
            return Left + Right;
         when '-' =>
            return Left - Right;
         when '*' =>
            return Left * Right;
         when '/' =>
            return Left / Right;
         when '^' =>
            if Left < 0.0 then
               if Right < 0.0 then
                  return 1.0 / Left ** Natural (-Right);
               else
                  return Left ** Natural (Right);
               end if;
            else
               return Concorde.Elementary_Functions."**" (Left, Right);
            end if;
      end case;
   end Operate;

   --------------
   -- Operator --
   --------------

   function Operator (Op : Operator_Type) return Primitive_Interface'Class is
   begin
      return Operator_Primitive'
        (Op => Op);
   end Operator;

end Concorde.Primitives.Operators;
