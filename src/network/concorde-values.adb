package body Concorde.Values is

   type Constant_Value_Type is
     new Value_Interface with
      record
         Value : Real;
      end record;

   overriding function To_Real
     (Value : Constant_Value_Type)
      return Real
   is (Value.Value);

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value
     (Value : Real)
      return Value_Interface'Class
   is
   begin
      return Constant_Value_Type'
        (Value => Value);
   end Constant_Value;

end Concorde.Values;
