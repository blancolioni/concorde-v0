with Concorde.Symbols;

package Concorde.Values is

   type Value_Interface is interface;

   function To_Real (Value : Value_Interface) return Real
                     is abstract;

   function Constant_Value
     (Value : Real)
      return Value_Interface'Class;

   type Environment_Interface is interface;

   function Get
     (Environment    : Environment_Interface;
      Name           : Concorde.Symbols.Symbol_Id;
      With_Delay     : Real := 0.0;
      With_Smoothing : Real := 0.0)
      return Value_Interface'Class
      is abstract;

end Concorde.Values;
