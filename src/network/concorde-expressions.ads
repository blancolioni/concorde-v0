with Concorde.Behaviors;
with Concorde.Symbols;
with Concorde.Values;

with Concorde.Values.Vectors;

package Concorde.Expressions is

   type Root_Expression_Type is abstract tagged private;

   function Evaluate
     (Expression  : Root_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      With_Delay  : Real := 0.0)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   function Apply
     (Expression : Root_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      With_Delay  : Real := 0.0)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   function Free_Variables
     (Expression : Root_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array;

   function To_String (Expression : Root_Expression_Type) return String
                       is abstract;

   type Expression_Type is access constant Root_Expression_Type'Class;

   function Apply
     (Left : not null access constant Root_Expression_Type'Class;
      Right : not null access constant Root_Expression_Type'Class)
      return Expression_Type;

   function Replace
     (Expression     : not null access constant Root_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
      is abstract;

   function Value_Expression
     (Value : Concorde.Values.Value_Interface'Class)
      return Expression_Type;

   function Constant_Expression
     (Value : Real)
      return Expression_Type
   is (Value_Expression (Concorde.Values.Constant_Value (Value)));

   function Behavior_Expression
     (Behavior : Concorde.Behaviors.Behavior_Type'Class)
      return Expression_Type;

   function Identifier_Expression
     (Identifier : String)
      return Expression_Type;

   function "+" (Left, Right : Expression_Type) return Expression_Type;
   function "-" (Left, Right : Expression_Type) return Expression_Type;
   function "*" (Left, Right : Expression_Type) return Expression_Type;
   function "/" (Left, Right : Expression_Type) return Expression_Type;
   function "**" (Left, Right : Expression_Type) return Expression_Type;

   function Delay_Expression
     (Age   : Real_Time;
      Inner : Expression_Type)
      return Expression_Type;

private

   type Root_Expression_Type is abstract tagged null record;

end Concorde.Expressions;
