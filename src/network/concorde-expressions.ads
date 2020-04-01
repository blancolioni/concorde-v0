with Concorde.Behaviors;
with Concorde.Symbols;
with Concorde.Values;

with Concorde.Values.Vectors;

package Concorde.Expressions is

   type Expression_Context is private;

   function With_Smoothing
     (Context     : Expression_Context;
      Smooth_Time : Non_Negative_Real)
      return Expression_Context;

   function With_Delay
     (Context    : Expression_Context;
      Eval_Delay : Non_Negative_Real)
      return Expression_Context;

   function Default_Context return Expression_Context;

   type Root_Expression_Type is abstract tagged private;

   function Evaluate
     (Expression  : Root_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   function Apply
     (Expression  : Root_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
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

   function Smooth_Expression
     (Age   : Real_Time;
      Inner : Expression_Type)
      return Expression_Type;

private

   type Expression_Context is
      record
         With_Delay     : Non_Negative_Real;
         With_Smoothing : Non_Negative_Real;
      end record;

   function With_Smoothing
     (Context     : Expression_Context;
      Smooth_Time : Non_Negative_Real)
      return Expression_Context
   is (Expression_Context'
         (Context with delta With_Smoothing => Smooth_Time));

   function With_Delay
     (Context    : Expression_Context;
      Eval_Delay : Non_Negative_Real)
      return Expression_Context
   is (Expression_Context'
         (Context with delta With_Delay => Eval_Delay));

   function Default_Context return Expression_Context
   is (Expression_Context'
         (With_Delay     => 0.0,
          With_Smoothing => 0.0));

   type Root_Expression_Type is abstract tagged null record;

   type Context_Expression_Type is
     abstract new Root_Expression_Type with
      record
         Period : Real_Time;
         Inner  : Expression_Type;
      end record;

   function Context_Operation
     (Expression : Context_Expression_Type)
      return String
      is abstract;

   function Update_Context
     (Expression : Context_Expression_Type;
      Current    : Expression_Context)
      return Expression_Context
      is abstract;

   overriding function Evaluate
     (Expression  : Context_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Context_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Free_Variables
     (Expression : Context_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is (Expression.Inner.Free_Variables);

   overriding function To_String
     (Expression : Context_Expression_Type)
      return String;

   overriding function Replace
     (Expression     : not null access constant Context_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type;

end Concorde.Expressions;
