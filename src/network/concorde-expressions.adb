with Ada.Containers.Indefinite_Holders;

with Concorde.Elementary_Functions;
with Concorde.Real_Images;

with Concorde.Primitives;

package body Concorde.Expressions is

   type Delay_Expression_Type is
     new Context_Expression_Type with null record;

   overriding function Context_Operation
     (Expression : Delay_Expression_Type)
      return String
   is ("delay");

   overriding function Update_Context
     (Expression : Delay_Expression_Type;
      Current    : Expression_Context)
      return Expression_Context
   is (With_Delay (Current, Expression.Period));

   type Smooth_Expression_Type is
     new Context_Expression_Type with null record;

   overriding function Context_Operation
     (Expression : Smooth_Expression_Type)
      return String
   is ("smooth");

   overriding function Update_Context
     (Expression : Smooth_Expression_Type;
      Current    : Expression_Context)
      return Expression_Context
   is (With_Smoothing (Current, Expression.Period));

   function New_Expression
     (From : Root_Expression_Type'Class)
      return Expression_Type;

   package Value_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Values.Value_Interface'Class, Concorde.Values."=");

   type Value_Expression_Type is
     new Root_Expression_Type with
      record
         Value : Value_Holders.Holder;
      end record;

   overriding function Evaluate
     (Expression  : Value_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Value_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function To_String
     (Expression : Value_Expression_Type)
      return String
   is (Concorde.Real_Images.Approximate_Image
       (Expression.Value.Element.To_Real));

   overriding function Replace
     (Expression     : not null access constant Value_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type;

   package Behavior_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Behaviors.Behavior_Type'Class, Concorde.Behaviors."=");

   type Behavior_Expression_Type is
     new Root_Expression_Type with
      record
         Behavior : Behavior_Holders.Holder;
      end record;

   overriding function Evaluate
     (Expression  : Behavior_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Behavior_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function To_String
     (Expression : Behavior_Expression_Type)
      return String;

   overriding function Free_Variables
     (Expression : Behavior_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array;

   overriding function Replace
     (Expression     : not null access constant Behavior_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type;

   type Identifier_Expression_Type is
     new Root_Expression_Type with
      record
         Symbol : Concorde.Symbols.Symbol_Id;
      end record;

   overriding function Evaluate
     (Expression  : Identifier_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Identifier_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Free_Variables
     (Expression : Identifier_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array;

   overriding function To_String
     (Expression : Identifier_Expression_Type)
      return String
   is (Concorde.Symbols.Get_Name (Expression.Symbol));

   overriding function Replace
     (Expression     : not null access constant Identifier_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type;

   type Operator_Type is ('+', '-', '*', '/', '^');

   type Operator_Expression_Type is
     new Root_Expression_Type with
      record
         Operator : Operator_Type;
         Left     : Expression_Type;
         Right    : Expression_Type;
      end record;

   overriding function Evaluate
     (Expression  : Operator_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Operator_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Free_Variables
     (Expression : Operator_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array;

   overriding function To_String
     (Expression : Operator_Expression_Type)
      return String;

   overriding function Replace
     (Expression     : not null access constant Operator_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type;

   type Application_Expression_Type is
     new Root_Expression_Type with
      record
         Left     : Expression_Type;
         Right    : Expression_Type;
      end record;

   overriding function Evaluate
     (Expression  : Application_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Apply
     (Expression  : Application_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class;

   overriding function Free_Variables
     (Expression : Application_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is (Concorde.Symbols."&"
       (Expression.Left.Free_Variables, Expression.Right.Free_Variables));

   overriding function To_String
     (Expression : Application_Expression_Type)
      return String
   is (Expression.Left.To_String & " "
       & (if Expression.Right.all in Application_Expression_Type'Class
          then "(" & Expression.Right.To_String & ")"
          else Expression.Right.To_String));

   overriding function Replace
     (Expression     : not null access constant Application_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is (Apply (Expression.Left.Replace (Original_Value, New_Value),
              Expression.Right.Replace (Original_Value, New_Value)));

   function "*" (Left, Right : Expression_Type) return Expression_Type
   is (New_Expression
       (Operator_Expression_Type'
          ('*', Left, Right)));

   function "**" (Left, Right : Expression_Type) return Expression_Type
   is (New_Expression
       (Operator_Expression_Type'
          ('^', Left, Right)));

   function "/" (Left, Right : Expression_Type) return Expression_Type
   is (New_Expression
       (Operator_Expression_Type'
          ('/', Left, Right)));

   function "+" (Left, Right : Expression_Type) return Expression_Type
   is (New_Expression
       (Operator_Expression_Type'
          ('+', Left, Right)));

   function "-" (Left, Right : Expression_Type) return Expression_Type
   is (New_Expression
       (Operator_Expression_Type'
          ('-', Left, Right)));

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Identifier_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment, Context);
   begin
      return Concorde.Primitives.Evaluate
        (Name        => Concorde.Symbols.Get_Name (Expression.Symbol),
         Arguments   => Arguments);
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Left : not null access constant Root_Expression_Type'Class;
      Right : not null access constant Root_Expression_Type'Class)
      return Expression_Type
   is
   begin
      return new Application_Expression_Type'
        (Left  => Expression_Type (Left),
         Right => Expression_Type (Right));
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Value_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment, Arguments, Context);
   begin
      return (raise Constraint_Error with
                "cannot apply value " & Expression.To_String);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Application_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      New_Arguments : Concorde.Values.Vectors.Vector;
   begin
      New_Arguments.Append
        (Expression.Right.Evaluate (Environment, Context));
      for Arg of Arguments loop
         New_Arguments.Append (Arg);
      end loop;

      return Expression.Left.Apply (Environment, New_Arguments, Context);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Behavior_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment, Arguments, Context);
   begin
      return (raise Constraint_Error with
                "cannot apply behaviour " & Expression.To_String);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Operator_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment, Arguments, Context);
   begin
      return (raise Constraint_Error with
                "cannot apply operator " & Expression.To_String);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Context_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Arguments   : Concorde.Values.Vectors.Vector;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
   begin
      return Expression.Inner.Apply
        (Environment, Arguments,
         Context_Expression_Type'Class (Expression)
         .Update_Context (Context));
   end Apply;

   -------------------------
   -- Behavior_Expression --
   -------------------------

   function Behavior_Expression
     (Behavior : Concorde.Behaviors.Behavior_Type'Class)
      return Expression_Type
   is
   begin
      return New_Expression
        (Behavior_Expression_Type'
           (Behavior => Behavior_Holders.To_Holder (Behavior)));
   end Behavior_Expression;

   ----------------------
   -- Delay_Expression --
   ----------------------

   function Delay_Expression
     (Age   : Real_Time;
      Inner : Expression_Type)
      return Expression_Type
   is
   begin
      return New_Expression
        (Delay_Expression_Type'
           (Period => Age,
            Inner  => Inner));
   end Delay_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Application_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      Arguments : Concorde.Values.Vectors.Vector;
   begin
      Arguments.Append (Expression.Right.Evaluate (Environment, Context));
      return Expression.Left.Apply (Environment, Arguments, Context);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Value_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment, Context);
   begin
      return Expression.Value.Element;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Behavior_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      Time_Symbol : constant Concorde.Symbols.Symbol_Id :=
                      Concorde.Symbols.Get_Symbol ("time");
      Current_Time : constant Real :=
                       Environment.Get (Time_Symbol).To_Real;
      Sample_Time  : constant Real_Time :=
                       Real_Time (Current_Time) - Context.With_Delay;
      Sample_Value : constant Real :=
                       Expression.Behavior.Element.Sample (Sample_Time);
   begin
      return Concorde.Values.Constant_Value (Sample_Value);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Identifier_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
   begin
      return Result : constant Concorde.Values.Value_Interface'Class :=
        Environment.Get (Expression.Symbol,
                         Context.With_Delay, Context.With_Smoothing);
--
--        do
--           declare
--              Img : constant String :=
--                      Expression.To_String & " => "
--                      & Image (Result.To_Real);
--           begin
--              if Context.With_Smoothing > 0.0 then
--                 Concorde.Logging.Log ("", "", "eval", Img);
--              end if;
--           end;
--        end return;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Operator_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      Left   : constant Concorde.Values.Value_Interface'Class :=
                 (if Expression.Left = null
                  then Concorde.Values.Constant_Value (0.0)
                  else Expression.Left.Evaluate (Environment, Context));
      Right  : constant Concorde.Values.Value_Interface'Class :=
                 (if Expression.Right = null
                  then Concorde.Values.Constant_Value (0.0)
                  else Expression.Right.Evaluate (Environment, Context));
      Result : Real;
   begin
      case Expression.Operator is
         when '+' =>
            Result := Left.To_Real + Right.To_Real;
         when '-' =>
            Result := Left.To_Real - Right.To_Real;
         when '*' =>
            Result := Left.To_Real * Right.To_Real;
         when '/' =>
            Result := Left.To_Real / Right.To_Real;
         when '^' =>
            declare
               X : constant Real := Left.To_Real;
               Y : constant Real := Right.To_Real;
            begin
               if X < 0.0 then
                  if Y < 0.0 then
                     Result := 1.0 / X ** Natural (-Y);
                  else
                     Result := X ** Natural (Y);
                  end if;
               else
                  Result := Concorde.Elementary_Functions."**" (X, Y);
               end if;
            end;
      end case;
      return Concorde.Values.Constant_Value (Result);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Context_Expression_Type;
      Environment : Concorde.Values.Environment_Interface'Class;
      Context     : Expression_Context)
      return Concorde.Values.Value_Interface'Class
   is
      New_Context : constant Expression_Context :=
                      Context_Expression_Type'Class (Expression)
                      .Update_Context (Context);

   begin
      return Expression.Inner.Evaluate
        (Environment, New_Context);
   end Evaluate;

   --------------------
   -- Free_Variables --
   --------------------

   function Free_Variables
     (Expression : Root_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is
      pragma Unreferenced (Expression);
   begin
      return Concorde.Symbols.No_Symbols;
   end Free_Variables;

   --------------------
   -- Free_Variables --
   --------------------

   overriding function Free_Variables
     (Expression : Behavior_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is
      pragma Unreferenced (Expression);
   begin
      return (1 => Concorde.Symbols.Get_Symbol ("time"));
   end Free_Variables;

   --------------------
   -- Free_Variables --
   --------------------

   overriding function Free_Variables
     (Expression : Identifier_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is
   begin
      return (1 => Expression.Symbol);
   end Free_Variables;

   --------------------
   -- Free_Variables --
   --------------------

   overriding function Free_Variables
     (Expression : Operator_Expression_Type)
      return Concorde.Symbols.Symbol_Id_Array
   is
      use Concorde.Symbols;
   begin
      return (if Expression.Left = null then No_Symbols
              else Expression.Left.Free_Variables)
        & (if Expression.Right = null then No_Symbols
           else Expression.Right.Free_Variables);
   end Free_Variables;

   ---------------------------
   -- Identifier_Expression --
   ---------------------------

   function Identifier_Expression
     (Identifier : String)
      return Expression_Type
   is
   begin
      return New_Expression
        (Identifier_Expression_Type'
           (Symbol => Concorde.Symbols.Get_Symbol (Identifier)));
   end Identifier_Expression;

   --------------------
   -- New_Expression --
   --------------------

   function New_Expression
     (From : Root_Expression_Type'Class)
      return Expression_Type
   is
   begin
      return new Root_Expression_Type'Class'(From);
   end New_Expression;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Expression     : not null access constant Value_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is
      pragma Unreferenced (Original_Value, New_Value);
   begin
      return Expression_Type (Expression);
   end Replace;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Expression     : not null access constant Behavior_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is
      pragma Unreferenced (Original_Value, New_Value);
   begin
      return Expression_Type (Expression);
   end Replace;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Expression     : not null access constant Identifier_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is
      Text : constant String := Concorde.Symbols.Get_Name (Expression.Symbol);
   begin
      for I in Text'Range loop
         if Text (I) = '[' then
            for J in I + 1 .. Text'Last loop
               if Text (J) = ']' then
                  if Text (I + 1 .. J - 1) = Original_Value then
                     return Identifier_Expression
                       (Identifier =>
                          Text (Text'First .. I - 1)
                        & New_Value
                        & Text (J + 1 .. Text'Last));
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      return Expression_Type (Expression);
   end Replace;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Expression     : not null access constant Operator_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is
   begin
      return New_Expression
        (Operator_Expression_Type'
           (Expression.Operator,
            Expression.Left.Replace (Original_Value, New_Value),
            Expression.Right.Replace (Original_Value, New_Value)));
   end Replace;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Expression     : not null access constant Context_Expression_Type;
      Original_Value : String;
      New_Value      : String)
      return Expression_Type
   is
      Result : Context_Expression_Type'Class :=
                 Context_Expression_Type'Class (Expression.all);
   begin
      Result.Inner := Result.Inner.Replace (Original_Value, New_Value);
      return new Context_Expression_Type'Class'(Result);
   end Replace;

   -----------------------
   -- Smooth_Expression --
   -----------------------

   function Smooth_Expression
     (Age   : Real_Time;
      Inner : Expression_Type)
      return Expression_Type
   is
   begin
      return New_Expression
        (Smooth_Expression_Type'
           (Period => Age,
            Inner  => Inner));
   end Smooth_Expression;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Behavior_Expression_Type)
      return String
   is
      pragma Unreferenced (Expression);
   begin
      return "f (t)";
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Operator_Expression_Type)
      return String
   is
      Op : constant String :=
             (case Expression.Operator is
                 when '+' => "+",
                 when '-' => "-",
                 when '*' => "*",
                 when '/' => "/",
                 when '^' => "**");
   begin
      return (if Expression.Left = null then ""
              elsif Expression.Left.all in Identifier_Expression_Type'Class
              or else Expression.Left.all in Value_Expression_Type'Class
              then Expression.Left.To_String & " "
              else "(" & Expression.Left.To_String & ") ")
        & Op
        & (if Expression.Right = null then ""
           else " " & Expression.Right.To_String);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Context_Expression_Type)
      return String
   is
   begin
      return Context_Expression_Type'Class (Expression).Context_Operation
        & " "
        & Concorde.Real_Images.Approximate_Image
        (Real (Expression.Period))
        & " (" & Expression.Inner.To_String & ")";
   end To_String;

   ----------------------
   -- Value_Expression --
   ----------------------

   function Value_Expression
     (Value : Concorde.Values.Value_Interface'Class)
      return Expression_Type
   is
   begin
      return New_Expression
        (Value_Expression_Type'
           (Value => Value_Holders.To_Holder (Value)));
   end Value_Expression;

end Concorde.Expressions;
