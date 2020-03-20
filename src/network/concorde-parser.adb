with Concorde.Parser.Tokens;               use Concorde.Parser.Tokens;
with Concorde.Parser.Lexical;              use Concorde.Parser.Lexical;

with Concorde.Behaviors.Sines;
with Concorde.Symbols;
with Concorde.Values;

package body Concorde.Parser is

   type Precedence_Range is range 1 .. 9;

   type Create_Op_Expr is
     access function (Left, Right : Concorde.Expressions.Expression_Type)
                      return Concorde.Expressions.Expression_Type;

   type Operator_Entry is
      record
         Is_Operator : Boolean := False;
         Precedence  : Precedence_Range := 9;
         Create      : Create_Op_Expr := null;
      end record;

   Ops : constant array (Token) of Operator_Entry :=
           (Tok_Plus     => (True, 4, Concorde.Expressions."+"'Access),
            Tok_Minus    => (True, 4, Concorde.Expressions."-"'Access),
            Tok_Asterisk => (True, 3, Concorde.Expressions."*"'Access),
            Tok_Slash    => (True, 3, Concorde.Expressions."/"'Access),
            Tok_Power    => (True, 2, Concorde.Expressions."**"'Access),
--              Tok_EQ       => (True, 6),
--              Tok_NE       => (True, 6),
--              Tok_GE       => (True, 6),
--              Tok_LE       => (True, 6),
--              Tok_GT       => (True, 6),
--              Tok_LT       => (True, 6),
            others       => <>);

   type Empty_Environment_Type is
     new Concorde.Values.Environment_Interface with null record;

   overriding function Get
     (Environment : Empty_Environment_Type;
      Name        : Concorde.Symbols.Symbol_Id;
      With_Delay  : Real_Time := 0.0)
      return Concorde.Values.Value_Interface'Class;

   function Parse_Atomic_Expression
     return Concorde.Expressions.Expression_Type;

   function Parse_Operator_Expression
     (Precedence : Precedence_Range)
      return Concorde.Expressions.Expression_Type;

   function Parse_Expression
     return Concorde.Expressions.Expression_Type;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Environment : Empty_Environment_Type;
      Name        : Concorde.Symbols.Symbol_Id;
      With_Delay  : Real_Time := 0.0)
      return Concorde.Values.Value_Interface'Class
   is
      pragma Unreferenced (Environment);
      use type Concorde.Symbols.Symbol_Id;
   begin
      if Name = Concorde.Symbols.Get_Symbol ("time") then
         return Concorde.Values.Constant_Value (-Real (With_Delay));
      else
         return raise Constraint_Error with
           "attempt to get value from empty environment";
      end if;
   end Get;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression
     return Concorde.Expressions.Expression_Type
   is

      use Concorde.Expressions;

      Negated : Boolean := False;

      function Result
        (Expr : Concorde.Expressions.Expression_Type)
         return Concorde.Expressions.Expression_Type
      is (if not Negated then Expr
          else (Constant_Expression (-1.0) * Expr));

   begin
      if Tok = Tok_Minus then
         Negated := True;
         Scan;
      elsif Tok = Tok_Plus then
         Scan;
      end if;

      if Tok = Tok_Float_Constant or else Tok = Tok_Integer_Constant then
         declare
            Value : Real := Real'Value (Tok_Text);
         begin
            Scan;
            if Tok = Tok_Percent then
               Value := Value / 100.0;
               Scan;
            end if;
            return Result (Constant_Expression (Value));
         end;
      elsif Tok = Tok_Identifier then
         declare
            Name : constant String := Tok_Text;
         begin
            Scan;
            return Result (Identifier_Expression (Name));
         end;
      elsif Tok = Tok_Left_Paren then
         Scan;
         declare
            Expr : constant Expression_Type :=
                     Result (Parse_Expression);
         begin
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
            return Expr;
         end;
      elsif Tok = Tok_Delay then
         Scan;
         if Tok = Tok_Integer_Constant then
            declare
               Delay_Count : constant Natural :=
                               Natural'Value (Tok_Text);
            begin
               Scan;
               declare
                  Delay_Formula : constant Expression_Type :=
                                    Parse_Atomic_Expression;
               begin
                  return Result
                    (Delay_Expression
                       (Age   => Real_Time (Delay_Count),
                        Inner => Delay_Formula));
               end;
            end;
         else
            Error ("missing delay count");
            while Tok_Indent > 1 loop
               Scan;
            end loop;
            return Concorde.Expressions.Value_Expression
              (Concorde.Values.Constant_Value (0.0));
         end if;
      elsif Tok = Tok_Sin then
         Scan;
         declare
            Wavelength : Non_Negative_Real := 1.0;
            Amplitude  : Non_Negative_Real := 1.0;
            Phase      : Non_Negative_Real := 0.0;
         begin
            if Tok = Tok_Float_Constant
              or else Tok = Tok_Integer_Constant
            then
               Wavelength := Real'Value (Tok_Text);
               Scan;
               if Tok = Tok_Float_Constant
                 or else Tok = Tok_Integer_Constant
               then
                  Amplitude := Real'Value (Tok_Text);
                  Scan;
                  if Tok = Tok_Float_Constant
                    or else Tok = Tok_Integer_Constant
                  then
                     Phase := Real'Value (Tok_Text);
                     Scan;
                  end if;
               end if;
            end if;

            return Result
              (Behavior_Expression
                 (Concorde.Behaviors.Sines.Sine_Behavior
                      (Frequency => 1.0 / Wavelength,
                       Amplitude => Amplitude,
                       Phase     => Phase)));
         end;

      else
         Error ("missing expression");
         return Concorde.Expressions.Value_Expression
           (Concorde.Values.Constant_Value (0.0));
      end if;
   end Parse_Atomic_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Concorde.Expressions.Expression_Type is
   begin
      return Parse_Operator_Expression (Precedence_Range'Last);
   end Parse_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Source : String)
      return Concorde.Expressions.Expression_Type
   is
   begin
      Open_String (Source);
      return Expression : constant Concorde.Expressions.Expression_Type :=
        Parse_Expression
      do
         Close;
      end return;
   end Parse_Expression;

   -------------------------------
   -- Parse_Operator_Expression --
   -------------------------------

   function Parse_Operator_Expression
     (Precedence : Precedence_Range)
      return Concorde.Expressions.Expression_Type
   is
      Left : constant Concorde.Expressions.Expression_Type :=
               (if Precedence = Precedence_Range'First
                then Parse_Atomic_Expression
                else Parse_Operator_Expression (Precedence - 1));
      Result : Concorde.Expressions.Expression_Type := Left;
   begin
      while Ops (Tok).Is_Operator
        and then Ops (Tok).Precedence = Precedence
      loop
         declare
            Op_Tok : constant Token := Tok;
            Right  : Concorde.Expressions.Expression_Type;
         begin
            Scan;
            Right :=
              (if Precedence = Precedence_Range'First
               then Parse_Atomic_Expression
               else Parse_Operator_Expression (Precedence - 1));
            Result := Ops (Op_Tok).Create (Result, Right);
         end;
      end loop;
      return Result;
   end Parse_Operator_Expression;

end Concorde.Parser;
