with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;

with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Primitives;
with Concorde.Values.Vectors;

package body Concorde.Evaluator is

   Log_Evaluation : constant Boolean := False;
   Log_Results    : constant Boolean := True;

   package Primitive_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Primitives.Primitive_Interface'Class,
        Concorde.Primitives."=");

   type Instruction_Type is
     (I_NOP,
      I_STOP,
      I_CONSTANT,
      I_NODE,
      I_PRIMITIVE);

   type Cell_Type (Instruction : Instruction_Type := I_NOP) is
      record
         case Instruction is
            when I_NOP | I_STOP =>
               null;
            when I_CONSTANT =>
               Value : Real;
            when I_NODE =>
               Node  : Concorde.Nodes.Node_Handle;
               Delayed_By : Non_Negative_Real;
               Smoothing  : Non_Negative_Real;
            when I_PRIMITIVE =>
               Primitive : Primitive_Holders.Holder;
         end case;
      end record;

   function Show (Cell : Cell_Type) return String;

   type Cell_Address is range 1 .. 10_000;

   package Cell_Vectors is
     new Ada.Containers.Vectors (Cell_Address, Cell_Type);

   type Stack_Address is range 1 .. 1_000;

   package Value_Stacks is
     new Ada.Containers.Vectors (Stack_Address, Real);

   package Handle_Vectors is
     new Ada.Containers.Vectors (Evaluation_Handle, Cell_Address);

   Cell_Vector   : Cell_Vectors.Vector;
   Handle_Vector : Handle_Vectors.Vector;

   type Compile_Context is
      record
         With_Delay : Real := 0.0;
         With_Smoothing : Real := 0.0;
      end record;

   package Compile_Context_Stacks is
     new Ada.Containers.Doubly_Linked_Lists (Compile_Context);

   type Compiler_Type is
     new Concorde.Expressions.Compiler_Interface with
      record
         Context_Stack : Compile_Context_Stacks.List;
      end record;

   overriding procedure Push_Value
     (Compiler : in out Compiler_Type;
      Value    : Real);

   overriding procedure Push_Node
     (Compiler : in out Compiler_Type;
      Tag      : String);

   overriding procedure Push_Primitive
     (Compiler  : in out Compiler_Type;
      Primitive : Concorde.Primitives.Primitive_Interface'Class);

   overriding procedure Push_Delay
     (Compiler : in out Compiler_Type;
      Value    : Real);

   overriding procedure Push_Smoothing
     (Compiler : in out Compiler_Type;
      Value    : Real);

   overriding procedure Pop_Delay
     (Compiler  : in out Compiler_Type);

   overriding procedure Pop_Smoothing
     (Compiler  : in out Compiler_Type);

   overriding procedure End_Of_Expression
     (Compiler : in out Compiler_Type);

   -------------
   -- Compile --
   -------------

   function Compile
     (Expression : Concorde.Expressions.Expression_Type)
      return Evaluation_Handle
   is
      Compiler : Compiler_Type;
   begin
      return Handle : constant Evaluation_Handle :=
        Handle_Vector.Last_Index + 1
      do
         Handle_Vector.Append (Cell_Vector.Last_Index + 1);
         Expression.Compile (Compiler);
         Compiler.End_Of_Expression;
      end return;
   end Compile;

   -----------------------
   -- End_Of_Expression --
   -----------------------

   overriding procedure End_Of_Expression
     (Compiler : in out Compiler_Type)
   is
      pragma Unreferenced (Compiler);
   begin
      Cell_Vector.Append
        (Cell_Type'
           (Instruction => I_STOP));
   end End_Of_Expression;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Handle  : Evaluation_Handle;
      Network : Concorde.Nodes.Value_Container)
      return Real
   is
      Stack : Value_Stacks.Vector;
      Next  : Cell_Address := Handle_Vector.Element (Handle);

      function Show_Stack (Position : Value_Stacks.Cursor) return String
      is (if Value_Stacks.Has_Element (Position)
          then " " & Concorde.Real_Images.Approximate_Image
            (Value_Stacks.Element (Position))
          & Show_Stack (Value_Stacks.Previous (Position))
          else " ");

   begin
      loop
         declare
            Cell : constant Cell_Type := Cell_Vector.Element (Next);
         begin

            Next := Next + 1;

            case Cell.Instruction is
               when I_NOP =>
                  null;
               when I_STOP =>
                  if Log_Results then
                     Concorde.Logging.Log
                       (Cell_Address'Image (Next - 1), "eval",
                        "Result",
                        Concorde.Real_Images.Approximate_Image
                          (Stack.Last_Element));
                  end if;
                  exit;
               when I_CONSTANT =>
                  Stack.Append (Cell.Value);
               when I_NODE =>
                  Stack.Append
                    (Concorde.Nodes.Inertial_Value
                       (Concorde.Nodes.Get_Handle (Network, Cell.Node),
                        Cell.Delayed_By, Cell.Smoothing));
               when I_PRIMITIVE =>
                  declare
                     use Concorde.Primitives;
                     P : constant Primitive_Interface'Class :=
                           Cell.Primitive.Element;
                     Arguments : array (1 .. P.Argument_Count) of Real;
                     Vector    : Concorde.Values.Vectors.Vector;
                  begin
                     for Arg of reverse Arguments loop
                        Arg := Stack.Last_Element;
                        Stack.Delete_Last;
                     end loop;

                     for Arg of Arguments loop
                        Vector.Append (Concorde.Values.Constant_Value (Arg));
                     end loop;

                     Stack.Append (P.Evaluate (Vector).To_Real);
                  end;

            end case;

            if Log_Evaluation then
               Concorde.Logging.Log
                 (Cell_Address'Image (Next - 1), "eval",
                  Show (Cell),
                  "[" & Show_Stack (Stack.Last) & "]");
            end if;

         end;
      end loop;

      return Stack.Last_Element;
   end Evaluate;

   ---------------
   -- Pop_Delay --
   ---------------

   overriding procedure Pop_Delay
     (Compiler  : in out Compiler_Type)
   is
   begin
      Compiler.Context_Stack.Delete_Last;
   end Pop_Delay;

   -------------------
   -- Pop_Smoothing --
   -------------------

   overriding procedure Pop_Smoothing
     (Compiler  : in out Compiler_Type)
   is
   begin
      Compiler.Context_Stack.Delete_Last;
   end Pop_Smoothing;

   ----------------
   -- Push_Delay --
   ----------------

   overriding procedure Push_Delay
     (Compiler  : in out Compiler_Type;
      Value     : Real)
   is
      Context : Compile_Context;
   begin
      if not Compiler.Context_Stack.Is_Empty then
         Context := Compiler.Context_Stack.Last_Element;
      end if;
      Context.With_Delay := Value;
      Compiler.Context_Stack.Append (Context);
   end Push_Delay;

   ---------------
   -- Push_Node --
   ---------------

   overriding procedure Push_Node
     (Compiler       : in out Compiler_Type;
      Tag            : String)
   is
      Context : Compile_Context;
   begin
      if not Compiler.Context_Stack.Is_Empty then
         Context := Compiler.Context_Stack.Last_Element;
      end if;
      Cell_Vector.Append
        (Cell_Type'
           (Instruction => I_NODE,
            Node        => Concorde.Nodes.Get_Handle (Tag),
            Delayed_By  => Context.With_Delay,
            Smoothing   => Context.With_Smoothing));
   end Push_Node;

   --------------------
   -- Push_Primitive --
   --------------------

   overriding procedure Push_Primitive
     (Compiler  : in out Compiler_Type;
      Primitive : Concorde.Primitives.Primitive_Interface'Class)
   is
      pragma Unreferenced (Compiler);
   begin
      Cell_Vector.Append
        (Cell_Type'
           (Instruction => I_PRIMITIVE,
            Primitive   => Primitive_Holders.To_Holder (Primitive)));
   end Push_Primitive;

   --------------------
   -- Push_Smoothing --
   --------------------

   overriding procedure Push_Smoothing
     (Compiler  : in out Compiler_Type;
      Value     : Real)
   is
      Context : Compile_Context;
   begin
      if not Compiler.Context_Stack.Is_Empty then
         Context := Compiler.Context_Stack.Last_Element;
      end if;
      Context.With_Smoothing := Value;
      Compiler.Context_Stack.Append (Context);
   end Push_Smoothing;

   ----------------
   -- Push_Value --
   ----------------

   overriding procedure Push_Value
     (Compiler : in out Compiler_Type;
      Value    : Real)
   is
      pragma Unreferenced (Compiler);
   begin
      Cell_Vector.Append
        (Cell_Type'
           (Instruction => I_CONSTANT,
            Value       => Value));
   end Push_Value;

   ----------
   -- Show --
   ----------

   function Show (Cell : Cell_Type) return String is
   begin
      case Cell.Instruction is
         when I_NOP =>
            return "NOP";
         when I_STOP =>
            return "STOP";
         when I_CONSTANT =>
            return "CONSTANT "
              & Concorde.Real_Images.Approximate_Image (Cell.Value);
         when I_NODE =>
            return "NODE " & Concorde.Nodes.Get_Tag (Cell.Node);
         when I_PRIMITIVE =>
            return "PRIMITIVE "
              & Cell.Primitive.Element.Name
              & "/"
              & Cell.Primitive.Element.Argument_Count'Image;
      end case;
   end Show;

end Concorde.Evaluator;
