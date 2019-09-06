with Ada.Containers.Vectors;
with Concorde.Elementary_Functions;

package body Concorde.Utility is

   type Zero_Utility_Function is
     new Utility_Function_Interface with null record;

   overriding function Evaluate
     (Fn          : Zero_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real;

   package Real_Vectors is
     new Ada.Containers.Vectors (Natural, Real);

   type Polynomial_Utility_Function is
     new Utility_Function_Interface with
      record
         A : Real_Vectors.Vector;
      end record;

   overriding function Evaluate
     (Fn          : Polynomial_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real;

   type Logarithmic_Utility_Function is
     new Utility_Function_Interface with
      record
         X_Offset : Real;
         Scale    : Real;
      end record;

   overriding function Evaluate
     (Fn          : Logarithmic_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Fn          : Zero_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real
   is
      pragma Unreferenced (Fn, Value, Deriviative);
   begin
      return 0.0;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Fn          : Polynomial_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real
   is
      A : Real_Vectors.Vector := Fn.A;
   begin
      for I in 1 .. Deriviative loop
         declare
            D : Real_Vectors.Vector;
         begin
            for J in 1 .. A.Last_Index loop
               D.Append (Real (J) * A.Element (I));
            end loop;
            A := D;
         end;
      end loop;

      declare
         X : Real := 1.0;
         Y : Real := 0.0;
      begin
         for K of A loop
            Y := Y + K * X;
            X := X * Value;
         end loop;
         return Y;
      end;
   end Evaluate;

   overriding function Evaluate
     (Fn          : Logarithmic_Utility_Function;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real
   is
      pragma Unreferenced (Deriviative);
   begin
      return Concorde.Elementary_Functions.Log (Value + Fn.X_Offset)
        * Fn.Scale;
   end Evaluate;

   -----------------
   -- Logarithmic --
   -----------------

   function Logarithmic
     (Zero_X : Real;
      One_X  : Real)
      return Utility_Function
   is
   begin
      return Fn : constant Logarithmic_Utility_Function :=
        Logarithmic_Utility_Function'
          (X_Offset => Zero_X + 1.0,
           Scale    =>
              1.0 / Concorde.Elementary_Functions.Log (One_X));
   end Logarithmic;

   ---------------
   -- Quadratic --
   ---------------

   function Quadratic
     (X1, Y1 : Real;
      X2, Y2 : Real;
      X3, Y3 : Real)
      return Utility_Function
   is
      C : constant Real :=
        (X1 * (Y3 - Y2) + X2 * (Y1 - Y3) + X3 * (Y2 - Y1))
          / ((X1 - X2) * (X1 - X3) * (X2 - X3));
      B : constant Real :=
        (Y2 - Y1) / (X2 - X1) - C * (X1 + X2);
      A : constant Real :=
        Y1 - C * X1 ** 2 - B * X1;
   begin
      return F : Polynomial_Utility_Function do
         F.A.Append (A);
         F.A.Append (B);
         F.A.Append (C);
      end return;
   end Quadratic;

   ----------
   -- Zero --
   ----------

   function Zero return Utility_Function is
   begin
      return Fn : Zero_Utility_Function;
   end Zero;

end Concorde.Utility;
