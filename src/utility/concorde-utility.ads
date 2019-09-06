private with Ada.Containers.Indefinite_Holders;

package Concorde.Utility is

   type Utility_Function_Interface is interface;

   subtype Utility_Function is Utility_Function_Interface'Class;

   function Evaluate
     (Fn          : Utility_Function_Interface;
      Value       : Real;
      Deriviative : Natural := 0)
      return Real
      is abstract;

--     function "+" (Left, Right : Utility_Function) return Utility_Function;
--     function "-" (Left, Right : Utility_Function) return Utility_Function;
--     function "*" (Left : Real;
--                   Right : Utility_Function)
--       return Utility_Function;
--     function "*" (Left  : Utility_Function;
--                   Right : Real)
--                   return Utility_Function;

   function Zero return Utility_Function;

   function Quadratic
     (X1, Y1 : Real;
      X2, Y2 : Real;
      X3, Y3 : Real)
      return Utility_Function;

   function Logarithmic
     (Zero_X : Real;
      One_X  : Real)
     return Utility_Function;

   type Utility_Function_Holder is tagged private;

   function Get (Holder : Utility_Function_Holder) return Utility_Function;
   function Put (Fn : Utility_Function) return Utility_Function_Holder;

private

   package Holders is new
     Ada.Containers.Indefinite_Holders (Utility_Function);

   type Utility_Function_Holder is
     new Holders.Holder with null record;

   function Get (Holder : Utility_Function_Holder) return Utility_Function
   is (Holder.Element);

   function Put (Fn : Utility_Function) return Utility_Function_Holder
   is (Utility_Function_Holder'
         (Holders.To_Holder (Fn) with null record));

end Concorde.Utility;
