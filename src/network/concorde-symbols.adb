with Ada.Containers.Indefinite_Vectors;
with WL.String_Maps;

package body Concorde.Symbols is

   subtype Real_Symbol_Id is Symbol_Id range 1 .. Symbol_Id'Last;

   package Symbol_Id_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Symbol_Id, String);

   package Symbol_Id_Maps is
     new WL.String_Maps (Real_Symbol_Id);

   Symbol_Id_Vector : Symbol_Id_Vectors.Vector;
   Symbol_Id_Map    : Symbol_Id_Maps.Map;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Symbol : Symbol_Id) return String is
   begin
      return (if Symbol = No_Symbol_Id then ""
              else Symbol_Id_Vector.Element (Symbol));
   end Get_Name;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Name : String) return Symbol_Id is
   begin
      if Name = "" then
         return No_Symbol_Id;
      elsif not Symbol_Id_Map.Contains (Name) then
         Symbol_Id_Vector.Append (Name);
         Symbol_Id_Map.Insert (Name, Symbol_Id_Vector.Last_Index);
      end if;
      return Symbol_Id_Map.Element (Name);
   end Get_Symbol;

   ----------------
   -- No_Symbols --
   ----------------

   function No_Symbols return Symbol_Id_Array is
   begin
      return Arr : Symbol_Id_Array (1 .. 0);
   end No_Symbols;

end Concorde.Symbols;
