package Concorde.Symbols is

   type Symbol_Id is private;

   No_Symbol_Id : constant Symbol_Id;

   function Get_Symbol (Name : String) return Symbol_Id;
   function Get_Name (Symbol : Symbol_Id) return String;

   type Symbol_Id_Array is array (Positive range <>) of Symbol_Id;

   function No_Symbols return Symbol_Id_Array;

private

   type Symbol_Id is new Natural;
   No_Symbol_Id : constant Symbol_Id := 0;

end Concorde.Symbols;
