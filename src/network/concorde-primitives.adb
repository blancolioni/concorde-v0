package body Concorde.Primitives is

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Name        : String;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class
   is
   begin
      if not Map.Contains (Name) then
         raise Constraint_Error with "undefined: " & Name;
      end if;

      declare
         Primitive : constant Primitive_Interface'Class :=
                       Map.Element (Name);
      begin
         if Arguments.Last_Index < Primitive.Minimum_Argument_Count then
            raise Constraint_Error with
              "too few arguments to " & Name & "; expected"
              & Primitive.Minimum_Argument_Count'Image
              & " but found"
              & Arguments.Last_Index'Image;
         end if;

         if Arguments.Last_Index > Primitive.Maximum_Argument_Count then
            raise Constraint_Error with
              "too many arguments to " & Name & "; expected"
              & Primitive.Maximum_Argument_Count'Image
              & " but found"
              & Arguments.Last_Index'Image;
         end if;

         return Primitive.Evaluate (Arguments);
      end;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Primitive   : Binary_Primitive;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class
   is
   begin
      return Binary_Primitive'Class (Primitive)
        .Evaluate (Arguments.Element (1), Arguments.Element (2));
   end Evaluate;

end Concorde.Primitives;
