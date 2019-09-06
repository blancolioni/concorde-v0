with Ada.Strings.Unbounded;

package body Concorde.Contexts.Errors is

   type Error_Context_Type is
     new Root_Context_Type with
      record
         Message : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Class
     (Context : Error_Context_Type)
      return String
   is ("error");

   overriding function Name
     (Context : Error_Context_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Context.Message));

   overriding function Is_Valid
     (Context : Error_Context_Type)
      return Boolean
   is (False);

   overriding procedure Get_Child_Contexts
     (Context  : Error_Context_Type;
      Children : in out Context_List'Class);

   -------------------
   -- Error_Context --
   -------------------

   function Error_Context
     (Message : String)
      return Context_Type
   is
   begin
      return Error_Context_Type'
        (Root_Context_Type with
           Message => Ada.Strings.Unbounded.To_Unbounded_String (Message));
   end Error_Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Error_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
   end Get_Child_Contexts;

end Concorde.Contexts.Errors;
