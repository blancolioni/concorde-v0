package body Concorde.Contexts.Containers is

   type Container_Context_Type is
     new Root_Context_Type with
      record
         Reference : Context_Reference;
      end record;

   overriding function Class
     (Context : Container_Context_Type)
      return String
   is ("container[" & Container_Name & "]");

   overriding function Name
     (Context : Container_Context_Type)
      return String
   is (Container_Name);

   overriding function Is_Valid
     (Context : Container_Context_Type)
      return Boolean
   is (True);

   overriding procedure Get_Child_Contexts
     (Context  : Container_Context_Type;
      Children : in out Context_List'Class);

   -----------------------
   -- Container_Context --
   -----------------------

   function Container_Context
     (Reference : Context_Reference)
      return Context_Type
   is
   begin
      return Container_Context_Type'
        (Reference => Reference);
   end Container_Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Container_Context_Type;
      Children : in out Context_List'Class)
   is
      procedure Add (Child : Context_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Child : Context_Type) is
      begin
         Children.Append (Child);
      end Add;

   begin
      Children.Clear;
      Iterate_Children (Context.Reference, Add'Access);
   end Get_Child_Contexts;

end Concorde.Contexts.Containers;
