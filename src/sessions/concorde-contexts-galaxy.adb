with Concorde.Db.Star_System;

with Concorde.Contexts.Star_Systems;

package body Concorde.Contexts.Galaxy is

   type Galaxy_Context_Type is
     new Root_Context_Type with null record;

   overriding function Class
     (Context : Galaxy_Context_Type)
      return String
   is ("container[star-system]");

   overriding function Name
     (Context : Galaxy_Context_Type)
      return String
   is ("systems");

   overriding function Is_Valid
     (Context : Galaxy_Context_Type)
      return Boolean
   is (True);

   overriding procedure Get_Child_Contexts
     (Context  : Galaxy_Context_Type;
      Children : in out Context_List'Class);

   --------------------
   -- Galaxy_Context --
   --------------------

   function Galaxy_Context
      return Context_Type
   is
   begin
      return Context : Galaxy_Context_Type;
   end Galaxy_Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Galaxy_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
      for Star_System of Concorde.Db.Star_System.Scan_By_Name loop
         Children.Append
           (Concorde.Contexts.Star_Systems.Star_System_Context
              (Star_System.Get_Star_System_Reference));
      end loop;
   end Get_Child_Contexts;

end Concorde.Contexts.Galaxy;
