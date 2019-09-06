with Concorde.Worlds;

package body Concorde.Contexts.Worlds is

   type World_Context_Type is
     new Root_Context_Type with
      record
         World : Concorde.Db.World_Reference;
      end record;

   overriding function Is_Valid
     (Context : World_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : World_Context_Type;
      Children : in out Context_List'Class);

   overriding function Class
     (Context : World_Context_Type)
      return String
   is ("world");

   overriding function Name
     (Context : World_Context_Type)
      return String
   is (Concorde.Worlds.Name (Context.World));

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : World_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
   end Get_Child_Contexts;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Context : World_Context_Type)
      return Boolean
   is
      use type Concorde.Db.World_Reference;
   begin
      return Context.World /= Concorde.Db.Null_World_Reference;
   end Is_Valid;

   -------------------
   -- World_Context --
   -------------------

   function World_Context
     (World : Concorde.Db.World_Reference)
      return Context_Type
   is
   begin
      return World_Context_Type'
        (Root_Context_Type with World => World);
   end World_Context;

end Concorde.Contexts.Worlds;
