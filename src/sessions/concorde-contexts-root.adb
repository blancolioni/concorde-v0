with WL.String_Maps;

with Concorde.Contexts.Faction_Container;
with Concorde.Contexts.Galaxy;
with Concorde.Contexts.Markets;
with Concorde.Contexts.System;

package body Concorde.Contexts.Root is

   package Context_Maps is
     new WL.String_Maps (Context_Type);

   Top_Level_Context_Map : Context_Maps.Map;

   procedure Check_Top_Level_Contexts;

   type Top_Context_Type is
     new Root_Context_Type with null record;

   overriding function Class
     (Context : Top_Context_Type)
      return String
   is ("root");

   overriding function Name
     (Context : Top_Context_Type)
      return String
   is ("");

   overriding function Is_Root
     (Context : Top_Context_Type)
      return Boolean
   is (True);

   overriding function Is_Valid
     (Context : Top_Context_Type)
      return Boolean
   is (True);

   overriding procedure Get_Child_Contexts
     (Context  : Top_Context_Type;
      Children : in out Context_List'Class);

   ------------------------------
   -- Check_Top_Level_Contexts --
   ------------------------------

   procedure Check_Top_Level_Contexts is

      procedure Add (Context : Context_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Context : Context_Type) is
      begin
         Top_Level_Context_Map.Insert (Context.Name, Context);
      end Add;

   begin
      if Top_Level_Context_Map.Is_Empty then
         Add (Concorde.Contexts.Faction_Container.Faction_Container_Context);
         Add (Concorde.Contexts.Galaxy.Galaxy_Context);
         Add (Concorde.Contexts.Markets.Top_Level_Container);
         Add (Concorde.Contexts.System.System_Context);
      end if;
   end Check_Top_Level_Contexts;

   ------------------
   -- Get_Children --
   ------------------

   overriding procedure Get_Child_Contexts
     (Context  : Top_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Check_Top_Level_Contexts;
      Children.Clear;
      for Position in Top_Level_Context_Map.Iterate loop
         Children.Append (Context_Maps.Element (Position));
      end loop;
   end Get_Child_Contexts;

   -------------------
   -- Root_Context --
   -------------------

   function Root_Context
     return Context_Type
   is
   begin
      return Context : Top_Context_Type;
   end Root_Context;

end Concorde.Contexts.Root;
