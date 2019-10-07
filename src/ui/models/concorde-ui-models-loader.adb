with WL.String_Maps;

with Concorde.UI.Models.Shell;

package body Concorde.UI.Models.Loader is

   type Model_Creator is access
     function (Argument : String) return Root_Concorde_Model'Class;

   package Model_Maps is
     new WL.String_Maps (Model_Creator);

   Map : Model_Maps.Map;

   procedure Check_Map;

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is

      procedure Add
        (Name    : String;
         Creator : Model_Creator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name    : String;
         Creator : Model_Creator)
      is
      begin
         Map.Insert (Name, Creator);
      end Add;

   begin
      if Map.Is_Empty then
         Add ("shell", Concorde.UI.Models.Shell.Shell_Model'Access);
      end if;
   end Check_Map;

   ------------
   -- Exists --
   ------------

   function Exists (Model_Name : String) return Boolean is
   begin
      Check_Map;
      return Map.Contains (Model_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Model_Name : String;
      Argument   : String)
      return Root_Concorde_Model'Class
   is
   begin
      Check_Map;
      return Map.Element (Model_Name) (Argument);
   end Get;

end Concorde.UI.Models.Loader;
