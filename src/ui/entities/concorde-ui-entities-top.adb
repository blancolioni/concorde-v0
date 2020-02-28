with Concorde.UI.Entities.Directories;
with Concorde.UI.Entities.Files;

with Concorde.Version;

package body Concorde.UI.Entities.Top is

   -----------------------
   -- Create_Top_Entity --
   -----------------------

   function Create_Top_Entity
     return Entity_Reference
   is
      Top : Nazar.Interfaces.Hierarchy.Node_Reference_Class :=
        Directories.Create_Directory_Node;
   begin
      Top.Update.Bind_Child
        ("version",
         Files.File_Node (Concorde.Version.Version_String));
      return Read_Only_Entity_Reference'
        (RW_Reference => Node_Reference_Holders.To_Holder (Top));
   end Create_Top_Entity;

end Concorde.UI.Entities.Top;
