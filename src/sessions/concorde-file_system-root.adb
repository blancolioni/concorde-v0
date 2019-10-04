package body Concorde.File_System.Root is

   ----------------------
   -- System_Root_Node --
   ----------------------

   function System_Root_Node_Id return Node_Id is
   begin
      return Root_Filesystem.Get_Root_Node_Id;
   end System_Root_Node_Id;

end Concorde.File_System.Root;
