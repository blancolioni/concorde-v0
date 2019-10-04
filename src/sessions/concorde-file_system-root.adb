package body Concorde.File_System.Root is

   ----------------------
   -- System_Root_Node --
   ----------------------

   function System_Root_Node_Id return Node_Id is
   begin
      return Real_Node_Id'First;
   end System_Root_Node_Id;

end Concorde.File_System.Root;
