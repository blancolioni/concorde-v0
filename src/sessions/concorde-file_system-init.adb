with Concorde.File_System.Directories;
with Concorde.File_System.Proc;

package body Concorde.File_System.Init is

   ----------------------------
   -- Initialize_File_System --
   ----------------------------

   procedure Initialize_File_System is
      Root_Id : constant Node_Id := Create (Directories.Directory_Node);
      pragma Assert (Root_Id = Real_Node_Id'First);
      Root    : constant access Node_Interface'Class :=
        Update (Root_Id);
      pragma Assert (not Root.Is_Leaf);
   begin

      Root.Bind_Child
        (Name => "dev",
         Child => Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "home",
         Child => Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "log",
         Child => Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "proc",
         Child => Create (Proc.Create_Proc_File_System));
      Root.Bind_Child
        (Name => "etc",
         Child => Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "root",
         Child => Create (Directories.Directory_Node));
      Root.Bind_Child
        (Name => "tmp",
         Child => Create (Directories.Directory_Node));
   end Initialize_File_System;

end Concorde.File_System.Init;
