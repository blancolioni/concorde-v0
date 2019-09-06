package Concorde.Contexts.System is

   function System_Context return Context_Type;

private

   type System_File_Context_Type is
     abstract new Root_Context_Type with
      record
         null;
      end record;

   function Contents (File : System_File_Context_Type) return String
                      is abstract;

   overriding procedure Get_Child_Contexts
     (Context  : System_File_Context_Type;
      Children : in out Context_List'Class);

   overriding function Class
     (Context : System_File_Context_Type)
      return String
   is ("system-file");

   overriding function Name
     (Context : System_File_Context_Type)
      return String
      is abstract;

   overriding procedure Iterate_Content_Lines
     (Context : System_File_Context_Type;
      Process : not null access
        procedure (Line : String));

end Concorde.Contexts.System;
