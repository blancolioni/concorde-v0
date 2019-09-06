with Concorde.Calendar;

with Concorde.Contexts.Containers;

package body Concorde.Contexts.System is

   type Date_System_File is
     new System_File_Context_Type with
      record
         null;
      end record;

   overriding function Name (File : Date_System_File) return String;
   overriding function Contents (File : Date_System_File) return String;

   procedure Scan_System_Contexts
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type));

   package System_Containers is
     new Concorde.Contexts.Containers
       (Container_Name    => "system",
        Context_Reference => Boolean,
        Iterate_Children  => Scan_System_Contexts);

   --------------
   -- Contents --
   --------------

   overriding function Contents (File : Date_System_File) return String is
      pragma Unreferenced (File);
   begin
      return Concorde.Calendar.Image (Concorde.Calendar.Clock, True);
   end Contents;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : System_File_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
   end Get_Child_Contexts;

   ---------------------------
   -- Iterate_Content_Lines --
   ---------------------------

   overriding procedure Iterate_Content_Lines
     (Context : System_File_Context_Type;
      Process : not null access
        procedure (Line : String))
   is
   begin
      Process (System_File_Context_Type'Class (Context).Contents);
   end Iterate_Content_Lines;

   ----------
   -- Name --
   ----------

   overriding function Name (File : Date_System_File) return String is
      pragma Unreferenced (File);
   begin
      return "date";
   end Name;

   --------------------------
   -- Scan_System_Contexts --
   --------------------------

   procedure Scan_System_Contexts
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type))
   is
      Date : Date_System_File;
   begin
      Process (Date);
   end Scan_System_Contexts;

   --------------------
   -- System_Context --
   --------------------

   function System_Context return Context_Type is
   begin
      return System_Containers.Container_Context (True);
   end System_Context;

end Concorde.Contexts.System;
