with Concorde.Handles.Installation;

package Concorde.Installations is

   function Describe
     (Installation : Concorde.Handles.Installation.Installation_Class)
      return String;

   procedure Log
     (Installation     : Concorde.Handles.Installation.Installation_Class;
      Message          : String);

end Concorde.Installations;
