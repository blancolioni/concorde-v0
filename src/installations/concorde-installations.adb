with Concorde.Logging;
with Concorde.Db;

package body Concorde.Installations is

   --------------
   -- Describe --
   --------------

   function Describe
     (Installation : Concorde.Handles.Installation.Installation_Class)
      return String
   is
   begin
      return Installation.Facility.Tag
        & Concorde.Db.To_String (Installation.Reference_Installation);
   end Describe;

   ---------
   -- Log --
   ---------

   procedure Log
     (Installation : Concorde.Handles.Installation.Installation_Class;
      Message      : String)
   is
   begin
      Concorde.Logging.Log (Describe (Installation), Message);
   end Log;

end Concorde.Installations;
