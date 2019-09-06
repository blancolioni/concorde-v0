package body Concorde.UI.Models is

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (Model   : in out Root_Concorde_Model'Class;
      Watcher : not null access Model_Watcher'Class)
   is
   begin
      Model.Watchers.Append (Model_Watcher_Access (Watcher));
   end Add_Watcher;

   --------------------
   -- Notify_Changed --
   --------------------

   procedure Notify_Changed
     (Model : in out Root_Concorde_Model)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Changed;
      end loop;
   end Notify_Changed;

end Concorde.UI.Models;
