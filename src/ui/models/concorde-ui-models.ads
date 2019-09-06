private with Ada.Containers.Doubly_Linked_Lists;

package Concorde.UI.Models is

   type Model_Watcher is limited interface;

   procedure Model_Changed
     (Watcher : in out Model_Watcher)
   is abstract;

   type Root_Concorde_Model is abstract tagged private;

   function Title
     (Model : Root_Concorde_Model)
      return String
      is abstract;

   procedure Update
     (Model : in out Root_Concorde_Model)
   is null;

   procedure Add_Watcher
     (Model   : in out Root_Concorde_Model'Class;
      Watcher : not null access Model_Watcher'Class);

   procedure Notify_Changed
     (Model : in out Root_Concorde_Model);

   type Model_Type is access all Root_Concorde_Model'Class;

private

   type Model_Watcher_Access is
     access all Model_Watcher'Class;

   package Model_Watcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Model_Watcher_Access);

   type Root_Concorde_Model is abstract tagged
      record
         Watchers : Model_Watcher_Lists.List;
      end record;

end Concorde.UI.Models;
