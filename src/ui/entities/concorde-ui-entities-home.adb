with Concorde.UI.Entities.Directories;

with Concorde.Db.Faction;

package body Concorde.UI.Entities.Home is

   ---------------
   -- Home_Node --
   ---------------

   function Home_Node return Entity_Reference is
   begin
      return Node : Entity_Reference :=
        Directories.Create_Directory_Node
      do
         for Faction of Concorde.Db.Faction.Scan_By_Name loop
            Node.Update.Bind_Child
              (Faction.Name, Directories.Create_Directory_Node);
         end loop;
      end return;
   end Home_Node;

end Concorde.UI.Entities.Home;
