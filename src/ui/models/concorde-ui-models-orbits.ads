with Concorde.Db;

with Concorde.UI.Models.Tables;

package Concorde.UI.Models.Orbits is

   type Root_Orbiting_Ship_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with private;

   type Orbiting_Ship_Model is access all Root_Orbiting_Ship_Model'Class;

   function Create
     (World : Concorde.Db.World_Reference)
      return Orbiting_Ship_Model;

private

   type Root_Orbiting_Ship_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with
      record
         World : Concorde.Db.World_Reference;
      end record;

end Concorde.UI.Models.Orbits;
