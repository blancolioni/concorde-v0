private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Db;

with Concorde.Worlds;

with Concorde.UI.Models.Tables;

package Concorde.UI.Models.World is

   type Root_World_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with private;

   function World
     (Model : Root_World_Model'Class)
      return Concorde.Db.World_Reference;

   procedure Scan_Surface
     (Model   : Root_World_Model'Class;
      Process : not null access
        procedure (Sector   : Concorde.Db.World_Sector_Reference;
                   Centre_X : Concorde.Worlds.Sector_Vertex;
                   Boundary : Concorde.Worlds.Sector_Vertex_Array));

   type World_Model is
     access all Root_World_Model'Class;

   function Create
     (World : Concorde.Db.World_Reference)
      return World_Model;

private

   package Sector_Boundary_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Worlds.Sector_Vertex_Array,
        Concorde.Worlds."=");

   type World_Sector_Type is
      record
         Reference : Concorde.Db.World_Sector_Reference;
         Owner     : Concorde.Db.Faction_Reference;
         Boundary  : Sector_Boundary_Holders.Holder;
         Centre    : Concorde.Worlds.Sector_Vertex;
      end record;

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Sector_Type);

   type Root_World_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with
      record
         Reference : Concorde.Db.World_Reference;
         Sectors   : World_Sector_Lists.List;
      end record;

   function World
     (Model : Root_World_Model'Class)
      return Concorde.Db.World_Reference
   is (Model.Reference);

end Concorde.UI.Models.World;
