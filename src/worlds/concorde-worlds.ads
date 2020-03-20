private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Quantities;

with Concorde.Db;
with Concorde.Db.World_Sector;

package Concorde.Worlds is

   type World_Selection is tagged private;

   function Is_Empty (Selection : World_Selection'Class) return Boolean;

   procedure Clear (Selection : in out World_Selection'Class);
   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Concorde.Db.World_Reference);

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Concorde.Db.World_Reference)
      return Boolean);

   type World_Array is
     array (Positive range <>) of Concorde.Db.World_Reference;

   function Get_Worlds (Selection : World_Selection'Class) return World_Array;

   function Star_System
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.Star_System_Reference;

   function Name
     (World : Concorde.Db.World_Reference)
      return String;

   function Mass
     (World : Concorde.Db.World_Reference)
      return Non_Negative_Real;

   function Radius
     (World : Concorde.Db.World_Reference)
      return Non_Negative_Real;

   function Climate
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.World_Climate;

   function Habitability
     (World : Concorde.Db.World_Reference)
      return Unit_Real;

   function Market
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.Market_Reference;

   function Is_Terrestrial
     (World : Concorde.Db.World_Reference)
      return Boolean;

   type World_Sector_Array is
     array (Positive range <>) of Concorde.Db.World_Sector_Reference;

   function Get_Neighbours
     (Sector : Concorde.Db.World_Sector_Reference)
      return World_Sector_Array;

   procedure Circular_Scan
     (Start : Concorde.Db.World_Sector_Reference;
      Process : not null access
        function (Sector : Concorde.Db.World_Sector_Reference)
      return Boolean);

   --  function should return False if scanning is to stop

   procedure Scan_Surface
     (World : Concorde.Db.World_Reference;
      Process : not null access
        procedure (Sector : Concorde.Db.World_Sector_Reference));

   function Best_Sector
     (World : Concorde.Db.World_Reference;
      Score : not null access
        function (Sector : Concorde.Db.World_Sector.World_Sector_Type)
      return Real)
      return Concorde.Db.World_Sector_Reference;

   function Find_Sector
     (World : Concorde.Db.World_Reference;
      Test : not null access
        function (Sector : Concorde.Db.World_Sector.World_Sector_Type)
      return Boolean)
      return Concorde.Db.World_Sector_Reference;

   function Get_World
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.World_Reference;

   function Get_Owner
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.Faction_Reference;

   procedure Set_Owner
     (Sector  : Concorde.Db.World_Sector_Reference;
      Faction : Concorde.Db.Faction_Reference);

   procedure Scan_Resources
     (Sector  : Concorde.Db.World_Sector_Reference;
      Process : not null access
        procedure (Resource : Concorde.Db.Resource_Reference;
                   Concentration : Unit_Real;
                   Difficulty    : Unit_Real;
                   Available     : Concorde.Quantities.Quantity_Type));

   type Sector_Vertex is
      record
         X, Y, Z : Signed_Unit_Real;
      end record;

   function Get_Centre
     (Sector : Concorde.Db.World_Sector_Reference)
      return Sector_Vertex;

   function Get_Terrain
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.Terrain_Reference;

   type Sector_Vertex_Array is array (Positive range <>) of Sector_Vertex;

   function Get_Vertices
     (Sector : Concorde.Db.World_Sector_Reference)
      return Sector_Vertex_Array;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Db.World_Reference, Concorde.Db."=");

   type World_Selection is tagged
      record
         List : World_Lists.List;
      end record;

end Concorde.Worlds;
