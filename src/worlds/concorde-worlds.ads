private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Quantities;
with Concorde.Spheres;
with Concorde.Trigonometry;

with Concorde.Handles.Faction;
with Concorde.Handles.Resource;
with Concorde.Handles.Terrain;
with Concorde.Handles.World;
with Concorde.Handles.World_Sector;

package Concorde.Worlds is

   subtype World_Class is Concorde.Handles.World.World_Class;
   subtype World_Handle is Concorde.Handles.World.World_Handle;

   type World_Selection is tagged private;

   function Is_Empty (Selection : World_Selection'Class) return Boolean;

   procedure Clear (Selection : in out World_Selection'Class);
   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : World_Class);

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : World_Class)
      return Boolean);

   type World_Array is
     array (Positive range <>) of World_Handle;

   function Get_Worlds (Selection : World_Selection'Class) return World_Array;

   function Is_Terrestrial
     (World : World_Class)
      return Boolean;

   type World_Sector_Array is
     array (Positive range <>)
     of Concorde.Handles.World_Sector.World_Sector_Handle;

   function Get_Neighbours
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return World_Sector_Array;

   function Get_Bearing
     (From, To : Concorde.Handles.World_Sector.World_Sector_Class)
      return Concorde.Trigonometry.Angle;

   function Get_Distance
     (From, To : Concorde.Handles.World_Sector.World_Sector_Class)
      return Non_Negative_Real;

   procedure Circular_Scan
     (Start : Concorde.Handles.World_Sector.World_Sector_Class;
      Process : not null access
        function (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Boolean);

   --  function should return False if scanning is to stop

   procedure Scan_Surface
     (World : World_Class;
      Process : not null access
        procedure (Sector : Concorde.Handles.World_Sector.World_Sector_Class));

   function Best_Sector
     (World : World_Class;
      Score : not null access
        function (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Real)
      return Concorde.Handles.World_Sector.World_Sector_Class;

   function Find_Sector
     (World : World_Class;
      Test : not null access
        function (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Boolean)
      return Concorde.Handles.World_Sector.World_Sector_Class;

   function Get_World
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return World_Class;

   function Get_Owner
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Concorde.Handles.Faction.Faction_Class;

   procedure Set_Owner
     (Sector  : Concorde.Handles.World_Sector.World_Sector_Class;
      Faction : Concorde.Handles.Faction.Faction_Handle);

   procedure Scan_Resources
     (Sector  : Concorde.Handles.World_Sector.World_Sector_Class;
      Process : not null access
        procedure (Resource : Concorde.Handles.Resource.Resource_Class;
                   Concentration : Unit_Real;
                   Difficulty    : Unit_Real;
                   Available     : Concorde.Quantities.Quantity_Type));

   subtype Sector_Vertex is Concorde.Spheres.Surface_Point;

   type Sector_Position is
      record
         Latitude  : Real;
         Longitude : Real;
      end record;

   function Get_Centre
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Sector_Vertex;

   function Get_Centre
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Sector_Position;

   function Get_Terrain
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Concorde.Handles.Terrain.Terrain_Class;

   type Sector_Vertex_Array is array (Positive range <>) of Sector_Vertex;

   function Get_Vertices
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Sector_Vertex_Array;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (World_Handle, Concorde.Handles.World."=");

   type World_Selection is tagged
      record
         List : World_Lists.List;
      end record;

end Concorde.Worlds;
