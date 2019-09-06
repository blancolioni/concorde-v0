with Concorde.Color;

with Concorde.Db;

package Concorde.Terrain is

   function Name
     (Terrain : Concorde.Db.Terrain_Reference)
      return String;

   function Color
     (Terrain : Concorde.Db.Terrain_Reference)
      return Concorde.Color.Concorde_Color;

   function Is_Water
     (Terrain : Concorde.Db.Terrain_Reference)
      return Boolean;

   function Hazard
     (Terrain : Concorde.Db.Terrain_Reference)
      return Unit_Real;

end Concorde.Terrain;
