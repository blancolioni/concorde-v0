with WL.Localisation;

with Concorde.Db.Terrain;

package body Concorde.Terrain is

   -----------
   -- Color --
   -----------

   function Color
     (Terrain : Concorde.Db.Terrain_Reference)
      return Concorde.Color.Concorde_Color
   is
      Rec : constant Concorde.Db.Terrain.Terrain_Type :=
              Concorde.Db.Terrain.Get (Terrain);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   ------------
   -- Hazard --
   ------------

   function Hazard
     (Terrain : Concorde.Db.Terrain_Reference)
      return Unit_Real
   is (Concorde.Db.Terrain.Get (Terrain).Hazard);

   ----------
   -- Name --
   ----------

   function Name
     (Terrain : Concorde.Db.Terrain_Reference)
      return String
   is (WL.Localisation.Local_Text
       (Concorde.Db.Terrain.Get (Terrain).Tag));

   --------------
   -- Is_Water --
   --------------

   function Is_Water
     (Terrain : Concorde.Db.Terrain_Reference)
      return Boolean
   is (Concorde.Db.Terrain.Get (Terrain).Is_Water);

end Concorde.Terrain;
