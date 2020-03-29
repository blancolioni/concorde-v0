with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Numerics;

with Nazar.Colors;

with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Worlds;

with Concorde.Handles.Has_Color;
with Concorde.Handles.Terrain;

with Concorde.Db.Colony;
with Concorde.Db.World_Sector;

package body Concorde.UI.Models.World is

   Sqrt_2 : constant := 1.4142135623730950488016887242;

   package Vertex_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Worlds.Sector_Vertex_Array, Concorde.Worlds."=");

   type Sector_Model is
      record
         Owner    : Concorde.Handles.Faction.Faction_Handle;
         Sector   : Concorde.Db.World_Sector_Reference;
         Centre   : Concorde.Worlds.Sector_Vertex;
         Boundary : Vertex_Holders.Holder;
         Color    : Nazar.Colors.Nazar_Color;
      end record;

   package Sector_Model_Vectors is
      new Ada.Containers.Vectors (Positive, Sector_Model);

   type World_Model_Type is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Sectors : Sector_Model_Vectors.Vector;
      end record;

   type Map_Point is
      record
         X, Y : Nazar.Nazar_Float;
      end record;

   function Project
     (Latitude   : Real;
      Longitude  : Real;
      Positive_X : Boolean)
      return Map_Point;

   procedure Draw_World
     (Model  : in out World_Model_Type'Class;
      Centre : Concorde.Worlds.Sector_Vertex);

   function To_Nazar_Color
     (Has_Color : Concorde.Handles.Has_Color.Has_Color_Class)
      return Nazar.Colors.Nazar_Color
   is (Red   => Nazar.Nazar_Unit_Float (Has_Color.Red),
       Green => Nazar.Nazar_Unit_Float (Has_Color.Green),
       Blue  => Nazar.Nazar_Unit_Float (Has_Color.Blue),
       Alpha => 1.0);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   ----------------
   -- Draw_World --
   ----------------

   procedure Draw_World
     (Model  : in out World_Model_Type'Class;
      Centre : Concorde.Worlds.Sector_Vertex)
   is
      use Ada.Numerics;
      use Concorde.Elementary_Functions;
      Long_0 : constant Real :=
                 Arctan (Centre.Y, Centre.X);
   begin

      Concorde.Logging.Log
        ("model", "world", "draw",
         "long-0 = "
         & Image (180.0 * Long_0 / Pi));

      for Fill in reverse Boolean loop

         Model.Set_Fill (Fill);

         if not Fill then
            Model.Set_Color ((0.75, 0.75, 0.75, 1.0));
         end if;

         for Sector of Model.Sectors loop
            declare
               Color       : constant Nazar.Colors.Nazar_Color :=
                               Sector.Color;
               Boundary    : constant Concorde.Worlds.Sector_Vertex_Array :=
                               Sector.Boundary.Element;
               Centre_Long : Real :=
                               Arctan (Sector.Centre.Y, Sector.Centre.X)
                               - Long_0;
               First       : Boolean := True;
               First_Point : Map_Point;
            begin
               if Centre_Long < -Pi then
                  Centre_Long := Centre_Long + 2.0 * Pi;
               elsif Centre_Long > Pi then
                  Centre_Long := Centre_Long - 2.0 * Pi;
               end if;

--                 Concorde.Logging.Log
--                   (Actor    => "model",
--                    Location => "world",
--                    Category => "draw",
--                    Message  =>
--                      "centre = ("
--                    & Image (Sector.Centre.X)
--                    & ","
--                    & Image (Sector.Centre.Y)
--                    & ","
--                    & Image (Sector.Centre.Z)
--                    & "); lat/long = ("
--                    & Image (Arcsin (Sector.Centre.Z) * 180.0 / Pi)
--                    & ","
--                    & Image (Centre_Long)
--                    & ")");

               if Fill then
                  Model.Set_Color (Color);
               end if;

               for Pt of Boundary loop
                  declare
                     Long    : constant Real :=
                                 Arctan (Pt.Y, Pt.X) - Long_0;
                     Lat     : constant Real := Arcsin (Pt.Z);
                     Map_Pt  : constant Map_Point :=
                                 Project (Lat, Long, Centre_Long > 0.0);
                  begin
--                       Concorde.Logging.Log
--                         (Actor    => "model",
--                          Location => "world",
--                          Category => "draw",
--                          Message  =>
--                            "    pt = ("
--                          & Image (Pt.X)
--                          & ","
--                          & Image (Pt.Y)
--                          & ","
--                          & Image (Pt.Z)
--                          & "); lat/long = ("
--                          & Image (Lat * 180.0 / Pi)
--                          & ","
--                          & Image (Long)
--                          & ")"
--                          & "; map pt = ("
--                          & Image (Real (Map_Pt.X))
--                          & ","
--                          & Image (Real (Map_Pt.Y))
--                          & ")");
--
                     if First then
                        Model.Move_To (Map_Pt.X, Map_Pt.Y);
                        First := False;
                        First_Point := Map_Pt;
                     else
                        Model.Line_To (Map_Pt.X, Map_Pt.Y);
                     end if;
                  end;
               end loop;
               Model.Line_To (First_Point.X, First_Point.Y);
               Model.Render;
            end;
         end loop;
      end loop;
   end Draw_World;

   -------------
   -- Project --
   -------------

   function Project
     (Latitude   : Real;
      Longitude  : Real;
      Positive_X : Boolean)
      return Map_Point
   is
      use Nazar;
      use Concorde.Elementary_Functions;

      Pi : constant := Ada.Numerics.Pi;
      Theta : Real := Latitude;
      Sin_Lat : constant Real := Sin (Latitude);
      Pi_Sin_Lat : constant Real := Pi * Sin_Lat;
      Abs_Lat    : constant Nazar_Float :=
                     Nazar_Float (abs Latitude);
      K : constant := 2.0 * Sqrt_2 / Pi;

   begin
      if Theta > -2.0 * Pi and then Theta < 2.0 * Pi then
         loop
            declare
               Prev : constant Real := Theta;
            begin
               Theta :=
                 Theta -
                   (2.0 * Theta + Sin (2.0 * Theta) - Pi_Sin_Lat)
                     / (2.0 + 2.0 * Cos (2.0 * Theta));

               exit when abs (Prev - Theta) < 1.0e-9;
            end;
         end loop;
      end if;

      declare
         Cos_Theta : constant Nazar_Float :=
                       Nazar_Float (Cos (Theta));
         X         : Nazar_Float :=
                       Nazar_Float
                         (K * Longitude) * Cos_Theta;
         Y         : constant Nazar_Float :=
                       Nazar_Float (Sqrt_2 * Sin (Theta));

      begin
         if Positive_X and then X < -Pi / 2.0 + Abs_Lat then
            X := X + 4.0 * Sqrt_2 * Cos_Theta;
         elsif not Positive_X and then X > Pi / 2.0 - Abs_Lat then
            X := X - 4.0 * Sqrt_2 * Cos_Theta;
         end if;

         return (X, Y);
      end;

   end Project;

   -----------------
   -- World_Model --
   -----------------

   function World_Model
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      World   : Concorde.Handles.World.World_Class)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      pragma Unreferenced (Faction);
      Result : World_Model_Type;
   begin
      for Sector of
        Concorde.Db.World_Sector.Select_By_World (World.Reference_World)
      loop
         declare
            Ref : constant Concorde.Db.World_Sector_Reference :=
                    Sector.Get_World_Sector_Reference;
            Terrain : constant Concorde.Handles.Terrain.Terrain_Handle :=
                        Concorde.Handles.Terrain.Get (Sector.Terrain);
            Faction : constant Concorde.Handles.Faction.Faction_Handle :=
                        Concorde.Handles.Faction.Get (Sector.Faction);
            Color   : constant Nazar.Colors.Nazar_Color :=
                        (if Faction.Has_Element
                         then To_Nazar_Color (Faction)
                         else To_Nazar_Color (Terrain));
         begin
            Result.Sectors.Append
              (Sector_Model'
                 (Sector   => Ref,
                  Owner    => Concorde.Handles.Faction.Get (Sector.Faction),
                  Centre   => Concorde.Worlds.Get_Centre (Ref),
                  Boundary =>
                    Vertex_Holders.To_Holder
                      (Concorde.Worlds.Get_Vertices (Ref)),
                  Color    => Color));
         end;
      end loop;

      declare
         Colony : constant Concorde.Db.Colony.Colony_Type :=
                    Concorde.Db.Colony.First_By_World
                      (World.Reference_World);
      begin
         Result.Draw_World
           (Centre =>
              Concorde.Worlds.Get_Centre (Colony.Capital));
      end;

      declare
         use Nazar;
      begin
         Result.Set_Bounding_Box
           (Box => (-3.0, -2.0, 6.0, 4.0));
      end;
      return new World_Model_Type'(Result);
   end World_Model;

end Concorde.UI.Models.World;
