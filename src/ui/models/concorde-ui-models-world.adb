with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Numerics;

with Nazar.Colors;

with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Worlds;

with Concorde.Db.Colony;
with Concorde.Db.Terrain;
with Concorde.Db.World_Sector;

package body Concorde.UI.Models.World is

   Sqrt_2 : constant := 1.4142135623730950488016887242;

   package Vertex_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Worlds.Sector_Vertex_Array, Concorde.Worlds."=");

   type Sector_Model is
      record
         Owned    : Boolean;
         Sector   : Concorde.Db.World_Sector_Reference;
         Centre   : Concorde.Worlds.Sector_Vertex;
         Boundary : Vertex_Holders.Holder;
         R, G, B  : Unit_Real;
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
     (Latitude  : Real;
      Longitude : Real)
      return Map_Point;

   procedure Draw_World
     (Model  : in out World_Model_Type'Class;
      Centre : Concorde.Worlds.Sector_Vertex);

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

      for Sector of Model.Sectors loop
         declare
            Color : constant Nazar.Colors.Nazar_Color :=
                      (Nazar.Nazar_Unit_Float (Sector.R),
                       Nazar.Nazar_Unit_Float (Sector.G),
                       Nazar.Nazar_Unit_Float (Sector.B),
                       1.0);
            Boundary : constant Concorde.Worlds.Sector_Vertex_Array :=
                         Sector.Boundary.Element;
            Centre_Long : Real :=
                            Arctan (Sector.Centre.Y, Sector.Centre.X)
                            - Long_0;
            First    : Boolean := True;
         begin
            if Centre_Long < -Pi then
               Centre_Long := Centre_Long + 2.0 * Pi;
            elsif Centre_Long > Pi then
               Centre_Long := Centre_Long - 2.0 * Pi;
            end if;

            Concorde.Logging.Log
              (Actor    => "model",
               Location => "world",
               Category => "draw",
               Message  =>
                 "centre = ("
               & Image (Sector.Centre.X)
               & ","
               & Image (Sector.Centre.Y)
               & ","
               & Image (Sector.Centre.Z)
               & "); lat/long = ("
               & Image (Arcsin (Sector.Centre.Z) * 180.0 / Pi)
               & ","
               & Image (Centre_Long)
               & ")");

            if True
              or else (Centre_Long in -Pi / 2.0 .. Pi / 2.0
                       and then Sector.Centre.Z >= 0.0)
            then
               Model.Set_Color (Color);
               Model.Set_Fill (True);
               for Pt of Boundary loop
                  declare
                     use Nazar;
                     Long   : constant Real :=
                                Arctan (Pt.Y, Pt.X) - Long_0;
                     Lat    : constant Real := Arcsin (Pt.Z);
                     Cos_Lat : constant Nazar_Float :=
                                 Nazar_Float (Cos (Lat));
                     Map_Pt : Map_Point :=
                                Project (Lat, Long);
                  begin
                     Concorde.Logging.Log
                       (Actor    => "model",
                        Location => "world",
                        Category => "draw",
                        Message  =>
                          "    pt = ("
                        & Image (Pt.X)
                        & ","
                        & Image (Pt.Y)
                        & ","
                        & Image (Pt.Z)
                        & "); lat/long = ("
                        & Image (Lat * 180.0 / Pi)
                        & ","
                        & Image (Long)
                        & ")"
                        & "; map pt = ("
                        & Image (Real (Map_Pt.X))
                        & ","
                        & Image (Real (Map_Pt.Y))
                        & ")");

                     if Centre_Long < 0.0
                       and then Real (Map_Pt.X) > Pi / 2.0 - abs Lat
                     then
                        Map_Pt.X := Map_Pt.X - 4.0 * Sqrt_2 * Cos_Lat;
                        Concorde.Logging.Log
                          (Actor    => "model",
                           Location => "world",
                           Category => "draw",
                           Message  =>
                             "    move x to " & Image (Real (Map_Pt.X)));
                     elsif Centre_Long > 0.0
                       and then Real (Map_Pt.X) < -Pi / 2.0 + abs Lat
                     then
                        Map_Pt.X := Map_Pt.X + 4.0 * Sqrt_2 * Cos_Lat;
                        Concorde.Logging.Log
                          (Actor    => "model",
                           Location => "world",
                           Category => "draw",
                           Message  =>
                             "    move x to " & Image (Real (Map_Pt.X)));
                     end if;

                     if First then
                        Model.Move_To (Map_Pt.X, Map_Pt.Y);
                        First := False;
                     else
                        Model.Line_To (Map_Pt.X, Map_Pt.Y);
                     end if;
                  end;
               end loop;
               Model.Render;
            end if;
         end;
      end loop;
   end Draw_World;

   -------------
   -- Project --
   -------------

   function Project
     (Latitude  : Real;
      Longitude : Real)
      return Map_Point
   is
      use Nazar;
      use Concorde.Elementary_Functions;

      Pi : constant := Ada.Numerics.Pi;
      Theta : Real := Latitude;
      Sin_Lat : constant Real := Sin (Latitude);
      Pi_Sin_Lat : constant Real := Pi * Sin_Lat;
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

               exit when abs (Prev - Theta) < 0.0001;
            end;
         end loop;
      end if;

      return Map_Point'
        (X =>
           Nazar_Float
             (K * Longitude * Cos (Theta)),
         Y =>
           Nazar_Float (Sqrt_2 * Sin (Theta)));
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
            use Concorde.Db;
            Ref : constant Concorde.Db.World_Sector_Reference :=
                    Sector.Get_World_Sector_Reference;
            Terrain : constant Concorde.Db.Terrain.Terrain_Type :=
                        Concorde.Db.Terrain.Get (Sector.Terrain);
         begin
            Result.Sectors.Append
              (Sector_Model'
                 (Sector   => Ref,
                  Owned    => Sector.Faction /= Null_Faction_Reference,
                  Centre   => Concorde.Worlds.Get_Centre (Ref),
                  Boundary =>
                    Vertex_Holders.To_Holder
                      (Concorde.Worlds.Get_Vertices (Ref)),
                  R        => Terrain.Red,
                  G        => Terrain.Green,
                  B        => Terrain.Blue));
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
