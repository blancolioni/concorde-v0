with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Numerics;

with WL.String_Maps;

with Nazar.Colors;

with Concorde.Elementary_Functions;
--  with Concorde.Logging;
with Concorde.Real_Images;
with Concorde.Trigonometry;

with Concorde.Worlds;

with Concorde.Handles.Feature;
with Concorde.Handles.Has_Color;
with Concorde.Handles.Terrain;

with Concorde.Handles.Colony;
with Concorde.Handles.World_Sector;

package body Concorde.UI.Models.World is

   Sqrt_2 : constant := 1.4142135623730950488016887242;

   package Vertex_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Worlds.Sector_Vertex_Array, Concorde.Worlds."=");

   type Map_Point is
      record
         X, Y : Nazar.Nazar_Float;
      end record;

   type Sector_Model is
      record
         Owner      : Concorde.Handles.Faction.Faction_Handle;
         Sector     : Concorde.Handles.World_Sector.World_Sector_Handle;
         Centre     : Concorde.Worlds.Sector_Vertex;
         Map_Centre : Map_Point;
         Boundary   : Vertex_Holders.Holder;
         Color      : Nazar.Colors.Nazar_Color;
         Wind       : Real;
         Wind_To    : Natural;
      end record;

   package Sector_Model_Vectors is
      new Ada.Containers.Vectors (Positive, Sector_Model);

   package Sector_Reference_Maps is
     new WL.String_Maps (Positive);

   type World_Model_Type is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Sectors : Sector_Model_Vectors.Vector;
         Ref_Map : Sector_Reference_Maps.Map;
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
                   renames Concorde.Real_Images.Approximate_Image
     with Unreferenced;

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

      --  Concorde.Logging.Log
      --    ("model", "world", "draw",
      --     "long-0 = "
      --     & Image (180.0 * Long_0 / Pi));

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
               Centre_Lat  : constant Real := Arcsin (Sector.Centre.Z);
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

               if Fill then
                  Model.Set_Color (Color);
                  Sector.Map_Centre :=
                    Project (Centre_Lat, Centre_Long, Centre_Long > 0.0);
               end if;

               for Pt of Boundary loop
                  declare
                     Long    : constant Real :=
                                 Arctan (Pt.Y, Pt.X) - Long_0;
                     Lat     : constant Real := Arcsin (Pt.Z);
                     Map_Pt  : constant Map_Point :=
                                 Project (Lat, Long, Centre_Long > 0.0);
                  begin
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

      Model.Set_Color (0.0, 0.0, 0.0, 1.0);
      for Sector of Model.Sectors loop
         declare
            use Nazar;
            From : constant Map_Point :=
                     (Sector.Map_Centre.X, Sector.Map_Centre.Y);
            To_S : constant Sector_Model :=
                     Model.Sectors.Element (Sector.Wind_To);
            To   : constant Map_Point :=
                     (From.X + (To_S.Map_Centre.X - From.X) / 2.0,
                      From.Y + (To_S.Map_Centre.Y - From.Y) / 2.0);
            DX   : constant Nazar_Float := To.X - From.X;
            DY   : constant Nazar_Float := To.Y - From.Y;
            D    : constant Non_Negative_Real :=
                     Sqrt (Real (DX) ** 2 + Real (DY) ** 2);
            N    : constant Map_Point :=
                     (DX / Nazar_Float (D), DY / Nazar_Float (D));
            AX   : constant Nazar_Float := 0.025 * (-N.Y - N.X);
            AY   : constant Nazar_Float := 0.025 * (N.X - N.Y);
         begin
            Model.Move_To (From.X, From.Y);
            Model.Line_To (To.X, To.Y);
            Model.Line_To (To.X + AX, To.Y + AY);
            Model.Move_To (To.X, To.Y);
            Model.Line_To (To.X - AY, To.Y + AX);

            Model.Render;
            --  Concorde.Logging.Log
            --    ("model", "world", "draw",
            --     "wind = "
            --     & Image (180.0 * Sector.Wind / Pi)
            --     & "; to ="
            --     & Sector.Wind_To'Image
            --     & "; line ("
            --     & Image (Real (From.X))
            --     & ","
            --     & Image (Real (From.Y))
            --     & ") -> ("
            --     & Image (Real (To.X))
            --     & ","
            --     & Image (Real (To.Y))
            --     & ")");
         end;
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
        Concorde.Handles.World_Sector.Select_By_World (World)
      loop
         declare
            Terrain : constant Concorde.Handles.Terrain.Terrain_Class :=
                        Sector.Terrain;
            Faction : constant Concorde.Handles.Faction.Faction_Class :=
                        Sector.Faction;
            Feature : constant Concorde.Handles.Feature.Feature_Class :=
                        Sector.Feature;
            Color   : constant Nazar.Colors.Nazar_Color :=
                        (if Faction.Has_Element
                         then To_Nazar_Color (Faction)
                         elsif Feature.Has_Element
                         then To_Nazar_Color (Feature)
                         else To_Nazar_Color (Terrain));
         begin
            Result.Sectors.Append
              (Sector_Model'
                 (Sector     => Sector.To_World_Sector_Handle,
                  Owner      => Sector.Faction.To_Faction_Handle,
                  Centre     => Concorde.Worlds.Get_Centre (Sector),
                  Map_Centre => (0.0, 0.0),
                  Boundary   =>
                    Vertex_Holders.To_Holder
                      (Concorde.Worlds.Get_Vertices (Sector)),
                  Color      => Color,
                  Wind       => Sector.Prevailing_Wind,
                  Wind_To    => 0));
            Result.Ref_Map.Insert
              (Sector.Identifier, Result.Sectors.Last_Index);
         end;
      end loop;

      for Sector of Result.Sectors loop
         declare
            use Concorde.Trigonometry;
            Wind    : constant Angle := From_Degrees (Sector.Wind);
            To      : Concorde.Handles.World_Sector.World_Sector_Handle :=
                        Concorde.Handles.World_Sector.Empty_Handle;
            Closest : Angle := From_Radians (0.0);
            First   : Boolean := True;
         begin

            for Neighbour of
              Concorde.Worlds.Get_Neighbours (Sector.Sector)
            loop
               declare
                  Bearing : constant Angle :=
                              Concorde.Worlds.Get_Bearing
                                (Sector.Sector, Neighbour);
               begin
                  if First or else Wind - Bearing < Closest then
                     Closest := Bearing;
                     To := Neighbour;
                     First := False;
                  end if;
               end;
            end loop;
            Sector.Wind_To :=
              Result.Ref_Map.Element (To.Identifier);
         end;
      end loop;

      declare
         Colony : constant Concorde.Handles.Colony.Colony_Class :=
                    Concorde.Handles.Colony.First_By_World (World);
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
