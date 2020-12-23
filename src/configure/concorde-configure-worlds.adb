with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with WL.Random.Height_Maps;

with Concorde.Constants;
with Concorde.Elementary_Functions;
with Concorde.Identifiers;
with Concorde.Logging;
with Concorde.Solar_System;
with Concorde.Surfaces;
with Concorde.Terrain;
with Concorde.Trigonometry;

with Concorde.Configure.Resources;

with Concorde.Handles.Climate_Terrain;
with Concorde.Handles.Elevation;
with Concorde.Handles.Faction;
with Concorde.Handles.Feature;
with Concorde.Handles.Sector;
with Concorde.Handles.Sector_Neighbour;
with Concorde.Handles.Sector_Use;
with Concorde.Handles.Sector_Vertex;
with Concorde.Handles.Star_System;
with Concorde.Handles.Terrain;
with Concorde.Handles.World_Sector;

with Concorde.Db;

package body Concorde.Configure.Worlds is

   package Heights renames WL.Random.Height_Maps;

   type Elevation_Terrain is
      record
         Elevation : Concorde.Handles.Elevation.Elevation_Handle;
         Terrain   : Concorde.Handles.Terrain.Terrain_Handle;
      end record;

   package Elevation_Vectors is
     new Ada.Containers.Vectors
       (Positive, Elevation_Terrain);

   type Climate_Terrain_Record is
      record
         Terrain   : Concorde.Handles.Terrain.Terrain_Handle;
         First     : Positive;
         Last      : Natural;
         Frequency : Unit_Real;
         Count     : Natural;
      end record;

   package Climate_Terrain_Vectors is
     new Ada.Containers.Vectors (Positive, Climate_Terrain_Record);

   procedure Save_Surface
     (Surface : Concorde.Surfaces.Surface_Type;
      World   : Concorde.Handles.World.World_Class);

   procedure Get_Climate_Terrain
     (World  : Concorde.Handles.World.World_Class;
      Vector : out Climate_Terrain_Vectors.Vector);

   function Get_Frequencies
     (World : Concorde.Handles.World.World_Class)
      return Heights.Frequency_Map;

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (World : Concorde.Handles.World.World_Class)
   is
      use Concorde.Db;
      Tile_Count : Natural;
      Radius     : constant Non_Negative_Real :=
                     World.Radius
                       / Concorde.Solar_System.Earth_Radius;
   begin

      case World.Composition is
         when Ice | Rock | Rock_Ice | Rock_Iron =>
            Tile_Count := Natural (Radius * 400.0);
         when Hydrogen | Gaseous =>
            Tile_Count := 0;
      end case;

      if Tile_Count > 0 then
         declare
            Surface   : Concorde.Surfaces.Root_Surface_Type;
         begin
            Concorde.Logging.Log
              ("generator", "surfaces", World.Name,
               "Creating surface with "
               & Tile_Count'Image & " tiles");
            Surface.Create_Voronoi_Partition (Tile_Count);
            Save_Surface (Surface, World);

            Concorde.Logging.Log
              ("generator", "surfaces", World.Name,
               "done");

         end;
      end if;

      declare
         System    : constant Concorde.Handles.Star_System.Star_System_Class :=
                       World.Star_System;
      begin
         Concorde.Configure.Resources.Create_Deposits
           (World     => World,
            Generator =>
              Concorde.Configure.Resources.Create_Generator
                (System.X, System.Y, System.Z));
      end;

   end Generate_Surface;

   -------------------------
   -- Get_Climate_Terrain --
   -------------------------

   procedure Get_Climate_Terrain
     (World  : Concorde.Handles.World.World_Class;
      Vector : out Climate_Terrain_Vectors.Vector)
   is
      Water_Last : constant Natural := World.Sea_Level;
      Next       : Positive := Water_Last + 1;
   begin

      if Water_Last > 0 then
         Vector.Append
           (Climate_Terrain_Record'
              (Terrain   => Concorde.Terrain.Ocean,
               First     => 1,
               Last      => Water_Last,
               Frequency => World.Hydrosphere,
               Count     => Water_Last));
      end if;

      for Climate_Terrain of
        Concorde.Handles.Climate_Terrain.Select_By_Climate
          (World.Climate)
      loop

         declare
            Hydrosphere  : constant Unit_Real := World.Hydrosphere;
            Terrain_Freq : constant Unit_Real := Climate_Terrain.Frequency;
            Frequency    : constant Unit_Real :=
                             Terrain_Freq * (1.0 - Hydrosphere);
            Count     : constant Natural :=
              Natural (Frequency * Real (World.Elevation_Range));
         begin
            Vector.Append (Climate_Terrain_Record'
                             (Terrain   =>
                                Climate_Terrain.Terrain.To_Terrain_Handle,
                              First     => Next,
                              Last      => Next + Count - 1,
                              Frequency => Frequency,
                              Count     => Count));
            Next := Next + Count;
         end;
      end loop;

      while Next < World.Elevation_Range loop
         declare
            Total_Move : Natural := 0;
            Remaining  : constant Natural :=
              World.Elevation_Range - Next;
         begin
            for Item of Vector loop
               declare
                  This_Move : constant Positive :=
                    Natural'Min
                      (Natural'Max (Remaining / Vector.Last_Index, 1),
                       Remaining - Total_Move);
               begin
                  Item.First := Item.First + This_Move;
                  Item.Count := Item.Count + This_Move;
                  Item.Last := Item.First + Item.Count - 1;
                  Total_Move := Total_Move + This_Move;
                  exit when Total_Move = Remaining;
               end;
            end loop;
            Next := Next + Total_Move;
         end;
      end loop;

   end Get_Climate_Terrain;

   ---------------------
   -- Get_Frequencies --
   ---------------------

   function Get_Frequencies
     (World : Concorde.Handles.World.World_Class)
      return Heights.Frequency_Map
   is
   begin
      return Freq : constant Heights.Frequency_Map
        (1 .. World.Elevation_Range) := (others => 1);
   end Get_Frequencies;

   ------------------
   -- Save_Surface --
   ------------------

   procedure Save_Surface
     (Surface   : Concorde.Surfaces.Surface_Type;
      World     : Concorde.Handles.World.World_Class)
   is
      Freqs : constant Heights.Frequency_Map :=
                Get_Frequencies (World);
      Hs    : Heights.Height_Array (1 .. Natural (Surface.Tile_Count));

      Tile_Refs : array (1 .. Surface.Tile_Count)
        of Concorde.Handles.Sector.Sector_Handle;

      Axial_Tilt : constant Real :=
                     Concorde.Trigonometry.To_Degrees (World.Tilt);

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real;

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array;

      function Prevailing_Wind
        (Latitude : Real)
         return Real;

      ----------------------
      -- Base_Temperature --
      ----------------------

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real
      is
         Y : constant Real := Surface.Tile_Centre (Tile) (3);
      begin
         return World.Average_Temperature
           + (0.5 - abs Y) * 10.0;
      end Base_Temperature;

      --------------------
      -- Get_Neighbours --
      --------------------

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array
      is
         Tile : constant Surfaces.Surface_Tile_Index :=
                  Surfaces.Surface_Tile_Index (Index);
      begin
         return Ns : Heights.Neighbour_Array
           (1 .. Natural (Surface.Neighbour_Count (Tile)))
         do
            for I in Ns'Range loop
               Ns (I) :=
                 Positive
                   (Surface.Neighbour
                      (Tile,
                       Surfaces.Tile_Neighbour_Index (I)));
            end loop;
         end return;
      end Get_Neighbours;

      ---------------------
      -- Prevailing_Wind --
      ---------------------

      function Prevailing_Wind
        (Latitude : Real)
         return Real
      is
      begin
         if Latitude > 90.0 - Axial_Tilt then
            return -135.0;
         elsif Latitude > Axial_Tilt then
            return 45.0;
         elsif Latitude > 0.0 then
            return -135.0;
         elsif Latitude > -Axial_Tilt then
            return 135.0;
         elsif Latitude > -90.0 + Axial_Tilt then
            return -45.0;
         else
            return 135.0;
         end if;
      end Prevailing_Wind;

      Climate_Vector : Climate_Terrain_Vectors.Vector;
      Elevation      : Elevation_Vectors.Vector;

   begin

      WL.Random.Reset (World.Seed);

      Get_Climate_Terrain (World, Climate_Vector);

      Heights.Generate_Height_Map
        (Heights     => Hs,
         Frequencies => Freqs,
         Smoothing   => 3,
         Neighbours  => Get_Neighbours'Access);

      for E of Concorde.Handles.Elevation.Scan_By_Top_Record loop
         declare
            Height  : constant Integer := E.Height + World.Sea_Level;
            Terrain : Concorde.Handles.Terrain.Terrain_Handle;
         begin
            if Height in 1 .. World.Elevation_Range then
               for Item of Climate_Vector loop
                  if Height in Item.First .. Item.Last then
                     Terrain := Item.Terrain;
                     exit;
                  end if;
               end loop;

               Elevation.Append
                 (Elevation_Terrain'
                    (Elevation => E.To_Elevation_Handle,
                     Terrain   => Terrain));
            end if;
         end;
      end loop;

      for I in Tile_Refs'Range loop
         declare
            Centre : constant Concorde.Surfaces.Vector_3 :=
                       Surface.Tile_Centre (I);
            Latitude : constant Real :=
                         Concorde.Elementary_Functions.Arcsin
                           (Centre (3), 360.0);
            E      : constant Concorde.Handles.Elevation.Elevation_Handle :=
                         Elevation.Element (Hs (Positive (I))).Elevation;
            Terrain : constant Concorde.Handles.Terrain.Terrain_Handle :=
                        Elevation.Element (Hs (Positive (I))).Terrain;
            Ave_Temp : constant Real :=
                         Base_Temperature (I)
                         - (if E.Height <= 0 then 0.0
                            else 6.5 * Real (E.Height) / 100.0);
            Sector               : constant Concorde.Handles.World_Sector
              .World_Sector_Handle :=
                Concorde.Handles.World_Sector.Create
                  (Identifier          => Concorde.Identifiers.Next_Identifier,
                   Surface             => World,
                   X                   => Centre (1),
                   Y                   => Centre (2),
                   Z                   => Centre (3),
                   Faction             =>
                     Concorde.Handles.Faction.Empty_Handle,
                   World               => World,
                   Terrain             => Terrain,
                   Feature             =>
                     Concorde.Handles.Feature.Empty_Handle,
                   Height              => Hs (Positive (I)),
                   Elevation           => E,
                   Sector_Use          =>
                     Concorde.Handles.Sector_Use.Empty_Handle,
                   Average_Temperature => Ave_Temp,
                   Prevailing_Wind     => Prevailing_Wind (Latitude),
                   Moisture            =>
                     (if Terrain.Is_Water
                      then 1.0 else 0.0));
         begin
            Tile_Refs (I) := Sector.To_Sector_Handle;
            for V of Surface.Tile_Boundary (I) loop
               Concorde.Handles.Sector_Vertex.Create
                 (Sector => Tile_Refs (I),
                  X      => V (1),
                  Y      => V (2),
                  Z      => V (3));
            end loop;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         for I in 1 .. Surface.Neighbour_Count (Tile_Index) loop
            Concorde.Handles.Sector_Neighbour.Create
              (Sector    => Tile_Refs (Tile_Index),
               Neighbour => Tile_Refs (Surface.Neighbour (Tile_Index, I)));
         end loop;
      end loop;

      declare
         package Ordered_Sector_Maps is
           new Ada.Containers.Ordered_Maps
             (Real, Concorde.Handles.World_Sector.World_Sector_Handle,
              "<", Concorde.Handles.World_Sector."=");

         Cold_Sectors : Ordered_Sector_Maps.Map;

      begin
         for World_Sector of
           Concorde.Handles.World_Sector
             .Select_World_Temperature_Bounded_By_Average_Temperature
               (World, 0.0, Concorde.Constants.Freezing_Point_Of_Water)
         loop
            if World_Sector.Terrain.Is_Water then
               if World_Sector.Average_Temperature
                 < Concorde.Constants.Freezing_Point_Of_Water - 10.0
               then
                  Cold_Sectors.Insert
                    (Key      => World_Sector.Average_Temperature + 10.0,
                     New_Item => World_Sector.To_World_Sector_Handle);
               end if;
            else
               if World_Sector.Average_Temperature
                 < Concorde.Constants.Freezing_Point_Of_Water
               then
                  Cold_Sectors.Insert
                    (Key      => World_Sector.Average_Temperature,
                     New_Item => World_Sector.To_World_Sector_Handle);
               end if;
            end if;
         end loop;

         declare
            Max_Ice_Sectors  : constant Natural :=
                                 (if World.Average_Temperature < -20.0
                                  then Natural (Surface.Tile_Count)
                                  else Natural (Real (Surface.Tile_Count)
                                    * World.Hydrosphere));
            Ice_Sector_Count : Natural := 0;
            Ice_Feature      : constant Concorde.Handles.Feature.Feature_Handle
              := Concorde.Handles.Feature.Get_By_Tag ("ice");
         begin
            for Ref of Cold_Sectors loop
               exit when Ice_Sector_Count >= Max_Ice_Sectors;

               Concorde.Handles.World_Sector.Update_World_Sector (Ref)
                 .Set_Feature (Ice_Feature)
                 .Done;
               Ice_Sector_Count := Ice_Sector_Count + 1;
            end loop;
         end;
      end;

   end Save_Surface;

end Concorde.Configure.Worlds;
