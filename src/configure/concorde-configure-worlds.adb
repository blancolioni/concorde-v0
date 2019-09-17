with Ada.Text_IO;

with Concorde.Random;

with Concorde.Climates;
with Concorde.Solar_System;
with Concorde.Surfaces;

with Concorde.Db.Climate_Terrain;
with Concorde.Db.Deposit;
with Concorde.Db.Sector_Neighbour;
with Concorde.Db.Sector_Vertex;
with Concorde.Db.Terrain_Resource;
with Concorde.Db.World;
with Concorde.Db.World_Sector;

package body Concorde.Configure.Worlds is

   procedure Save_Surface
     (Surface : Concorde.Surfaces.Surface_Type;
      World   : Concorde.Db.World.World_Type);

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (World : Concorde.Db.World_Reference)
   is
      use Concorde.Db;
      Tile_Count : Natural;
      Rec        : constant Concorde.Db.World.World_Type :=
                     Concorde.Db.World.Get (World);
      Radius     : constant Non_Negative_Real :=
                     Rec.Radius
                       / Concorde.Solar_System.Earth_Radius;
   begin

      case Rec.Category is
         when Asteroid | Dwarf | Terrestrial | Super_Terrestrial =>
            Tile_Count := Natural (Radius * 200.0);
         when Sub_Jovian | Jovian | Super_Jovian =>
            Tile_Count := 0;
      end case;

      if Tile_Count = 0 then
         return;
      end if;

      declare
         Surface   : Concorde.Surfaces.Root_Surface_Type;
      begin
         Ada.Text_IO.Put_Line
           (Rec.Name & ": creating surface with "
            & Tile_Count'Image & " tiles");
         Surface.Create_Voronoi_Partition (Tile_Count);
         Save_Surface (Surface, Rec);

         Ada.Text_IO.Put_Line
           (Rec.Name & ": done");

      end;

   end Generate_Surface;

   ------------------
   -- Save_Surface --
   ------------------

   procedure Save_Surface
     (Surface   : Concorde.Surfaces.Surface_Type;
      World     : Concorde.Db.World.World_Type)
   is
      Tile_Refs : array (1 .. Surface.Tile_Count)
        of Concorde.Db.Sector_Reference;
      Terrain_Refs : array (1 .. Surface.Tile_Count)
        of Concorde.Db.Terrain_Reference :=
          (others => Concorde.Db.Null_Terrain_Reference);

      Climate   : constant Concorde.Db.Climate_Reference := World.Climate;
   begin
      for Tile_Index in 1 .. Surface.Tile_Count loop
         declare
            use Concorde.Surfaces;

            type Terrain_Array is
              array (Tile_Neighbour_Count range <>)
              of Concorde.Db.Terrain_Reference;

            function Get_Neighbour_Terrain
              return Terrain_Array;

            ---------------------------
            -- Get_Neighbour_Terrain --
            ---------------------------

            function Get_Neighbour_Terrain
              return Terrain_Array
            is
               use type Concorde.Db.Terrain_Reference;
               Result : Terrain_Array
                 (1 .. Surface.Neighbour_Count (Tile_Index));
               Count : Tile_Neighbour_Count := 0;
            begin
               for I in Result'Range loop
                  declare
                     Neighbour : constant Surface_Tile_Index :=
                                   Surface.Neighbour (Tile_Index, I);
                     Terrain   : constant Concorde.Db.Terrain_Reference :=
                                   Terrain_Refs (Neighbour);
                  begin
                     if Terrain /= Concorde.Db.Null_Terrain_Reference then
                        Count := Count + 1;
                        Result (Count) := Terrain;
                     end if;
                  end;
               end loop;
               return Result (1 .. Count);
            end Get_Neighbour_Terrain;

            Neighbour_Terrain : constant Terrain_Array :=
                                  Get_Neighbour_Terrain;

            Assigned : Boolean := False;
         begin
            for Climate_Terrain of
              Concorde.Db.Climate_Terrain.Select_By_Climate (Climate)
            loop
               declare
                  use type Concorde.Db.Terrain_Reference;
                  Chance : Non_Negative_Real :=
                             Climate_Terrain.Chance;
                  Count  : Natural := 0;
               begin
                  for Terrain of Neighbour_Terrain loop
                     if Terrain = Climate_Terrain.Terrain then
                        Count := Count + 1;
                     end if;
                  end loop;
                  Chance := Chance * (1.0 + Real (Count) / 5.0);

                  if Concorde.Random.Unit_Random < Chance then
                     Terrain_Refs (Tile_Index) := Climate_Terrain.Terrain;
                     Assigned := True;
                     exit;
                  end if;
               end;
            end loop;

            if not Assigned then
               Terrain_Refs (Tile_Index) :=
                 Concorde.Climates.Default_Terrain (Climate);
            end if;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         declare
            use Concorde.Surfaces;
            Sector : constant Concorde.Db.World_Sector.World_Sector_Type :=
                       Concorde.Db.World_Sector.Create;
            Centre : constant Vector_3 := Surface.Tile_Centre (Tile_Index);
         begin
            Sector.Set_World (World);
            Sector.Set_Surface (World);
            Sector.Set_X (Centre (1));
            Sector.Set_Y (Centre (2));
            Sector.Set_Z (Centre (3));
            Sector.Set_Terrain (Terrain_Refs (Tile_Index));

            for Terrain_Resource of
              Concorde.Db.Terrain_Resource.Select_By_Terrain
                (Terrain_Refs (Tile_Index))
            loop
               if Concorde.Random.Unit_Random < Terrain_Resource.Chance then
                  Concorde.Db.Deposit.Create
                    (World_Sector  => Sector.Get_World_Sector_Reference,
                     Resource      => Terrain_Resource.Resource,
                     Accessibility => Concorde.Random.Unit_Random,
                     Abundance     =>
                       (Concorde.Random.Unit_Random + 0.5)
                     * 1.0e6);
               end if;
            end loop;

            for Point of Surface.Tile_Boundary (Tile_Index) loop
               Concorde.Db.Sector_Vertex.Create
                 (Sector        => Sector.Get_Sector_Reference,
                  X             => Point (1),
                  Y             => Point (2),
                  Z             => Point (3));
            end loop;

            Tile_Refs (Tile_Index) := Sector.Get_Sector_Reference;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         for I in 1 .. Surface.Neighbour_Count (Tile_Index) loop
            Concorde.Db.Sector_Neighbour.Create
              (Sector    => Tile_Refs (Tile_Index),
               Neighbour => Tile_Refs (Surface.Neighbour (Tile_Index, I)));
         end loop;
      end loop;

   end Save_Surface;

end Concorde.Configure.Worlds;
