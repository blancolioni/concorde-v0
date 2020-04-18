with Concorde.Configure.Worlds;

with Concorde.Elementary_Functions;

with Concorde.Db.Deposit;
with Concorde.Db.Generation;
with Concorde.Db.Market;
with Concorde.Db.Sector_Neighbour;
with Concorde.Db.Sector_Vertex;
with Concorde.Db.World;

package body Concorde.Worlds is

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Db.World_Sector_Reference,
        Concorde.Db."=");

   procedure Check_Surface
     (World : Concorde.Db.World_Reference);

   -----------------
   -- Best_Sector --
   -----------------

   function Best_Sector
     (World : Concorde.Db.World_Reference;
      Score : not null access
        function (Sector : Concorde.Db.World_Sector.World_Sector_Type)
      return Real)
      return Concorde.Db.World_Sector_Reference
   is
      Best_Reference : Concorde.Db.World_Sector_Reference :=
                         Concorde.Db.Null_World_Sector_Reference;
      Best_Score     : Real := Real'First;
   begin
      Check_Surface (World);
      for Sector of
        Concorde.Db.World_Sector.Select_By_World
          (World)
      loop
         declare
            This_Score : constant Real :=
                           Score (Sector);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Reference := Sector.Get_World_Sector_Reference;
            end if;
         end;
      end loop;
      return Best_Reference;
   end Best_Sector;

   -------------------
   -- Check_Surface --
   -------------------

   procedure Check_Surface
     (World : Concorde.Db.World_Reference)
   is
      Is_Gen : constant Concorde.Db.Is_Generated_Reference :=
                 Concorde.Db.World.Get (World).Get_Is_Generated_Reference;
      Gen    : constant Concorde.Db.Generation.Generation_Type :=
                 Concorde.Db.Generation.Get_By_Is_Generated
                   (Is_Gen);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Concorde.Configure.Worlds.Generate_Surface (World);
         if Gen.Has_Element then
            Concorde.Db.Generation.Update_Generation
              (Gen.Get_Generation_Reference)
              .Set_Ready (True)
              .Done;
         else
            Concorde.Db.Generation.Create (Is_Gen, True);
         end if;
      end if;
   end Check_Surface;

   -------------------
   -- Circular_Scan --
   -------------------

   procedure Circular_Scan
     (Start : Concorde.Db.World_Sector_Reference;
      Process : not null access
        function (Sector : Concorde.Db.World_Sector_Reference)
      return Boolean)
   is
      package Sector_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Concorde.Db.World_Sector_Reference, Concorde.Db."=");

      Visited : Sector_Lists.List;
      Queued  : Sector_Lists.List;
   begin
      for Neighbour of Get_Neighbours (Start) loop
         Queued.Append (Neighbour);
      end loop;

      while not Queued.Is_Empty loop
         declare
            Current : constant Concorde.Db.World_Sector_Reference :=
                        Queued.First_Element;
         begin
            Queued.Delete_First;
            if not Visited.Contains (Current) then
               Visited.Append (Current);
               if not Process (Current) then
                  exit;
               end if;

               for Neighbour of Get_Neighbours (Current) loop
                  if not Visited.Contains (Neighbour) then
                     Queued.Append (Neighbour);
                  end if;
               end loop;
            end if;
         end;
      end loop;

   end Circular_Scan;

   -----------
   -- Clear --
   -----------

   procedure Clear (Selection : in out World_Selection'Class) is
   begin
      Selection.List.Clear;
   end Clear;

   -------------
   -- Climate --
   -------------

   function Climate
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.World_Climate
   is
   begin
      return Concorde.Db.World.Get (World).Climate;
   end Climate;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Concorde.Db.World_Reference)
      return Boolean)
   is
      New_List : World_Lists.List;
      Changed  : Boolean := False;
   begin
      for World of Selection.List loop
         if Test (World) then
            New_List.Append (World);
         else
            Changed := True;
         end if;
      end loop;
      if Changed then
         Selection.List := New_List;
      end if;
   end Filter;

   -----------------
   -- Find_Sector --
   -----------------

   function Find_Sector
     (World : Concorde.Db.World_Reference;
      Test  : not null access
        function (Sector : Concorde.Db.World_Sector.World_Sector_Type)
      return Boolean)
      return Concorde.Db.World_Sector_Reference
   is
   begin
      for Sector of
        Concorde.Db.World_Sector.Select_By_World
          (World)
      loop
         if Test (Sector) then
            return Sector.Get_World_Sector_Reference;
         end if;
      end loop;
      return Concorde.Db.Null_World_Sector_Reference;
   end Find_Sector;

   -----------------
   -- Get_Bearing --
   -----------------

   function Get_Bearing
     (From, To : Concorde.Db.World_Sector_Reference)
      return Concorde.Trigonometry.Angle
   is
      use Concorde.Spheres;
      B   : constant Surface_Point := Get_Centre (From);
      C   : constant Surface_Point := Get_Centre (To);
   begin
      return Get_Bearing (B, C);
   end Get_Bearing;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Concorde.Db.World_Sector_Reference)
      return Sector_Vertex
   is
      Rec : constant Concorde.Db.World_Sector.World_Sector_Type :=
              Concorde.Db.World_Sector.Get (Sector);
   begin
      return Sector_Vertex'
        (Rec.X, Rec.Y, Rec.Z);
   end Get_Centre;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Concorde.Db.World_Sector_Reference)
      return Sector_Position
   is
      use Concorde.Elementary_Functions;
      Vertex : constant Sector_Vertex := Get_Centre (Sector);
   begin
      return Sector_Position'
        (Latitude  => Arcsin (Vertex.Z),
         Longitude => Arctan (Vertex.Y, Vertex.X));
   end Get_Centre;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance
     (From, To : Concorde.Db.World_Sector_Reference)
      return Non_Negative_Real
   is
      use Concorde.Elementary_Functions;
      P1 : constant Sector_Vertex := Get_Centre (From);
      P2 : constant Sector_Vertex := Get_Centre (To);
      D  : constant Non_Negative_Real :=
             Sqrt
               ((P1.X - P2.X) ** 2
                + (P1.Y - P2.Y) ** 2
                + (P1.Z - P2.Z) ** 2);
      A  : constant Real := 2.0 * Arcsin (D / 2.0);
      R  : constant Non_Negative_Real :=
             Concorde.Db.World.Get (Get_World (From)).Radius;
   begin
      return A * R;
   end Get_Distance;

   --------------------
   -- Get_Neighbours --
   --------------------

   function Get_Neighbours
     (Sector : Concorde.Db.World_Sector_Reference)
      return World_Sector_Array
   is
      Result : World_Sector_Array (1 .. 20);
      Count  : Natural := 0;
   begin
      for Neighbour of
        Concorde.Db.Sector_Neighbour.Select_By_Sector
          (Concorde.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         declare
            Neighbour_Ref : constant Db.Sector_Reference :=
                              Neighbour.Neighbour;
            Neighbour_Sec : constant Db.World_Sector.World_Sector_Type :=
                              Db.World_Sector.Get_World_Sector
                                (Neighbour_Ref);
            World_Sec_Ref : constant Db.World_Sector_Reference :=
                              Neighbour_Sec.Get_World_Sector_Reference;
         begin
            Result (Count) := World_Sec_Ref;
         end;
      end loop;
      return Result (1 .. Count);
   end Get_Neighbours;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.Faction_Reference
   is
   begin
      return Concorde.Db.World_Sector.Get (Sector).Faction;
   end Get_Owner;

   -----------------
   -- Get_Terrain --
   -----------------

   function Get_Terrain
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.Terrain_Reference
   is
   begin
      return Concorde.Db.World_Sector.Get (Sector).Terrain;
   end Get_Terrain;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices
     (Sector : Concorde.Db.World_Sector_Reference)
      return Sector_Vertex_Array
   is
      Count  : Natural := 0;
      Result : Sector_Vertex_Array (1 .. 10);
   begin
      for Vertex of
        Concorde.Db.Sector_Vertex.Select_By_Sector
          (Concorde.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         Result (Count) := (Vertex.X, Vertex.Y, Vertex.Z);
      end loop;
      return Result (1 .. Count);
   end Get_Vertices;

   ---------------
   -- Get_World --
   ---------------

   function Get_World
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.World_Reference
   is
   begin
      return Concorde.Db.World_Sector.Get (Sector).World;
   end Get_World;

   ----------------
   -- Get_Worlds --
   ----------------

   function Get_Worlds
     (Selection : World_Selection'Class)
      return World_Array
   is
      Index : Natural := 0;
   begin
      return Arr : World_Array (1 .. Natural (Selection.List.Length)) do
         for World of Selection.List loop
            Index := Index + 1;
            Arr (Index) := World;
         end loop;
      end return;
   end Get_Worlds;

   ------------------
   -- Habitability --
   ------------------

   function Habitability
     (World : Concorde.Db.World_Reference)
      return Unit_Real
   is
   begin
      return Concorde.Db.World.Get (World).Habitability;
   end Habitability;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Concorde.Db.World_Reference)
   is
   begin
      Selection.List.Append (World);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Selection : World_Selection'Class) return Boolean is
   begin
      return Selection.List.Is_Empty;
   end Is_Empty;

   --------------------
   -- Is_Terrestrial --
   --------------------

   function Is_Terrestrial
     (World : Concorde.Db.World_Reference)
      return Boolean
   is
      use all type Concorde.Db.World_Composition;
   begin
      return Concorde.Db.World.Get (World).Composition in
        Ice .. Rock_Iron;
   end Is_Terrestrial;

   ------------
   -- Market --
   ------------

   function Market
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.Market_Reference
   is
   begin
      return Concorde.Db.Market.Get_By_World (World).Get_Market_Reference;
   end Market;

   ----------
   -- Mass --
   ----------

   function Mass
     (World : Concorde.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.World.Get (World).Mass;
   end Mass;

   ----------
   -- Name --
   ----------

   function Name
     (World : Concorde.Db.World_Reference)
      return String
   is
   begin
      return Concorde.Db.World.Get (World).Name;
   end Name;

   ------------
   -- Radius --
   ------------

   function Radius
     (World : Concorde.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.World.Get (World).Radius;
   end Radius;

   --------------------
   -- Scan_Resources --
   --------------------

   procedure Scan_Resources
     (Sector  : Concorde.Db.World_Sector_Reference;
      Process : not null access
        procedure (Resource : Concorde.Db.Resource_Reference;
                   Concentration : Unit_Real;
                   Difficulty    : Unit_Real;
                   Available     : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Deposit of
        Concorde.Db.Deposit.Select_By_World_Sector
          (Sector)
      loop
         Process (Deposit.Resource, Deposit.Concentration,
                  Deposit.Difficulty, Deposit.Available);
      end loop;
   end Scan_Resources;

   ------------------
   -- Scan_Surface --
   ------------------

   procedure Scan_Surface
     (World   : Concorde.Db.World_Reference;
      Process : not null access
        procedure (Sector : Concorde.Db.World_Sector_Reference))
   is
      List : World_Sector_Lists.List;
   begin
      Check_Surface (World);
      for Sector of Concorde.Db.World_Sector.Select_By_World (World) loop
         List.Append (Sector.Get_World_Sector_Reference);
      end loop;

      for Sector of List loop
         Process (Sector);
      end loop;

   end Scan_Surface;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Sector  : Concorde.Db.World_Sector_Reference;
      Faction : Concorde.Db.Faction_Reference)
   is
   begin
      Concorde.Db.World_Sector.Update_World_Sector (Sector)
        .Set_Faction (Faction)
        .Done;
   end Set_Owner;

   -----------------
   -- Star_System --
   -----------------

   function Star_System
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.Star_System_Reference
   is
   begin
      return Concorde.Db.World.Get (World).Star_System;
   end Star_System;

end Concorde.Worlds;
