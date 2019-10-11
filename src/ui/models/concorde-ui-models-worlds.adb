with Ada.Containers.Vectors;

with Concorde.Color;

with Concorde.Factions;
with Concorde.Terrain;
with Concorde.Worlds;

with Concorde.Db.World;

package body Concorde.UI.Models.Worlds is

   package Vertex_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Worlds.Sector_Vertex, Concorde.Worlds."=");

   type Sector_Record is
      record
         Centre   : Concorde.Worlds.Sector_Vertex;
         Terrain  : Concorde.Db.Terrain_Reference;
         Owner    : Concorde.Db.Faction_Reference;
         Vertices : Vertex_Vectors.Vector;
      end record;

   package Sector_Vectors is
     new Ada.Containers.Vectors (Positive, Sector_Record);

   type World_Model_Type is
     new Concorde.UI.Models.Root_Concorde_Model with
      record
         Sectors : Sector_Vectors.Vector;
      end record;

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class;

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Concorde.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : World_Model_Type)
      return String
   is ("world");

   overriding function Default_View_Name
     (Model : World_Model_Type)
      return String
   is ("World");

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);
      Json_Sectors : Concorde.Json.Json_Array;
   begin
      for Sector of Model.Sectors loop
         declare
            Object   : Concorde.Json.Json_Object;
            Vertices : Concorde.Json.Json_Array;
         begin
            for V of Sector.Vertices loop
               declare
                  Vertex : Concorde.Json.Json_Object;
               begin
                  Vertex.Set_Property ("x", Json.Float_Value (Float (V.X)));
                  Vertex.Set_Property ("y", Json.Float_Value (Float (V.Y)));
                  Vertex.Set_Property ("z", Json.Float_Value (Float (V.Z)));
                  Vertices.Append (Vertex);
               end;
            end loop;

            Object.Set_Property ("vertices", Vertices);

            declare
               Color : constant Concorde.Color.Concorde_Color :=
                 Concorde.Terrain.Color
                   (Sector.Terrain);
            begin
               Object.Set_Property
                 ("r", Json.Float_Value (Float (Color.Red)));
               Object.Set_Property
                 ("g", Json.Float_Value (Float (Color.Green)));
               Object.Set_Property
                 ("b", Json.Float_Value (Float (Color.Blue)));
            end;
            Json_Sectors.Append (Object);
         end;
      end loop;

      return Result : Json.Json_Object do
         Result.Set_Property ("data", Json_Sectors);
      end return;

   end Handle;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Concorde.Db.User_Reference;
      Arguments : String)
   is
      Name : constant String :=
        (if Arguments = ""
         then Concorde.Worlds.Name
           (Concorde.Factions.Get_User_Faction (User).Capital_World)
         else Arguments);
      World : constant Concorde.Db.World_Reference :=
        Concorde.Db.World.First_Reference_By_Name (Name);

      procedure Add_Sector
        (Sector : Concorde.Db.World_Sector_Reference);

      ----------------
      -- Add_Sector --
      ----------------

      procedure Add_Sector
        (Sector : Concorde.Db.World_Sector_Reference)
      is
         C : constant Concorde.Worlds.Sector_Vertex :=
           Concorde.Worlds.Get_Centre (Sector);
         T : constant Concorde.Db.Terrain_Reference :=
           Concorde.Worlds.Get_Terrain (Sector);
         O : constant Concorde.Db.Faction_Reference :=
           Concorde.Worlds.Get_Owner (Sector);
         Vs : constant Concorde.Worlds.Sector_Vertex_Array :=
           Concorde.Worlds.Get_Vertices (Sector);
         V : Vertex_Vectors.Vector;
      begin
         for Vertex of Vs loop
            V.Append (Vertex);
         end loop;

         Model.Sectors.Append
           (Sector_Record'
              (Centre   => C,
               Terrain  => T,
               Owner    => O,
               Vertices => V));

      end Add_Sector;

   begin
      Concorde.Worlds.Scan_Surface
        (World   => World,
         Process => Add_Sector'Access);
   end Start;

   -----------------
   -- World_Model --
   -----------------

   function World_Model return Root_Concorde_Model'Class is
   begin
      return Model : World_Model_Type;
   end World_Model;

end Concorde.UI.Models.Worlds;
