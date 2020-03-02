with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Concorde.UI.Entities.Directories;
with Concorde.UI.Entities.Generic_Database_Entity;
with Concorde.Db.Faction;

with Concorde.Db.World;

package body Concorde.UI.Entities.Home is

   function Get_Reference
     (World : Concorde.Db.World.World_Type)
      return Concorde.Db.World_Reference
   is (World.Get_World_Reference);

   function Get_Owned_World
     (Faction : Concorde.Db.Faction_Reference;
      Name    : String)
      return Concorde.Db.World_Reference;

   procedure Iterate_Owned_Worlds
     (Faction : Concorde.Db.Faction_Reference;
      Process : not null access
        procedure (World : Concorde.Db.World.World_Interface'Class));

   function World_Contents
     (World : Concorde.Db.World.World_Type)
      return String;

   package World_Directory is
     new Concorde.UI.Entities.Generic_Database_Entity
       (Container_Handle      => Concorde.Db.Faction_Reference,
        Record_Reference      => Concorde.Db.World_Reference,
        Get_Record            => Concorde.Db.World.Get,
        Get_Reference         => Get_Reference,
        Get_Reference_By_Name => Get_Owned_World,
        Null_Record_Reference => Concorde.Db.Null_World_Reference,
        Record_Interface      => Concorde.Db.World.World_Interface,
        Iterate               => Iterate_Owned_Worlds,
        Contents              => World_Contents,
        "="                   => Concorde.Db."=");

   ---------------------
   -- Get_Owned_World --
   ---------------------

   function Get_Owned_World
     (Faction : Concorde.Db.Faction_Reference;
      Name    : String)
      return Concorde.Db.World_Reference
   is
      use Concorde.Db;
      Owner   : constant Owner_Reference :=
        Concorde.Db.Faction.Get (Faction).Get_Owner_Reference;
      World   : constant Concorde.Db.World.World_Type :=
        Concorde.Db.World.First_By_Name (Name);
   begin
      if World.Owner = Owner then
         return World.Get_World_Reference;
      else
         return Null_World_Reference;
      end if;
   end Get_Owned_World;

   ---------------
   -- Home_Node --
   ---------------

   function Home_Node return Entity_Reference is
   begin
      return Node : constant Entity_Reference :=
        Directories.Create_Directory_Node
      do
         for Faction of Concorde.Db.Faction.Scan_By_Name loop
            declare
               Faction_Home : constant Entity_Reference :=
                 Directories.Create_Directory_Node;
            begin
               Faction_Home.Update.Bind_Child
                 ("worlds",
                  World_Directory.Get_Container_Node
                    (Faction.Get_Faction_Reference));
               Node.Update.Bind_Child
                 (Faction.Name, Faction_Home);
            end;
         end loop;
      end return;
   end Home_Node;

   --------------------------
   -- Iterate_Owned_Worlds --
   --------------------------

   procedure Iterate_Owned_Worlds
     (Faction : Concorde.Db.Faction_Reference;
      Process : not null access
        procedure (World : Concorde.Db.World.World_Interface'Class))
   is
   begin
      for World of Concorde.Db.World.Select_By_Owner
        (Concorde.Db.Faction.Get (Faction).Get_Owner_Reference)
      loop
         Process (World);
      end loop;
   end Iterate_Owned_Worlds;

   --------------------
   -- World_Contents --
   --------------------

   function World_Contents
     (World : Concorde.Db.World.World_Type)
      return String
   is
      package String_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);

      Headings : String_Vectors.Vector;
      Values   : String_Vectors.Vector;

      procedure Add (Field_Name : String);

      function Collate return String;

      ---------
      -- Add --
      ---------

      procedure Add (Field_Name : String) is
      begin
         Headings.Append (Field_Name);
         Values.Append (World.Get (Field_Name));
      end Add;

      -------------
      -- Collate --
      -------------

      function Collate return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for I in 1 .. Headings.Last_Index loop
            Result := Result & Headings.Element (I) & ": "
              & Values.Element (I) & Character'Val (10);
         end loop;
         return To_String (Result);
      end Collate;

   begin
      Add ("name");
      Add ("category");
      Add ("climate");
      Add ("habitability");

      return Collate;
   end World_Contents;

end Concorde.UI.Entities.Home;
