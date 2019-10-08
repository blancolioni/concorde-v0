with Concorde.Db.Faction;
with Concorde.Db.World;

package body Concorde.Factions is

   --------------------
   -- Capital_System --
   --------------------

   function Capital_System
     (Faction : Concorde.Db.Faction_Reference)
      return Concorde.Db.Star_System_Reference
   is
   begin
      return Concorde.Db.World.Get (Capital_World (Faction)).Star_System;
   end Capital_System;

   --------------------
   -- Capital_System --
   --------------------

   function Capital_System
     (Faction : Faction_Type'Class)
      return Concorde.Db.Star_System_Reference
   is
   begin
      return Capital_System (Faction.Reference);
   end Capital_System;

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World
     (Faction : Concorde.Db.Faction_Reference)
      return Concorde.Db.World_Reference
   is
   begin
      return Concorde.Db.Faction.Get (Faction).Capital_World;
   end Capital_World;

   -------------------
   -- Capital_World --
   -------------------

   function Capital_World
     (Faction : Faction_Type'Class)
      return Concorde.Db.World_Reference
   is
   begin
      return Capital_World (Faction.Reference);
   end Capital_World;

   -----------
   -- Color --
   -----------

   function Color
     (Faction : Faction_Type'Class)
      return Concorde.Color.Concorde_Color
   is
      Rec : constant Concorde.Db.Faction.Faction_Type :=
              Concorde.Db.Faction.Get (Faction.Reference);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Concorde.Db.Faction_Reference)
      return Faction_Type'Class
   is
   begin
      return Faction_Type'(Reference => Reference);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Concorde.Db.Owner_Reference)
      return Faction_Type'Class
   is
      Faction : constant Concorde.Db.Faction.Faction_Type :=
                  Concorde.Db.Faction.Get_Faction (Reference);
   begin
      return Get (Faction.Get_Faction_Reference);
   end Get;

   ----------------------
   -- Get_User_Faction --
   ----------------------

   function Get_User_Faction
     (Reference : Concorde.Db.User_Reference)
      return Faction_Type'Class
   is
   begin
      return Get
        (Concorde.Db.Faction.First_Reference_By_User (Reference));
   end Get_User_Faction;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Faction : Faction_Type'Class)
      return Boolean
   is
      use type Concorde.Db.Faction_Reference;
   begin
      return Faction.Reference /= Concorde.Db.Null_Faction_Reference;
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Faction_Type'Class)
      return String
   is
   begin
      return Concorde.Db.Faction.Get (Faction.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Faction : Concorde.Db.Faction_Reference)
      return String
   is
   begin
      return Concorde.Db.Faction.Get (Faction).Name;
   end Name;

end Concorde.Factions;
