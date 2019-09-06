with Concorde.Color;

with Concorde.Db;

package Concorde.Factions is

   type Faction_Type is tagged private;

   function Get
     (Reference : Concorde.Db.Faction_Reference)
      return Faction_Type'Class;

   function Get
     (Reference : Concorde.Db.Owner_Reference)
      return Faction_Type'Class;

   function Has_Element
     (Faction : Faction_Type'Class)
     return Boolean;

   function Name
     (Faction : Faction_Type'Class)
      return String;

   function Color
     (Faction : Faction_Type'Class)
      return Concorde.Color.Concorde_Color;

   function Name
     (Faction : Concorde.Db.Faction_Reference)
      return String;

   function Capital_World
     (Faction : Concorde.Db.Faction_Reference)
      return Concorde.Db.World_Reference;

   function Capital_System
     (Faction : Concorde.Db.Faction_Reference)
      return Concorde.Db.Star_System_Reference;

private

   type Faction_Type is tagged
      record
         Reference : Concorde.Db.Faction_Reference;
      end record;

end Concorde.Factions;
