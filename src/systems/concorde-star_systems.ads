with Concorde.Worlds;

with Concorde.Handles.Faction;
with Concorde.Handles.Star;
with Concorde.Handles.Star_System;

package Concorde.Star_Systems is

   type Interstellar_Position is
      record
         X, Y, Z : Real;
      end record;

   subtype Star_System_Class is Concorde.Handles.Star_System.Star_System_Class;

   function Owner
     (Star_System : Star_System_Class)
      return Concorde.Handles.Faction.Faction_Class;

   function Position
     (Star_System : Star_System_Class)
      return Interstellar_Position;

   function Primary
     (Star_System : Star_System_Class)
      return Concorde.Handles.Star.Star_Class;

   function First return Star_System_Class;

   function Find_Exact
     (Name : String)
      return Star_System_Class;

   procedure Claim
     (Star_System : Star_System_Class);

   function Worlds
     (Star_System : Star_System_Class)
      return Concorde.Worlds.World_Selection;

   function Terrestrial_Worlds
     (Star_System : Star_System_Class)
      return Concorde.Worlds.World_Selection;

end Concorde.Star_Systems;
