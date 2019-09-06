with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Factions.Lists is
  new Ada.Containers.Doubly_Linked_Lists
    (Concorde.Db.Faction_Reference, Concorde.Db."=");
