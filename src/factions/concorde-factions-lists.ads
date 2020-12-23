with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Factions.Lists is
  new Ada.Containers.Doubly_Linked_Lists
    (Concorde.Handles.Faction.Faction_Handle, Concorde.Handles."=");
