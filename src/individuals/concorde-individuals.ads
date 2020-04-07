with Concorde.Abilities;

with Concorde.Db;

package Concorde.Individuals is

   function Ability_Summary
     (Individual : Concorde.Db.Individual_Reference)
      return String;

   function Ability_Score
     (Individual : Concorde.Db.Individual_Reference;
      Ability    : Concorde.Db.Ability_Reference)
      return Natural;

   function Ability_Modifier
     (Individual : Concorde.Db.Individual_Reference;
      Ability    : Concorde.Db.Ability_Reference)
      return Integer
   is (Concorde.Abilities.Check_Modifier
       (Ability_Score (Individual, Ability)));

   function Check
     (Individual : Concorde.Db.Individual_Reference;
      Ability    : Concorde.Db.Ability_Reference;
      Modifiers  : Integer;
      Difficulty : Natural)
      return Integer
   is (if Difficulty = 0
       then 0
       else Concorde.Abilities.Check
         (Score      => Ability_Score (Individual, Ability),
          Modifiers  => Modifiers,
          Difficulty => Difficulty));

end Concorde.Individuals;
