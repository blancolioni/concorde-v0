with Concorde.Abilities;

with Concorde.Handles.Individual;

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
      Difficulty : Positive)
      return Concorde.Abilities.Check_Result
   is (Concorde.Abilities.Check
         (Score      => Ability_Score (Individual, Ability),
          Modifiers  => Modifiers,
          Difficulty => Difficulty));

   procedure Advance_Skill
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
     with Post => Has_Skill (Individual, Skill);

   function Has_Skill
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
      return Boolean;

   function Current_Level
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
      return Natural
     with Pre => Has_Skill (Individual, Skill);

   procedure Log
     (Individual : Concorde.Handles.Individual.Individual_Handle;
      Message    : String);

end Concorde.Individuals;
