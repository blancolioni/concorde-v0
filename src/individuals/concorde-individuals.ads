with Concorde.Abilities;

with Concorde.Handles.Ability;
with Concorde.Handles.Individual;
with Concorde.Handles.Skill;

package Concorde.Individuals is

   function Ability_Summary
     (Individual : Concorde.Handles.Individual.Individual_Class)
      return String;

   function Ability_Score
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Ability    : Concorde.Handles.Ability.Ability_Class)
      return Natural;

   function Ability_Modifier
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Ability    : Concorde.Handles.Ability.Ability_Class)
      return Integer
   is (Concorde.Abilities.Check_Modifier
       (Ability_Score (Individual, Ability)));

   function Check
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Ability    : Concorde.Handles.Ability.Ability_Class;
      Difficulty : Positive)
      return Concorde.Abilities.Check_Result
   is (Concorde.Abilities.Check_Ability
         (Score      => Ability_Score (Individual, Ability),
          Difficulty => Difficulty));

   procedure Advance_Skill
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
     with Pre => Individual.Has_Element and then Skill.Has_Element,
     Post => Has_Skill (Individual, Skill);

   procedure Advance_Skill
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class;
      Level      : Natural)
     with Pre => Individual.Has_Element and then Skill.Has_Element,
     Post => Has_Skill (Individual, Skill);

   function Has_Skill
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
      return Boolean;

   function Current_Level
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
      return Natural
     with Pre => Has_Skill (Individual, Skill);

   procedure Log
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Message    : String);

end Concorde.Individuals;
