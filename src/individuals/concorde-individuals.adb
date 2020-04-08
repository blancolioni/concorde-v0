with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Concorde.Logging;

with Concorde.Db.Ability;
with Concorde.Db.Ability_Score;
with Concorde.Db.Individual;
with Concorde.Db.Skill;
with Concorde.Db.Skill_Level;

package body Concorde.Individuals is

   -------------------
   -- Ability_Score --
   -------------------

   function Ability_Score
     (Individual : Concorde.Db.Individual_Reference;
      Ability    : Concorde.Db.Ability_Reference) return Natural
   is
   begin
      return Concorde.Db.Ability_Score.Get_By_Ability_Score
        (Individual, Ability)
        .Score;
   end Ability_Score;

   ---------------------
   -- Ability_Summary --
   ---------------------

   function Ability_Summary
     (Individual : Concorde.Db.Individual_Reference)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Ability of Concorde.Db.Ability.Scan_By_Top_Record loop
         declare
            use Ada.Strings, Ada.Strings.Fixed;
            Score : constant Concorde.Db.Ability_Score.Ability_Score_Type :=
                      Concorde.Db.Ability_Score.Get_By_Ability_Score
                        (Individual, Ability.Get_Ability_Reference);
         begin
            Result := Result
              & (if Result = Null_Unbounded_String then "[" else ",")
              & Ability.Tag & ":" & Trim (Score.Score'Image, Left);
         end;
      end loop;
      return To_String (Result & "]");
   end Ability_Summary;

   -------------------
   -- Advance_Skill --
   -------------------

   procedure Advance_Skill
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
   is
   begin
      if not Has_Skill (Individual, Skill) then
         Log (Concorde.Handles.Individual.Get (Individual),
              "gains "
              & Concorde.Db.Skill.Get (Skill).Tag
              & " at level 0");

         Concorde.Db.Skill_Level.Create
           (Individual => Individual,
            Skill      => Skill,
            Level      => 0);
      else
         declare
            use Concorde.Db.Skill_Level;
            Skill_Level : constant Concorde.Db.Skill_Level_Reference :=
                            Get_Reference_By_Skill_Level
                              (Individual, Skill);
         begin
            Update_Skill_Level (Skill_Level)
              .Set_Level (Current_Level (Individual, Skill) + 1)
              .Done;
         end;

         Log (Concorde.Handles.Individual.Get (Individual),
              "raises "
              & Concorde.Db.Skill.Get (Skill).Tag
              & " to level"
              & Natural'Image (Current_Level (Individual, Skill)));
      end if;
   end Advance_Skill;

   -------------------
   -- Current_Level --
   -------------------

   function Current_Level
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
      return Natural
   is
   begin
      return Concorde.Db.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Level;
   end Current_Level;

   ---------------
   -- Has_Skill --
   ---------------

   function Has_Skill
     (Individual : Concorde.Db.Individual_Reference;
      Skill      : Concorde.Db.Skill_Reference)
      return Boolean
   is
   begin
      return Concorde.Db.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Has_Element;
   end Has_Skill;

   ---------
   -- Log --
   ---------

   procedure Log
     (Individual : Concorde.Handles.Individual.Individual_Handle;
      Message    : String)
   is
      Title : constant String :=
                Concorde.Db.Individual.Get (Individual.Reference_Individual)
                .Title;
   begin
      Concorde.Logging.Log
        (Actor    =>
           (if Title = "" then "" else Title & " ")
             & Individual.First_Name & " " & Individual.Last_Name,
         Location => Individual.World_Sector.World.Name,
         Category => "career",
         Message  => Message);
   end Log;

end Concorde.Individuals;
