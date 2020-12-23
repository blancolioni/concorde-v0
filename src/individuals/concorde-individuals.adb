with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Concorde.Logging;

with Concorde.Handles.Ability_Score;
with Concorde.Handles.Skill_Level;

package body Concorde.Individuals is

   -------------------
   -- Ability_Score --
   -------------------

   function Ability_Score
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Ability    : Concorde.Handles.Ability.Ability_Class)
      return Natural
   is
   begin
      return Concorde.Handles.Ability_Score.Get_By_Ability_Score
        (Individual, Ability)
        .Score;
   end Ability_Score;

   ---------------------
   -- Ability_Summary --
   ---------------------

   function Ability_Summary
     (Individual : Concorde.Handles.Individual.Individual_Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Ability of Concorde.Handles.Ability.Scan_By_Top_Record loop
         declare
            use Ada.Strings, Ada.Strings.Fixed;
            use Concorde.Handles.Ability_Score;
            Score : constant Ability_Score_Handle :=
                      Get_By_Ability_Score
                        (Individual, Ability);
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
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
   is
   begin
      if not Has_Skill (Individual, Skill) then
         Log (Individual,
              "gains "
              & Skill.Tag
              & " at level 0");

         Concorde.Handles.Skill_Level.Create
           (Individual => Individual,
            Skill      => Skill,
            Level      => 0);
      else
         declare
            use Concorde.Handles.Skill_Level;
            Skill_Level : constant Skill_Level_Handle :=
                            Get_By_Skill_Level
                              (Individual, Skill);
         begin
            Skill_Level.Update_Skill_Level
              .Set_Level (Current_Level (Individual, Skill) + 1)
              .Done;
         end;

         Log (Individual,
              "raises "
              & Skill.Tag
              & " to level"
              & Natural'Image (Current_Level (Individual, Skill)));
      end if;
   end Advance_Skill;

   -------------------
   -- Advance_Skill --
   -------------------

   procedure Advance_Skill
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class;
      Level      : Natural)
   is
   begin
      if not Has_Skill (Individual, Skill) then
         Log (Individual,
              "gains "
              & Skill.Tag
              & " at level" & Level'Image);

         Concorde.Handles.Skill_Level.Create
           (Individual => Individual,
            Skill      => Skill,
            Level      => Level);
      elsif Current_Level (Individual, Skill) < Level then
         declare
            use Concorde.Handles.Skill_Level;
            Skill_Level : constant Skill_Level_Handle :=
                            Get_By_Skill_Level
                              (Individual, Skill);
         begin
            Skill_Level.Update_Skill_Level
              .Set_Level (Level)
              .Done;
         end;

         Log (Individual,
              "raises "
              & Skill.Tag
              & " to level"
              & Level'Image);
      end if;
   end Advance_Skill;

   -------------------
   -- Current_Level --
   -------------------

   function Current_Level
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
      return Natural
   is
   begin
      return Concorde.Handles.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Level;
   end Current_Level;

   ---------------
   -- Has_Skill --
   ---------------

   function Has_Skill
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Skill      : Concorde.Handles.Skill.Skill_Class)
      return Boolean
   is
   begin
      return Concorde.Handles.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Has_Element;
   end Has_Skill;

   ---------
   -- Log --
   ---------

   procedure Log
     (Individual : Concorde.Handles.Individual.Individual_Class;
      Message    : String)
   is
      Title : constant String :=
                Individual.Title;
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
