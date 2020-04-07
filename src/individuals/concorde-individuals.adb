with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Concorde.Db.Ability;
with Concorde.Db.Ability_Score;

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

end Concorde.Individuals;
