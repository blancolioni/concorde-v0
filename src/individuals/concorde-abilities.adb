with WL.Random;

package body Concorde.Abilities is

   Score_Modifiers : constant array (0 .. 15) of Integer :=
                       (0        => -3,
                        1 .. 2   => -2,
                        3 .. 5   => -1,
                        6 .. 8   => 0,
                        9 .. 11  => 1,
                        12 .. 14 => 2,
                        15       => 3);

   -----------
   -- Check --
   -----------

   function Check
     (Score      : Natural;
      Modifiers  : Integer;
      Difficulty : Positive)
      return Check_Result
   is
      Roll : constant Positive := WL.Random.Random_Number (1, 6)
               + WL.Random.Random_Number (1, 6);
      Total : constant Integer := Roll + Check_Modifier (Score) + Modifiers;
   begin
      return Check_Result'
        (Roll       => Roll,
         Modifier   => Modifiers,
         Difficulty => Difficulty,
         Success    => Roll > 2 and then Total >= Difficulty);
   end Check;

   --------------------
   -- Check_Modifier --
   --------------------

   function Check_Modifier (Score : Natural) return Integer is
   begin
      return Score_Modifiers (Natural'Min (Score, Score_Modifiers'Last));
   end Check_Modifier;

end Concorde.Abilities;
