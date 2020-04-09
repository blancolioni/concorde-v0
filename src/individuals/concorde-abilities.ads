package Concorde.Abilities is

   type Check_Result is
      record
         Roll       : Positive;
         Modifier   : Integer;
         Difficulty : Positive;
         Success    : Boolean;
      end record;

   function Check_Modifier (Score : Natural) return Integer;

   function Check
     (Score      : Natural;
      Modifiers  : Integer;
      Difficulty : Positive)
      return Check_Result;

end Concorde.Abilities;
