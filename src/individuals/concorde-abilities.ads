package Concorde.Abilities is

   function Check_Modifier (Score : Natural) return Integer;

   function Check
     (Score      : Natural;
      Modifiers  : Integer;
      Difficulty : Positive)
      return Integer;

end Concorde.Abilities;
