--  with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Random.Weighted_Random_Choices;
with WL.Random;
with WL.String_Sets;

with Concorde.Logging;
with Concorde.Money;
with Concorde.Random;
with Concorde.Random_Names;

with Concorde.Handles.Ability;
with Concorde.Handles.Assignment;
with Concorde.Handles.Career;
with Concorde.Handles.Individual;

with Concorde.Db.Ability;
with Concorde.Db.Ability_Score;
with Concorde.Db.Account;
with Concorde.Db.Assignment;
with Concorde.Db.Assignment_Rank;
with Concorde.Db.Colony;
with Concorde.Db.Individual;
with Concorde.Db.Individual_Career;

package body Concorde.Individuals.Create is

   function Roll_2D6 return Positive is
     (WL.Random.Random_Number (1, 6)
      + WL.Random.Random_Number (1, 6));

--     function Roll_1D6 return Positive is
--       (WL.Random.Random_Number (1, 6));

   procedure Run_Career
     (Individual : Concorde.Db.Individual_Reference);

   procedure Log
     (Individual : Concorde.Handles.Individual.Individual_Handle;
      Message    : String);

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

   --------------------
   -- New_Individual --
   --------------------

   procedure New_Individual
     (Home       : Concorde.Db.Colony_Reference;
      Birth_Date : Concorde.Calendar.Time)
   is

      Base_Gender  : constant Signed_Unit_Real :=
                       Signed_Unit_Clamp
                         (Concorde.Random.Normal_Random (0.1));
      Gender       : constant Signed_Unit_Real :=
                       (if Base_Gender < 0.0
                        then -1.0 - Base_Gender
                        else 1.0 - Base_Gender);
      Power        : Natural := 0;

      Colony : constant Concorde.Db.Colony.Colony_Type :=
                 Concorde.Db.Colony.Get (Home);
      Account      : constant Concorde.Db.Account_Reference :=
                       Concorde.Db.Account.Create
                         (Concorde.Db.Null_Account_Reference,
                          Start_Cash => Concorde.Money.Zero,
                          Cash       => Concorde.Money.Zero,
                          Earn       => Concorde.Money.Zero,
                          Spend      => Concorde.Money.Zero);

      function First_Name return String
      is (if Gender >= 0.0
          then Concorde.Random_Names.Random_Female_Name
          else Concorde.Random_Names.Random_Male_Name);

      function Last_Name return String
      is (Concorde.Random_Names.Random_Last_Name);

      Ref          : constant Concorde.Db.Individual_Reference :=
                       Concorde.Db.Individual.Create
                         (Account      => Account,
                          Last_Earn    => Concorde.Money.Zero,
                          Last_Spend   => Concorde.Money.Zero,
                          Alive        => True,
                          Faction      => Colony.Faction,
                          World_Sector => Colony.Capital,
                          Ship         => Concorde.Db.Null_Ship_Reference,
                          Title        => "",
                          First_Name   => First_Name,
                          Last_Name    => Last_Name,
                          Gender       => Gender,
                          Power        => 0,
                          Birth_Date   => Birth_Date,
                          Death_Date   => Birth_Date);
   begin
      for Ability of
        Concorde.Db.Ability.Scan_By_Tag
      loop
         declare
            Score : constant Positive := Roll_2D6;
         begin
            Concorde.Db.Ability_Score.Create
              (Individual => Ref,
               Ability    => Ability.Get_Ability_Reference,
               Score      => Score);
            Power := Power + Score;
         end;
      end loop;

      Run_Career (Ref);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "error while creating individual: "
            & Ada.Exceptions.Exception_Message (E));
   end New_Individual;

   ----------------
   -- Run_Career --
   ----------------

   procedure Run_Career
     (Individual : Concorde.Db.Individual_Reference)
   is
      use Concorde.Calendar;
      Handle         : constant Handles.Individual.Individual_Handle :=
                         Concorde.Handles.Individual.Get (Individual);
      Year_18        : constant Year_Number :=
                         Year (Handle.Birth_Date + Years (18));
      Term           : constant Duration := Years (4);
      Current_Start  : Time := Time_Of (Year_18, 1, 1);
      Current_Finish : Time := Current_Start + Term;
      Choose_Career  : Boolean := True;
      Current_Assignment : Handles.Assignment.Assignment_Handle;
      Current_Rank       : Natural := 0;
      Tried_Careers      : WL.String_Sets.Set;

      function Choose_Assignment
        return Concorde.Handles.Assignment.Assignment_Handle;

      -----------------------
      -- Choose_Assignment --
      -----------------------

      function Choose_Assignment
        return Concorde.Handles.Assignment.Assignment_Handle
      is
         package Weighted_Assignment_Choice is
           new WL.Random.Weighted_Random_Choices
             (Concorde.Db.Assignment_Reference);

--           package Assignment_Lists is
--             new Ada.Containers.Doubly_Linked_Lists
--               (Concorde.Db.Career_Reference,
--                Concorde.Db."=");

         Choices : Weighted_Assignment_Choice.Weighted_Choice_Set;
         Default : Concorde.Db.Career_Reference :=
                     Concorde.Db.Null_Career_Reference;
      begin
         for Assignment of
           Concorde.Db.Assignment.Scan_By_Tag
         loop
            declare
               Career : constant Handles.Career.Career_Handle :=
                          Handles.Career.Get (Assignment.Career);
               Score  : Integer := 0;

               function Score_Check
                 (Ability    : Concorde.Db.Ability_Reference;
                  Check      : Positive)
                  return Integer
               is (Ability_Modifier (Individual, Ability) - Check);

            begin
               if Career.Check = 0 then
                  Default := Career.Reference_Career;
               elsif not Tried_Careers.Contains (Career.Tag) then
                  Score := Score
                    + Score_Check (Career.Qualification.Reference_Ability,
                                   Career.Check)
                    + Score_Check (Assignment.Survival_Ability,
                                   Assignment.Survival_Check)
                    + Score_Check (Assignment.Advance_Ability,
                                   Assignment.Advance_Check)
                    + 20;

                  if Score > 0 then
                     Log (Handle,
                          Career.Tag & "/" & Assignment.Tag
                          & ": score ="
                          & Score'Image);
                     Choices.Insert
                       (Assignment.Get_Assignment_Reference, Score);
                  else
                     Log (Handle,
                          "skipping "
                          & Career.Tag & "/" & Assignment.Tag);
                  end if;
               end if;
            end;
         end loop;

         if Choices.Is_Empty then
            for Assignment of
              Concorde.Db.Assignment.Select_By_Career (Default)
            loop
               declare
                  Career : constant Handles.Career.Career_Handle :=
                             Handles.Career.Get (Assignment.Career);
                  Score  : Integer := 0;

                  function Score_Check
                    (Ability    : Concorde.Db.Ability_Reference;
                     Check      : Positive)
                  return Integer
                  is (Ability_Modifier (Individual, Ability) - Check);

               begin
                  Score := Score
                    + Score_Check (Assignment.Survival_Ability,
                                   Assignment.Survival_Check)
                    + Score_Check (Assignment.Advance_Ability,
                                   Assignment.Advance_Check)
                    + 20;

                  if Score > 0 then
                     Log (Handle,
                          Career.Tag & "/" & Assignment.Tag
                          & ": score ="
                          & Score'Image);
                     Choices.Insert
                       (Assignment.Get_Assignment_Reference, Score);
                  else
                     Log (Handle,
                          "skipping "
                          & Career.Tag & "/" & Assignment.Tag);
                  end if;
               end;
            end loop;
         end if;

         declare
            Result : constant Concorde.Db.Assignment_Reference :=
                       (if Choices.Is_Empty
                        then Concorde.Db.Assignment.First_Reference_By_Career
                          (Default)
                        else Choices.Choose);
         begin
            return Handles.Assignment.Get (Result);
         end;
      end Choose_Assignment;

   begin

      Log (Handle, Ability_Summary (Individual));
      Log (Handle, "start career on " & Image (Current_Start));

      while Current_Start <= Clock loop

         if Choose_Career then
            loop
               declare
                  use Concorde.Handles.Assignment;
                  Assignment    : constant Assignment_Handle :=
                                    Choose_Assignment;
                  Qualification : constant Integer :=
                                    Concorde.Individuals.Check
                                      (Individual => Individual,
                                       Ability    =>
                                         Assignment.Career.Qualification
                                       .Reference_Ability,
                                       Modifiers  => 0,
                                       Difficulty =>
                                         Assignment.Career.Check);
               begin
                  Tried_Careers.Include (Assignment.Career.Tag);

                  if Qualification < 0 then
                     Log (Handle,
                          Assignment.Career.Tag
                          & "/"
                          & Assignment.Tag
                          & ": failed to qualify");
                  else
                     Current_Assignment := Assignment;
                     Current_Rank := 0;

                     Concorde.Logging.Log
                       (Actor    => "",
                        Location => Handle.World_Sector.World.Name,
                        Category => "career",
                        Message  =>
                          Handle.First_Name & " " & Handle.Last_Name
                        & " " & Concorde.Calendar.Image (Current_Start)
                        & ": starts new career "
                        & Assignment.Career.Tag
                        & "/"
                        & Assignment.Tag);
                     exit;
                  end if;
               end;
            end loop;
         else
            Concorde.Logging.Log
              (Actor    => "",
               Location => Handle.World_Sector.World.Name,
               Category => "career",
               Message  =>
                 Handle.First_Name & " " & Handle.Last_Name
               & " " & Concorde.Calendar.Image (Current_Start)
               & ": continues career "
               & Current_Assignment.Tag
               & " at rank" & Current_Rank'Image);
         end if;

         Choose_Career := False;

         Concorde.Db.Individual_Career.Create
           (Individual  => Individual,
            Career      => Current_Assignment.Career.Reference_Career,
            Assignment  => Current_Assignment.Reference,
            Rank        => Current_Rank,
            Term_Start  => Current_Start,
            Term_Finish => Current_Finish);

         Current_Start := Current_Finish;
         Current_Finish := Current_Start + Term;

         declare
            Survival : constant Integer :=
                         Concorde.Individuals.Check
                           (Individual => Individual,
                            Ability    =>
                              Current_Assignment.Survival_Ability
                            .Reference_Ability,
                            Modifiers  => 0,
                            Difficulty => Current_Assignment.Survival_Check);
         begin
            Log (Handle,
                 "checking survival ("
                 & Current_Assignment.Survival_Ability.Tag
                 & Current_Assignment.Survival_Check'Image
                 & "): "
                 & (if Survival < 0
                   then "FAIL"
                   else "Pass"));
            if Survival < 0 then
               Log (Handle, "leaving career");
               Choose_Career := True;
            elsif Current_Rank < 6 then
               declare
                  Advance : constant Integer :=
                               Concorde.Individuals.Check
                                 (Individual => Individual,
                                  Ability    =>
                                    Current_Assignment.Advance_Ability
                                  .Reference_Ability,
                                  Modifiers  => 0,
                                  Difficulty =>
                                    Current_Assignment.Advance_Check);
               begin
                  Log (Handle,
                       "checking advancement ("
                       & Current_Assignment.Advance_Ability.Tag
                       & Current_Assignment.Advance_Check'Image
                       & "): "
                       & (if Advance < 0
                         then "FAIL"
                         else "Pass"));
                  if Advance >= 0 then
                     Log (Handle, "rank goes up");
                     Current_Rank := Current_Rank + 1;

                     declare
                        use Concorde.Db.Assignment_Rank;
                        Rank : constant Assignment_Rank_Type :=
                                 Get_By_Assignment_Rank
                                   (Current_Assignment.Reference_Assignment,
                                    Current_Rank);
                     begin
                        if Rank.Tag /= "" then
                           Concorde.Db.Individual.Update_Individual
                             (Individual)
                             .Set_Title (Rank.Tag)
                             .Done;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      Log (Handle,
           Ability_Summary (Individual)
           & ": age"
           & Year_Number'Image (Year (Clock) - Year (Handle.Birth_Date)));

   end Run_Career;

end Concorde.Individuals.Create;
