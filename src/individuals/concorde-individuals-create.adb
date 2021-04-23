with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Random.Weighted_Random_Choices;
with WL.Random;
with WL.String_Sets;

with Concorde.Identifiers;
with Concorde.Logging;
with Concorde.Money;
with Concorde.Random;
with Concorde.Random_Names;

with Concorde.Agents;
with Concorde.Events;

with Concorde.Handles.Ability;
with Concorde.Handles.Advancement;
with Concorde.Handles.Advancement_Table;
with Concorde.Handles.Assignment;
with Concorde.Handles.Career;
with Concorde.Handles.Event;
with Concorde.Handles.Individual;
with Concorde.Handles.Ship;

with Concorde.Handles.Ability_Score;
with Concorde.Handles.Account;
with Concorde.Handles.Assignment_Rank;
with Concorde.Handles.Career_Mishap;
with Concorde.Handles.Individual_Career;

with Concorde.Db;

package body Concorde.Individuals.Create is

   function Roll_1D6 return Positive is
     (WL.Random.Random_Number (1, 6));

   function Roll_2D6 return Positive is
     (WL.Random.Random_Number (1, 6)
      + WL.Random.Random_Number (1, 6));

   procedure Run_Career
     (Individual : Concorde.Handles.Individual.Individual_Class);

   --------------------
   -- New_Individual --
   --------------------

   procedure New_Individual
     (Home       : Concorde.Handles.Colony.Colony_Class;
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

      Account      : constant Concorde.Handles.Account.Account_Handle :=
                       Concorde.Agents.New_Account
                         (Starting_Balance => Concorde.Money.Zero,
                          Guarantor        =>
                            Concorde.Handles.Account.Empty_Handle);
      function First_Name return String
      is (if Gender >= 0.0
          then Concorde.Random_Names.Random_Female_Name
          else Concorde.Random_Names.Random_Male_Name);

      function Last_Name return String
      is (Concorde.Random_Names.Random_Last_Name);

      Ref          : constant Concorde.Handles.Individual.Individual_Class :=
                       Concorde.Handles.Individual.Create
                         (Identifier   => Concorde.Identifiers.Next_Identifier,
                          Account      => Account,
                          Last_Earn    => Concorde.Money.Zero,
                          Last_Spend   => Concorde.Money.Zero,
                          Alive        => True,
                          Faction      => Home.Faction,
                          World_Sector => Home.Capital,
                          Ship         => Concorde.Handles.Ship.Empty_Handle,
                          Title        => "",
                          First_Name   => First_Name,
                          Last_Name    => Last_Name,
                          Gender       => Gender,
                          Power        => 0,
                          Birth_Date   => Birth_Date,
                          Death_Date   => Birth_Date);
   begin
      for Ability of
        Concorde.Handles.Ability.Scan_By_Tag
      loop
         declare
            Score : constant Positive := Roll_2D6;
         begin
            Concorde.Handles.Ability_Score.Create
              (Individual => Ref,
               Ability    => Ability,
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
     (Individual : Concorde.Handles.Individual.Individual_Class)
   is
      use Concorde.Calendar;
      Year_18        : constant Year_Number :=
                         Year (Individual.Birth_Date + Years (18));
      Term           : constant Concorde_Duration := Years (4);
      Current_Start  : Time := Time_Of (Year_18, 1, 1);
      Current_Finish : Time := Current_Start + Term;
      Choose_Career  : Boolean := True;
      Current_Assignment : Handles.Assignment.Assignment_Handle;
      Current_Rank       : Natural := 0;
      Tried_Careers      : WL.String_Sets.Set;
      First_Career       : Boolean := True;
      Career_Count       : Natural := 0;

      function Choose_Assignment
        return Concorde.Handles.Assignment.Assignment_Handle;

      procedure Apply_Advancement
        (Advance : Concorde.Handles.Advancement.Advancement_Class);

      procedure Execute_Mishap;

      -----------------------
      -- Apply_Advancement --
      -----------------------

      procedure Apply_Advancement
        (Advance : Concorde.Handles.Advancement.Advancement_Class)
      is
         use type Concorde.Money.Money_Type;
      begin
         if Advance.Cash > Concorde.Money.Zero then
            Log (Individual, "cash: " & Concorde.Money.Show (Advance.Cash));
            Concorde.Agents.Add_Cash
              (Account => Individual.Account,
               Cash    => Advance.Cash,
               Tag     => "career-advance");
         end if;

         if Advance.Ability.Has_Element then
            Log (Individual, Advance.Ability.Tag & " + 1");
            Concorde.Handles.Ability_Score.Update_Ability_Score
              (Concorde.Handles.Ability_Score.Get_By_Ability_Score
                 (Individual, Advance.Ability))
                .Set_Score (Ability_Score (Individual, Advance.Ability) + 1)
                  .Done;
         end if;

         if Advance.Skill.Has_Element then
            if Advance.Skill_Level < 0 then
               Advance_Skill (Individual, Advance.Skill);
            else
               if not Has_Skill (Individual, Advance.Skill) then
                  Advance_Skill (Individual, Advance.Skill);
               end if;

               while Current_Level (Individual, Advance.Skill)
                 < Advance.Skill_Level
               loop
                  Advance_Skill (Individual, Advance.Skill);
               end loop;
            end if;
         end if;
      end Apply_Advancement;

      -----------------------
      -- Choose_Assignment --
      -----------------------

      function Choose_Assignment
        return Concorde.Handles.Assignment.Assignment_Handle
      is
         package Weighted_Assignment_Choice is
           new WL.Random.Weighted_Random_Choices
             (Concorde.Handles.Assignment.Assignment_Handle,
              Concorde.Handles.Assignment."=");

--           package Assignment_Lists is
--             new Ada.Containers.Doubly_Linked_Lists
--               (Concorde.Handles.Career_Reference,
--                Concorde.Handles."=");

         Choices : Weighted_Assignment_Choice.Weighted_Choice_Set;
         Default : Concorde.Handles.Career.Career_Handle :=
                     Concorde.Handles.Career.Empty_Handle;
      begin
         for Assignment of
           Concorde.Handles.Assignment.Scan_By_Tag
         loop
            declare
               Career : constant Handles.Career.Career_Handle :=
                          Assignment.Career.To_Career_Handle;
               Score  : Integer := 0;

               function Score_Check
                 (Ability    : Concorde.Handles.Ability.Ability_Class;
                  Check      : Positive)
                  return Integer
               is (Ability_Modifier (Individual, Ability) - Check);

            begin
               if Career.Check = 0 then
                  Default := Career;
               elsif not Tried_Careers.Contains (Career.Tag) then
                  Score := Score
                    + 2 * Score_Check (Assignment.Survival_Ability,
                                       Assignment.Survival_Check)
                    + Score_Check (Assignment.Advance_Ability,
                                   Assignment.Advance_Check)
                    + 20;

                  if Score > 0 then
                     Choices.Insert
                       (Assignment.To_Assignment_Handle, Score);
                  end if;
               end if;
            end;
         end loop;

         if Choices.Is_Empty then
            for Assignment of
              Concorde.Handles.Assignment.Select_By_Career (Default)
            loop
               declare
                  Score  : Integer := 0;

                  function Score_Check
                    (Ability    : Concorde.Handles.Ability.Ability_Class;
                     Check      : Positive)
                  return Integer
                  is (Ability_Modifier (Individual, Ability) - Check);

               begin
                  Score := Score
                    + 2 * Score_Check (Assignment.Survival_Ability,
                                       Assignment.Survival_Check)
                    + Score_Check (Assignment.Advance_Ability,
                                   Assignment.Advance_Check)
                    + 30;

                  if Score > 0 then
                     Choices.Insert
                       (Assignment.To_Assignment_Handle, Score);
                  end if;
               end;
            end loop;
         end if;

         declare
            Result : constant Concorde.Handles.Assignment.Assignment_Handle :=
                       (if Choices.Is_Empty
                        then Concorde.Handles.Assignment.First_By_Career
                          (Default)
                        else Choices.Choose);
         begin
            return Result;
         end;
      end Choose_Assignment;

      --------------------
      -- Execute_Mishap --
      --------------------

      procedure Execute_Mishap is

         package Event_Vectors is
           new Ada.Containers.Vectors
             (Positive, Concorde.Handles.Event.Event_Handle,
              Concorde.Handles.Event."=");
         Mishaps : Event_Vectors.Vector;
      begin
         for M of Concorde.Handles.Career_Mishap.Select_By_Career
           (Current_Assignment.Career)
         loop
            Mishaps.Append (M.Event.To_Event_Handle);
         end loop;

         declare
            Index : constant Positive :=
                      WL.Random.Random_Number (1, Mishaps.Last_Index);
            Event : constant Concorde.Handles.Event.Event_Handle :=
                      Mishaps (Index);
         begin
            Concorde.Events.Execute_Event (Event, Individual);
         end;

      end Execute_Mishap;

   begin

      Log (Individual, Ability_Summary (Individual));
      Log (Individual, "start career on " & Image (Current_Start));

      while Current_Start <= Clock loop

         if Choose_Career then
            loop
               declare
                  use Concorde.Handles.Assignment;
                  Assignment     : constant Assignment_Handle :=
                                     Choose_Assignment;
                  Auto_Qualified : constant Boolean :=
                                     Assignment.Career.Check = 0;
               begin

                  if Auto_Qualified then
                     Current_Assignment := Assignment;
                     exit;
                  end if;

                  declare
                     Check : constant Abilities.Check_Result :=
                               Concorde.Individuals.Check
                                 (Individual => Individual,
                                  Ability    =>
                                    Assignment.Career.Qualification,
                                  Difficulty =>
                                    Assignment.Career.Check + Career_Count);
                  begin
                     Tried_Careers.Include (Assignment.Career.Tag);

                     Log (Individual,
                          Assignment.Career.Tag
                          & "/"
                          & Assignment.Tag
                          & " "
                          & Assignment.Career.Qualification.Tag
                          & ": " & Abilities.Show (Check));
                     if Check.Success then
                        Current_Assignment := Assignment;
                        exit;
                     end if;
                  end;
               end;
            end loop;

            Current_Rank := 0;
            Career_Count := Career_Count + 1;

            Concorde.Logging.Log
              (Actor    => "",
               Location => Individual.World_Sector.World.Name,
               Category => "career",
               Message  =>
                 Individual.First_Name & " " & Individual.Last_Name
               & " " & Concorde.Calendar.Image (Current_Start)
               & ": starts new career "
               & Current_Assignment.Career.Tag
               & "/"
               & Current_Assignment.Tag);

         else
            Concorde.Logging.Log
              (Actor    => "",
               Location => Individual.World_Sector.World.Name,
               Category => "career",
               Message  =>
                 Individual.First_Name & " " & Individual.Last_Name
               & " " & Concorde.Calendar.Image (Current_Start)
               & ": continues career "
               & Current_Assignment.Tag
               & " at rank" & Current_Rank'Image);
         end if;

         Concorde.Handles.Individual_Career.Create
           (Individual  => Individual,
            Career      => Current_Assignment.Career,
            Assignment  => Current_Assignment,
            Rank        => Current_Rank,
            Term_Start  => Current_Start,
            Term_Finish => Current_Finish);

         if First_Career then
            for Advance of
              Concorde.Handles.Advancement_Table.Select_By_Advancement_Table
                (Current_Assignment.Career,
                 Current_Assignment.Career.Basic_Training,
                 Concorde.Handles.Assignment.Empty_Handle)
            loop
               Advance_Skill (Individual, Advance.Skill);
            end loop;

            First_Career := False;

         elsif Choose_Career then
            declare
               use Concorde.Handles.Advancement_Table;
               Advance : constant Advancement_Table_Handle :=
                           Get_By_Advancement_Entry
                             (Current_Assignment.Career,
                              Current_Assignment.Career.Basic_Training,
                              Concorde.Handles.Assignment.Empty_Handle,
                              Roll_1D6);
            begin
               Advance_Skill (Individual, Advance.Skill);
            end;
         else
            declare
               package Training_Vectors is
                 new Ada.Containers.Vectors
                   (Positive,
                    Concorde.Db.Skill_Training_Type,
                    Concorde.Db."=");
               Vector : Training_Vectors.Vector;
               Career : Concorde.Handles.Career.Career_Class renames
                          Current_Assignment.Career;
            begin
               Vector.Append (Concorde.Db.Personal_Development);
               Vector.Append (Concorde.Db.Service_Skills);
               Vector.Append (Concorde.Db.Assignment_Training);
               if Ability_Score (Individual,
                                 Career.Advanced_Ability)
                 >= Career.Advanced_Check
               then
                  Vector.Append (Concorde.Db.Advanced_Skills);
               end if;

               declare
                  use Concorde.Db, Concorde.Handles.Advancement_Table;
                  Choice : constant Positive :=
                             WL.Random.Random_Number (1, Vector.Last_Index);
                  Training : constant Concorde.Db.Skill_Training_Type :=
                               Vector.Element (Choice);
                  Roll     : constant Positive := Roll_1D6;
                  Advance : constant Advancement_Table_Handle :=
                                  Get_By_Advancement_Entry
                                    (Career     => Current_Assignment.Career,
                                     Table      => Training,
                                     Assignment =>
                                       (if Training = Assignment_Training
                                        then Current_Assignment
                                        else Concorde.Handles.Assignment
                                        .Empty_Handle),
                                     Dr         => Roll);
               begin
                  Apply_Advancement (Advance);
               end;
            end;
         end if;

         Choose_Career := False;

         Current_Start := Current_Finish;
         Current_Finish := Current_Start + Term;

         if Current_Start <= Clock then
            declare
               Survival : constant Concorde.Abilities.Check_Result :=
                            Concorde.Individuals.Check
                              (Individual => Individual,
                               Ability    =>
                                 Current_Assignment.Survival_Ability,
                               Difficulty =>
                                 Current_Assignment.Survival_Check);
            begin
               Log (Individual,
                    "checking survival ("
                    & Current_Assignment.Survival_Ability.Tag
                    & Current_Assignment.Survival_Check'Image
                    & "): "
                    & Concorde.Abilities.Show (Survival));
               if not Survival.Success then
                  Execute_Mishap;
                  Choose_Career := True;
               elsif Current_Rank < 6 then
                  declare
                     Advance : constant Concorde.Abilities.Check_Result :=
                                 Concorde.Individuals.Check
                                   (Individual => Individual,
                                    Ability    =>
                                      Current_Assignment.Advance_Ability,
                                    Difficulty =>
                                      Current_Assignment.Advance_Check);
                  begin
                     Log (Individual,
                          "checking advancement ("
                          & Current_Assignment.Advance_Ability.Tag
                          & Current_Assignment.Advance_Check'Image
                          & "): "
                          & Concorde.Abilities.Show (Advance));
                     if Advance.Success then
                        Log (Individual, "rank goes up");
                        Current_Rank := Current_Rank + 1;

                        declare
                           use Concorde.Handles.Assignment_Rank;
                           Rank : constant Assignment_Rank_Handle :=
                                    Get_By_Assignment_Rank
                                      (Current_Assignment,
                                       Current_Rank);
                        begin
                           if Rank.Tag /= "" then
                              Individual.Update_Individual
                                .Set_Title (Rank.Tag)
                                .Done;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      Log (Individual,
           Ability_Summary (Individual)
           & ": age"
           & Year_Number'Image (Year (Clock) - Year (Individual.Birth_Date)));

   end Run_Career;

end Concorde.Individuals.Create;
