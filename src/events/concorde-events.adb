with Ada.Containers.Vectors;

with WL.Random;

with Concorde.Logging;

with Concorde.Individuals;

with Concorde.Handles.Ability;
with Concorde.Handles.Ability_Score;
with Concorde.Handles.Change_Ability;
with Concorde.Handles.Child_Event;
with Concorde.Handles.Event_Choice;
with Concorde.Handles.Event_Effect;
with Concorde.Handles.Gain_Skill;
with Concorde.Handles.Skill;

with Concorde.Db;

package body Concorde.Events is

   package Choice_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Handles.Event_Choice.Event_Choice_Handle,
        Concorde.Handles.Event_Choice."=");

   procedure Log
     (Event   : Concorde.Handles.Event.Event_Class;
      Target  : Concorde.Handles.Individual.Individual_Class;
      Message : String);

   procedure Log
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class;
      Message : String);

   procedure Execute_Effect
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class);

   type Effect_Handler is access
     procedure (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
                Target  : Concorde.Handles.Individual.Individual_Class);

   procedure Handle_Child_Event
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class);

   procedure Handle_Change_Ability
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class);

   procedure Handle_Gain_Skill
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class);

   Effect_Handlers : constant array (Concorde.Db.Record_Type) of Effect_Handler
     := (Concorde.Db.R_Child_Event    => Handle_Child_Event'Access,
         Concorde.Db.R_Change_Ability => Handle_Change_Ability'Access,
         Concorde.Db.R_Gain_Skill     => Handle_Gain_Skill'Access,
         others                       => null);

   --------------------
   -- Execute_Effect --
   --------------------

   procedure Execute_Effect
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class)
   is
      Handler : constant Effect_Handler := Effect_Handlers (Effect.Top_Record);
   begin
      if Handler = null then
         raise Constraint_Error with
           "no handler for effect " & Effect.Top_Record'Image;
      end if;

      Handler (Effect, Target);
   end Execute_Effect;

   -------------------
   -- Execute_Event --
   -------------------

   procedure Execute_Event
     (Event  : Concorde.Handles.Event.Event_Class;
      Target : Concorde.Handles.Individual.Individual_Class)
   is
      Choices : Choice_Vectors.Vector;
   begin

      Log (Event, Target, "executing");

      for Choice of
        Concorde.Handles.Event_Choice.Select_By_Event (Event)
      loop
         Choices.Append (Choice.To_Event_Choice_Handle);
      end loop;

      if Choices.Is_Empty then
         Log (Event, Target, "done");
      else
         declare
            Chosen : constant Positive :=
                       (if Event.Random_Choice
                        then WL.Random.Random_Number (1, Choices.Last_Index)
                        else WL.Random.Random_Number (1, Choices.Last_Index));
         begin
            for Effect of
              Concorde.Handles.Event_Effect.Select_By_Event_Choice
                (Choices (Chosen))
            loop
               Execute_Effect (Effect, Target);
            end loop;
         end;
      end if;
   end Execute_Event;

   ---------------------------
   -- Handle_Change_Ability --
   ---------------------------

   procedure Handle_Change_Ability
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class)
   is
      Change : constant Handles.Change_Ability.Change_Ability_Handle :=
                 Handles.Change_Ability.Get_From_Event_Effect (Effect);
      Ability : Concorde.Handles.Ability.Ability_Handle :=
                  Change.Ability.To_Ability_Handle;
   begin
      if not Ability.Has_Element then
         declare
            Highest : Natural := 0;
         begin
            for A of Concorde.Handles.Ability.Select_By_Category
              (Change.Category)
            loop
               if Concorde.Individuals.Ability_Score (Target, A)
                 > Highest
               then
                  Ability := A.To_Ability_Handle;
                  Highest := Concorde.Individuals.Ability_Score
                    (Target, Ability);
               end if;
            end loop;
         end;
      end if;

      declare
         Score_Change : constant Integer :=
                          (if Change.Low = Change.High then Change.Low
                           else WL.Random.Random_Number
                             (Change.Low, Change.High));
         Current : constant Natural :=
                     Concorde.Individuals.Ability_Score
                       (Target, Ability);
         New_Score : constant Natural :=
                       Integer'Max (0, Current + Score_Change);
      begin
         Log (Effect, Target,
              Ability.Tag
              & " changed from" & Current'Image & " to" & New_Score'Image);
         Concorde.Handles.Ability_Score.Update_Ability_Score
           (Concorde.Handles.Ability_Score.Get_By_Ability_Score
              (Target, Ability))
             .Set_Score (New_Score)
             .Done;
      end;
   end Handle_Change_Ability;

   ------------------------
   -- Handle_Child_Event --
   ------------------------

   procedure Handle_Child_Event
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class)
   is
      Child : constant Concorde.Handles.Child_Event.Child_Event_Handle :=
                Concorde.Handles.Child_Event.Get_From_Event_Effect (Effect);
      Event : constant Concorde.Handles.Event.Event_Handle :=
                Concorde.Handles.Event.Get_By_Tag (Child.Tag);
   begin
      Log (Event, Target, "executing child event");
      Execute_Event (Event, Target);
   end Handle_Child_Event;

   -----------------------
   -- Handle_Gain_Skill --
   -----------------------

   procedure Handle_Gain_Skill
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class)
   is
      Change : constant Concorde.Handles.Gain_Skill.Gain_Skill_Handle :=
                 Concorde.Handles.Gain_Skill.Get_From_Event_Effect (Effect);
      Skill  : constant Concorde.Handles.Skill.Skill_Class := Change.Skill;
   begin
      Log (Effect, Target,
           Skill.Tag & Change.Level'Image);
      Concorde.Individuals.Advance_Skill
        (Individual => Target,
         Skill      => Skill,
         Level      => Change.Level);
   end Handle_Gain_Skill;

   ---------
   -- Log --
   ---------

   procedure Log
     (Event   : Concorde.Handles.Event.Event_Class;
      Target  : Concorde.Handles.Individual.Individual_Class;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Target.First_Name,
         "",
         Event.Tag,
         Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Effect  : Concorde.Handles.Event_Effect.Event_Effect_Class;
      Target  : Concorde.Handles.Individual.Individual_Class;
      Message : String)
   is
      pragma Unreferenced (Effect);
   begin
      Concorde.Logging.Log
        (Target.First_Name,
         "",
         "effect",
         Message);
   end Log;

end Concorde.Events;
