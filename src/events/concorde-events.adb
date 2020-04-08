with Ada.Containers.Vectors;

with WL.Random;

with Concorde.Logging;

with Concorde.Individuals;

with Concorde.Handles.Event;

with Concorde.Db.Ability;
with Concorde.Db.Ability_Score;
with Concorde.Db.Change_Ability;
with Concorde.Db.Child_Event;
with Concorde.Db.Event;
with Concorde.Db.Event_Choice;
with Concorde.Db.Event_Effect;
with Concorde.Db.Individual;

package body Concorde.Events is

   package Choice_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Db.Event_Choice_Reference, Concorde.Db."=");

   procedure Log
     (Event   : Concorde.Db.Event_Reference;
      Target  : Concorde.Db.Individual_Reference;
      Message : String);

   procedure Log
     (Effect  : Concorde.Db.Event_Effect_Reference;
      Target  : Concorde.Db.Individual_Reference;
      Message : String);

   procedure Execute_Effect
     (Effect  : Concorde.Db.Event_Effect.Event_Effect_Type;
      Target  : Concorde.Db.Individual_Reference);

   type Effect_Handler is access
     procedure (Effect : Concorde.Db.Event_Effect_Reference;
                Target : Concorde.Db.Individual_Reference);

   procedure Handle_Child_Event
     (Effect : Concorde.Db.Event_Effect_Reference;
      Target : Concorde.Db.Individual_Reference);

   procedure Handle_Change_Ability
     (Effect : Concorde.Db.Event_Effect_Reference;
      Target : Concorde.Db.Individual_Reference);

   Effect_Handlers : constant array (Concorde.Db.Record_Type) of Effect_Handler
     := (Concorde.Db.R_Child_Event    => Handle_Child_Event'Access,
         Concorde.Db.R_Change_Ability => Handle_Change_Ability'Access,
         others                       => null);

   --------------------
   -- Execute_Effect --
   --------------------

   procedure Execute_Effect
     (Effect  : Concorde.Db.Event_Effect.Event_Effect_Type;
      Target  : Concorde.Db.Individual_Reference)
   is
      Handler : constant Effect_Handler := Effect_Handlers (Effect.Top_Record);
   begin
      if Handler = null then
         raise Constraint_Error with
           "no handler for effect " & Effect.Top_Record'Image;
      end if;

      Handler (Effect.Get_Event_Effect_Reference, Target);
   end Execute_Effect;

   -------------------
   -- Execute_Event --
   -------------------

   procedure Execute_Event
     (Event  : Concorde.Db.Event_Reference;
      Target : Concorde.Db.Individual_Reference)
   is
      Choices : Choice_Vectors.Vector;
      Handle  : constant Concorde.Handles.Event.Event_Handle :=
                  Concorde.Handles.Event.Get (Event);
   begin

      Log (Event, Target, "executing");

      for Choice of
        Concorde.Db.Event_Choice.Select_By_Event (Event)
      loop
         Choices.Append (Choice.Get_Event_Choice_Reference);
      end loop;

      if Choices.Is_Empty then
         Log (Event, Target, "done");
      else
         declare
            Chosen : constant Positive :=
                       (if Handle.Random_Choice
                        then WL.Random.Random_Number (1, Choices.Last_Index)
                        else 1);
         begin
            for Effect of
              Concorde.Db.Event_Effect.Select_By_Event_Choice
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
     (Effect : Concorde.Db.Event_Effect_Reference;
      Target : Concorde.Db.Individual_Reference)
   is
      use Concorde.Db;
      Change : constant Concorde.Db.Change_Ability.Change_Ability_Type :=
                 Concorde.Db.Change_Ability.Get_Change_Ability (Effect);
      Ability : Ability_Reference := Change.Ability;
   begin
      if Ability = Null_Ability_Reference then
         declare
            Highest : Natural := 0;
         begin
            for A of Concorde.Db.Ability.Select_By_Category
              (Change.Category)
            loop
               if Concorde.Individuals.Ability_Score
                 (Target, A.Get_Ability_Reference)
                 > Highest
               then
                  Ability := A.Get_Ability_Reference;
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
              Concorde.Db.Ability.Get (Ability).Tag
              & " changed from" & Current'Image & " to" & New_Score'Image);
         Concorde.Db.Ability_Score.Update_Ability_Score
           (Concorde.Db.Ability_Score.Get_Reference_By_Ability_Score
              (Target, Ability))
             .Set_Score (New_Score)
             .Done;
      end;
   end Handle_Change_Ability;

   ------------------------
   -- Handle_Child_Event --
   ------------------------

   procedure Handle_Child_Event
     (Effect : Concorde.Db.Event_Effect_Reference;
      Target : Concorde.Db.Individual_Reference)
   is
      Child : constant Concorde.Db.Child_Event.Child_Event_Type :=
                Concorde.Db.Child_Event.Get_Child_Event (Effect);
      Event : constant Concorde.Db.Event_Reference :=
                Concorde.Db.Event.Get_Reference_By_Tag (Child.Tag);
   begin
      Log (Event, Target, "executing child event");
      Execute_Event (Event, Target);
   end Handle_Child_Event;

   ---------
   -- Log --
   ---------

   procedure Log
     (Event   : Concorde.Db.Event_Reference;
      Target  : Concorde.Db.Individual_Reference;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Concorde.Db.Individual.Get (Target).First_Name,
         "",
         Concorde.Db.Event.Get (Event).Tag,
         Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Effect  : Concorde.Db.Event_Effect_Reference;
      Target  : Concorde.Db.Individual_Reference;
      Message : String)
   is
      pragma Unreferenced (Effect);
   begin
      Concorde.Logging.Log
        (Concorde.Db.Individual.Get (Target).First_Name,
         "",
         "effect",
         Message);
   end Log;

end Concorde.Events;
