with WL.String_Maps;

with Tropos.Reader;

with Concorde.Handles.Ability;
with Concorde.Handles.Change_Ability;
with Concorde.Handles.Gain_Skill;
with Concorde.Handles.Child_Event;
with Concorde.Handles.Event;
with Concorde.Handles.Event_Choice;
with Concorde.Handles.Skill;

with Concorde.Db;

package body Concorde.Configure.Events is

   type Configure_Procedure is access
     procedure (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
                Config : Tropos.Configuration);

   package Configure_Maps is
     new WL.String_Maps (Configure_Procedure);

   Configure_Table : Configure_Maps.Map;

   procedure Load_Configurers;

   procedure Configure_Effect
     (Choice        : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Effect_Config : Tropos.Configuration);

   procedure Configure_Event
     (Event_Config : Tropos.Configuration);

   procedure Configure_Ability_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Ability_Category_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Skill_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Child_Event
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Get_Change_Range
     (Change : String;
      Low    : out Integer;
      High   : out Integer);

   ---------------------------------------
   -- Configure_Ability_Category_Change --
   ---------------------------------------

   procedure Configure_Ability_Category_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
      Tag : constant String := Config.Config_Name;
      Category : constant Concorde.Db.Ability_Category :=
                   (if Tag = "physical-ability"
                    then Concorde.Db.Physical
                    elsif Tag = "mental-ability"
                    then Concorde.Db.Mental
                    elsif Tag = "social-ability"
                    then Concorde.Db.Social
                    else raise Constraint_Error with
                    "unknown category: " & Tag);

      Value : constant String := Config.Value;
      Low   : Integer;
      High  : Integer;
   begin
      Get_Change_Range (Value, Low, High);
      Concorde.Handles.Change_Ability.Create
        (Event_Choice => Choice,
         Category     => Category,
         Ability      => Concorde.Handles.Ability.Empty_Handle,
         Low          => Low,
         High         => High);
   end Configure_Ability_Category_Change;

   ------------------------------
   -- Configure_Ability_Change --
   ------------------------------

   procedure Configure_Ability_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
      Value : constant String := Config.Value;
      Low   : Integer;
      High  : Integer;
   begin
      Get_Change_Range (Value, Low, High);
      Concorde.Handles.Change_Ability.Create
        (Event_Choice => Choice,
         Category     => Concorde.Db.Physical,
         Ability      =>
           Concorde.Handles.Ability.Get_By_Tag
             (Config.Config_Name),
         Low          => Low,
         High         => High);
   end Configure_Ability_Change;

   ---------------------------
   -- Configure_Child_Event --
   ---------------------------

   procedure Configure_Child_Event
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
   begin
      Concorde.Handles.Child_Event.Create
        (Event_Choice => Choice,
         Tag          => Config.Value,
         Redirect     => Config.Config_Name = "redirect");
   end Configure_Child_Event;

   ----------------------
   -- Configure_Effect --
   ----------------------

   procedure Configure_Effect
     (Choice        : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Effect_Config : Tropos.Configuration)
   is
      Tag : constant String := Effect_Config.Config_Name;
   begin
      if Configure_Table.Contains (Tag) then
         Configure_Table.Element (Tag) (Choice, Effect_Config);
      else
         raise Constraint_Error with
           "no such effect: " & Tag;
      end if;
   end Configure_Effect;

   ---------------------
   -- Configure_Event --
   ---------------------

   procedure Configure_Event
     (Event_Config : Tropos.Configuration)
   is
      Event : constant Concorde.Handles.Event.Event_Handle :=
                Concorde.Handles.Event.Create
                  (Tag           => "evt-" & Event_Config.Config_Name,
                   Heading       =>
                     Event_Config.Get ("heading", Event_Config.Config_Name),
                   Text          =>
                     Event_Config.Get ("text", "default-event-text"),
                   Random_Choice => Event_Config.Contains ("random"),
                   Auto_Execute  => Event_Config.Contains ("auto"));
   begin
      for Choice_Config of Event_Config.Child ("choices") loop
         declare
            Choice               : constant Concorde.Handles.Event_Choice
              .Event_Choice_Handle :=
                       Concorde.Handles.Event_Choice.Create
                         (Event => Event,
                          Tag   => Choice_Config.Config_Name);
         begin
            for Effect_Config of Choice_Config loop
               Configure_Effect (Choice, Effect_Config);
            end loop;
         end;
      end loop;

   end Configure_Event;

   ----------------------
   -- Configure_Events --
   ----------------------

   procedure Configure_Events (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "events");
   begin
      Load_Configurers;
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "event",
         Configure => Configure_Event'Access);
   end Configure_Events;

   ----------------------------
   -- Configure_Skill_Change --
   ----------------------------

   procedure Configure_Skill_Change
     (Choice : Concorde.Handles.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
   begin
      Concorde.Handles.Gain_Skill.Create
        (Event_Choice => Choice,
         Skill        =>
           Concorde.Handles.Skill.Get_By_Tag
             (Config.Config_Name),
         Level        => Config.Value);
   end Configure_Skill_Change;

   ----------------------
   -- Get_Change_Range --
   ----------------------

   procedure Get_Change_Range
     (Change : String;
      Low    : out Integer;
      High   : out Integer)
   is
   begin
      Low :=
        (if Change = "-d6" then -6
         elsif Change = "d6" then 1
         else Integer'Value (Change));
      High :=
        (if Change = "-d6" then -1
         elsif Change = "d6" then 6
         else Integer'Value (Change));
   end Get_Change_Range;

   ----------------------
   -- Load_Configurers --
   ----------------------

   procedure Load_Configurers is
   begin
      for Ability of
        Concorde.Handles.Ability.Scan_By_Tag
      loop
         Configure_Table.Insert
           (Ability.Tag,
            Configure_Ability_Change'Access);
      end loop;

      for Skill of
        Concorde.Handles.Skill.Scan_By_Tag
      loop
         Configure_Table.Insert
           (Skill.Tag,
            Configure_Skill_Change'Access);
      end loop;

      Configure_Table.Insert
        ("physical-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("mental-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("social-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("event", Configure_Child_Event'Access);
      Configure_Table.Insert
        ("redirect", Configure_Child_Event'Access);

   end Load_Configurers;

end Concorde.Configure.Events;
