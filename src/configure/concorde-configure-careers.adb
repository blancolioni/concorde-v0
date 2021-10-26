with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Handles.Ability;
with Concorde.Handles.Advancement_Table;
with Concorde.Handles.Assignment;
with Concorde.Handles.Assignment_Rank;
with Concorde.Handles.Career;
with Concorde.Handles.Career_Benefit;
with Concorde.Handles.Career_Mishap;
with Concorde.Handles.Event;
with Concorde.Handles.Skill;

with Concorde.Db;

package body Concorde.Configure.Careers is

   procedure Configure_Career
     (Career_Config : Tropos.Configuration);

   procedure Configure_Assignment
     (Career            : Concorde.Handles.Career.Career_Class;
      Assignment_Config : Tropos.Configuration);

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Concorde.Handles.Ability.Ability_Handle;
      Score   : out Natural);

   procedure Configure_Advancement
     (Config          : Tropos.Configuration;
      Advance_Cash    : out Concorde.Money.Money_Type;
      Advance_Ability : out Concorde.Handles.Ability.Ability_Handle;
      Advance_Skill   : out Concorde.Handles.Skill.Skill_Handle;
      Advance_Level   : out Integer);

   procedure Configure_Benefits
     (Career : Concorde.Handles.Career.Career_Class;
      Config : Tropos.Configuration);

   procedure Configure_Advancement_Table
     (Career       : Concorde.Handles.Career.Career_Class;
      Assignment   : Concorde.Handles.Assignment.Assignment_Class;
      Training     : Concorde.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration);

   procedure Configure_Rank_Table
     (Assignment   : Concorde.Handles.Assignment.Assignment_Class;
      Table_Config : Tropos.Configuration);

   function Get_Skill_Training
     (Tag : String)
      return Concorde.Db.Skill_Training_Type
   is (if Tag = "personal-development"
       then Concorde.Db.Personal_Development
       elsif Tag = "service-skills"
       then Concorde.Db.Service_Skills
       elsif Tag = "advanced-education"
       then Concorde.Db.Advanced_Skills
       elsif Tag = "advancement-table"
       then Concorde.Db.Assignment_Training
       elsif Tag = "commission"
       then Concorde.Db.Commission_Skills
       else raise Constraint_Error with
         "undefined skill training tag: " & Tag);

   ---------------------------
   -- Configure_Advancement --
   ---------------------------

   procedure Configure_Advancement
     (Config          : Tropos.Configuration;
      Advance_Cash    : out Concorde.Money.Money_Type;
      Advance_Ability : out Concorde.Handles.Ability.Ability_Handle;
      Advance_Skill   : out Concorde.Handles.Skill.Skill_Handle;
      Advance_Level   : out Integer)
   is
      use Concorde.Handles.Ability;
      use Concorde.Handles.Skill;
   begin
      for Item_Config of Config loop
         declare
            Tag : constant String := Item_Config.Config_Name;
            Cash : constant Boolean := Tag = "cash";
            Ability : constant Ability_Handle :=
                        Concorde.Handles.Ability.Get_By_Tag (Tag);
            Skill   : constant Skill_Handle :=
                        Concorde.Handles.Skill.Get_By_Tag (Tag);
         begin
            if Cash then
               Advance_Cash :=
                 Concorde.Money.To_Money
                   (Real (Long_Float'(Item_Config.Value)));
            elsif Ability.Has_Element then
               Advance_Ability := Ability;
            elsif Skill.Has_Element then
               Advance_Skill := Skill;
               Advance_Level :=
                 (if Item_Config.Child_Count = 0
                  then -1
                  else Item_Config.Value);
            end if;
         end;
      end loop;

   end Configure_Advancement;

   ---------------------------------
   -- Configure_Advancement_Table --
   ---------------------------------

   procedure Configure_Advancement_Table
     (Career       : Concorde.Handles.Career.Career_Class;
      Assignment   : Concorde.Handles.Assignment.Assignment_Class;
      Training     : Concorde.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration)
   is
      DR : Positive := 1;
   begin
      for Config of Table_Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Concorde.Handles.Ability.Ability_Handle;
            Advance_Skill   : Concorde.Handles.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Concorde.Handles.Advancement_Table.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Table       => Training,
               Career      => Career,
               Assignment  => Assignment,
               Dr          => DR);
            DR := DR + 1;
         end;
      end loop;
   end Configure_Advancement_Table;

   --------------------------
   -- Configure_Assignment --
   --------------------------

   procedure Configure_Assignment
     (Career            : Concorde.Handles.Career.Career_Class;
      Assignment_Config : Tropos.Configuration)
   is
      Survival_Ability : Concorde.Handles.Ability.Ability_Handle;
      Survival_Check   : Natural;
      Advance_Ability  : Concorde.Handles.Ability.Ability_Handle;
      Advance_Check    : Natural;

   begin

      Get_Ability_Score
        (Assignment_Config.Child ("survival"),
         Survival_Ability, Survival_Check);

      Get_Ability_Score
        (Assignment_Config.Child ("advance"),
         Advance_Ability, Advance_Check);

      declare
         Assignment : constant Concorde.Handles.Assignment.Assignment_Handle :=
                        Concorde.Handles.Assignment.Create
                          (Tag              => Assignment_Config.Config_Name,
                           Career           => Career,
                           Survival_Ability => Survival_Ability,
                           Survival_Check   => Survival_Check,
                           Advance_Ability  => Advance_Ability,
                           Advance_Check    => Advance_Check);
      begin
         Configure_Advancement_Table
           (Career       => Career,
            Assignment   => Assignment,
            Training     => Concorde.Db.Assignment_Training,
            Table_Config => Assignment_Config.Child ("advancement-table"));
         Configure_Rank_Table
           (Assignment   => Assignment,
            Table_Config => Assignment_Config.Child ("ranks"));
      end;

   end Configure_Assignment;

   ------------------------
   -- Configure_Benefits --
   ------------------------

   procedure Configure_Benefits
     (Career : Concorde.Handles.Career.Career_Class;
      Config : Tropos.Configuration)
   is
      Benefit_Index : Positive := 1;
   begin
      for Item_Config of Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Concorde.Handles.Ability.Ability_Handle;
            Advance_Skill   : Concorde.Handles.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Item_Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Concorde.Handles.Career_Benefit.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Career      => Career,
               Index       => Benefit_Index);
            Benefit_Index := Benefit_Index + 1;
         end;
      end loop;
   end Configure_Benefits;

   ----------------------
   -- Configure_Career --
   ----------------------

   procedure Configure_Career
     (Career_Config : Tropos.Configuration)
   is
      Qualification    : Concorde.Handles.Ability.Ability_Handle;
      Score            : Natural;
      Advanced_Ability : Concorde.Handles.Ability.Ability_Handle;
      Advanced_Check   : Natural;
   begin
      Get_Ability_Score
        (Career_Config.Child ("qualification"),
         Qualification, Score);
      Get_Ability_Score
        (Career_Config.Child ("advanced-education"),
         Advanced_Ability, Advanced_Check);

      declare
         Career : constant Concorde.Handles.Career.Career_Class :=
                    Concorde.Handles.Career.Create
                      (Tag              => Career_Config.Config_Name,
                       Qualification    => Qualification,
                       Check            => Score,
                       Advanced_Ability => Advanced_Ability,
                       Advanced_Check   => Advanced_Check,
                       Has_Commission   =>
                         Career_Config.Get ("has-commission"),
                       Basic_Training   =>
                         Get_Skill_Training
                           (Career_Config.Get
                                ("basic-training",
                                 "service-skills")));

      begin
         Configure_Benefits (Career, Career_Config.Child ("benefits"));

         for Advancement_Config of
           Career_Config.Child ("advancement-tables")
         loop
            declare
               Training : constant Concorde.Db.Skill_Training_Type :=
                            Get_Skill_Training
                              (Advancement_Config.Config_Name);
            begin
               Configure_Advancement_Table
                 (Career, Concorde.Handles.Assignment.Empty_Handle, Training,
                  Advancement_Config);
            end;
         end loop;

         for Assignment_Config of Career_Config.Child ("assignment") loop
            Configure_Assignment (Career, Assignment_Config);
         end loop;

         for Mishap_Config of Career_Config.Child ("mishaps") loop
            declare
               use Concorde.Handles.Event;
               Event : constant Event_Handle :=
                         Concorde.Handles.Event.Get_By_Tag
                           (Mishap_Config.Value);
            begin
               if not Event.Has_Element then
                  Ada.Text_IO.Put_Line
                    ("undefined event: " & Mishap_Config.Value);
               else
                  Concorde.Handles.Career_Mishap.Create
                    (Career => Career,
                     Event  => Event);
               end if;
            end;
         end loop;
      end;

   end Configure_Career;

   -----------------------
   -- Configure_Careers --
   -----------------------

   procedure Configure_Careers (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "careers");
   begin
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "career",
         Configure => Configure_Career'Access);
   end Configure_Careers;

   --------------------------
   -- Configure_Rank_Table --
   --------------------------

   procedure Configure_Rank_Table
     (Assignment   : Concorde.Handles.Assignment.Assignment_Class;
      Table_Config : Tropos.Configuration)
   is
      Rank : Natural := 0;
   begin
      for Rank_Config of Table_Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Concorde.Handles.Ability.Ability_Handle;
            Advance_Skill   : Concorde.Handles.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Rank_Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Concorde.Handles.Assignment_Rank.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Tag         => Rank_Config.Config_Name,
               Assignment  => Assignment,
               Rank_Index  => Rank);
            Rank := Rank + 1;
         end;
      end loop;
   end Configure_Rank_Table;

   -----------------------
   -- Get_Ability_Score --
   -----------------------

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Concorde.Handles.Ability.Ability_Handle;
      Score   : out Natural)
   is
   begin
      if Config.Child_Count = 0 then
         Ability := Concorde.Handles.Ability.Empty_Handle;
         Score := 0;
      elsif Config.Contains ("ability") then
         Ability :=
           Concorde.Handles.Ability.Get_By_Tag
             (Config.Get ("ability"));
         Score := Config.Get ("score", 99);
      else
         declare
            Child : constant Tropos.Configuration :=
                      Config.Child (1);
         begin
            Ability :=
              Concorde.Handles.Ability.Get_By_Tag
                (Child.Config_Name);
            Score := Child.Value;
         end;
      end if;
   end Get_Ability_Score;

end Concorde.Configure.Careers;
