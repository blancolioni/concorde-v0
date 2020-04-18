with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Db.Ability;
with Concorde.Db.Advancement;
with Concorde.Db.Advancement_Table;
with Concorde.Db.Assignment;
with Concorde.Db.Assignment_Rank;
with Concorde.Db.Career;
with Concorde.Db.Career_Benefit;
with Concorde.Db.Career_Mishap;
with Concorde.Db.Event;
with Concorde.Db.Skill;

package body Concorde.Configure.Careers is

   procedure Configure_Career
     (Career_Config : Tropos.Configuration);

   procedure Configure_Assignment
     (Career            : Concorde.Db.Career_Reference;
      Assignment_Config : Tropos.Configuration);

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Concorde.Db.Ability_Reference;
      Score   : out Natural);

   procedure Configure_Advancement
     (Advancement : Concorde.Db.Advancement_Reference;
      Config      : Tropos.Configuration);

   procedure Configure_Benefits
     (Career : Concorde.Db.Career_Reference;
      Config : Tropos.Configuration);

   procedure Configure_Advancement_Table
     (Career       : Concorde.Db.Career_Reference;
      Assignment   : Concorde.Db.Assignment_Reference;
      Training     : Concorde.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration);

   procedure Configure_Rank_Table
     (Assignment   : Concorde.Db.Assignment_Reference;
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
     (Advancement : Concorde.Db.Advancement_Reference;
      Config      : Tropos.Configuration)
   is
      use Concorde.Db;
      Advance_Cash : Concorde.Money.Money_Type := Concorde.Money.Zero;
      Advance_Ability : Ability_Reference := Null_Ability_Reference;
      Advance_Skill   : Skill_Reference := Null_Skill_Reference;
      Advance_Level   : Integer := -1;
   begin
      for Item_Config of Config loop
         declare
            Tag : constant String := Item_Config.Config_Name;
            Cash : constant Boolean := Tag = "cash";
            Ability : constant Ability_Reference :=
                        Concorde.Db.Ability.Get_Reference_By_Tag (Tag);
            Skill   : constant Skill_Reference :=
                        Concorde.Db.Skill.Get_Reference_By_Tag (Tag);
         begin
            if Cash then
               Advance_Cash :=
                 Concorde.Money.To_Money
                   (Real (Long_Float'(Item_Config.Value)));
            elsif Ability /= Null_Ability_Reference then
               Advance_Ability := Ability;
            elsif Skill /= Null_Skill_Reference then
               Advance_Skill := Skill;
               Advance_Level :=
                 (if Item_Config.Child_Count = 0
                  then -1
                  else Item_Config.Value);
            end if;
         end;
      end loop;

      Concorde.Db.Advancement.Update_Advancement (Advancement)
        .Set_Cash (Advance_Cash)
        .Set_Ability (Advance_Ability)
        .Set_Skill (Advance_Skill)
        .Set_Skill_Level (Advance_Level)
        .Done;

   end Configure_Advancement;

   ---------------------------------
   -- Configure_Advancement_Table --
   ---------------------------------

   procedure Configure_Advancement_Table
     (Career       : Concorde.Db.Career_Reference;
      Assignment   : Concorde.Db.Assignment_Reference;
      Training     : Concorde.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration)
   is
      DR : Positive := 1;
   begin
      for Config of Table_Config loop
         declare
            Row : constant Concorde.Db.Advancement_Table_Reference :=
                    Concorde.Db.Advancement_Table.Create
                      (Ability     => Concorde.Db.Null_Ability_Reference,
                       Cash        => Concorde.Money.Zero,
                       Skill       => Concorde.Db.Null_Skill_Reference,
                       Skill_Level => -1,
                       Table       => Training,
                       Career      => Career,
                       Assignment  => Assignment,
                       Dr          => DR);
         begin
            Configure_Advancement
              (Concorde.Db.Advancement_Table.Get (Row)
               .Get_Advancement_Reference,
               Config);
            DR := DR + 1;
         end;
      end loop;
   end Configure_Advancement_Table;

   --------------------------
   -- Configure_Assignment --
   --------------------------

   procedure Configure_Assignment
     (Career            : Concorde.Db.Career_Reference;
      Assignment_Config : Tropos.Configuration)
   is
      Survival_Ability : Concorde.Db.Ability_Reference;
      Survival_Check   : Natural;
      Advance_Ability  : Concorde.Db.Ability_Reference;
      Advance_Check    : Natural;

   begin

      Get_Ability_Score
        (Assignment_Config.Child ("survival"),
         Survival_Ability, Survival_Check);

      Get_Ability_Score
        (Assignment_Config.Child ("advance"),
         Advance_Ability, Advance_Check);

      declare
         Assignment : constant Concorde.Db.Assignment_Reference :=
                        Concorde.Db.Assignment.Create
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
     (Career : Concorde.Db.Career_Reference;
      Config : Tropos.Configuration)
   is
      Benefit_Index : Positive := 1;
   begin
      for Item_Config of Config loop
         declare
            Benefit : constant Concorde.Db.Career_Benefit_Reference :=
                        Concorde.Db.Career_Benefit.Create
                          (Ability     => Concorde.Db.Null_Ability_Reference,
                           Skill       => Concorde.Db.Null_Skill_Reference,
                           Skill_Level => -1,
                           Career      => Career,
                           Index       => Benefit_Index,
                           Cash        => Concorde.Money.Zero);
         begin
            Configure_Advancement
              (Concorde.Db.Career_Benefit.Get (Benefit)
               .Get_Advancement_Reference,
               Item_Config);
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
      Qualification    : Concorde.Db.Ability_Reference;
      Score            : Natural;
      Advanced_Ability : Concorde.Db.Ability_Reference;
      Advanced_Check   : Natural;
   begin
      Get_Ability_Score
        (Career_Config.Child ("qualification"),
         Qualification, Score);
      Get_Ability_Score
        (Career_Config.Child ("advanced-education"),
         Advanced_Ability, Advanced_Check);

      declare
         Career : constant Concorde.Db.Career_Reference :=
                    Concorde.Db.Career.Create
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
                 (Career, Concorde.Db.Null_Assignment_Reference, Training,
                  Advancement_Config);
            end;
         end loop;

         for Assignment_Config of Career_Config.Child ("assignment") loop
            Configure_Assignment (Career, Assignment_Config);
         end loop;

         for Mishap_Config of Career_Config.Child ("mishaps") loop
            declare
               use Concorde.Db;
               Event : constant Event_Reference :=
                         Concorde.Db.Event.Get_Reference_By_Tag
                           (Mishap_Config.Value);
            begin
               if Event = Null_Event_Reference then
                  Ada.Text_IO.Put_Line
                    ("undefined event: " & Mishap_Config.Value);
               else
                  Concorde.Db.Career_Mishap.Create
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
     (Assignment   : Concorde.Db.Assignment_Reference;
      Table_Config : Tropos.Configuration)
   is
      Rank : Natural := 0;
   begin
      for Rank_Config of Table_Config loop
         declare
            Row : constant Concorde.Db.Assignment_Rank_Reference :=
                    Concorde.Db.Assignment_Rank.Create
                      (Assignment  => Assignment,
                       Ability     => Concorde.Db.Null_Ability_Reference,
                       Cash        => Concorde.Money.Zero,
                       Skill       => Concorde.Db.Null_Skill_Reference,
                       Skill_Level => -1,
                       Tag         => Rank_Config.Config_Name,
                       Rank_Index  => Rank);
         begin
            Configure_Advancement
              (Concorde.Db.Assignment_Rank.Get (Row)
               .Get_Advancement_Reference,
               Rank_Config);
            Rank := Rank + 1;
         end;
      end loop;
   end Configure_Rank_Table;

   -----------------------
   -- Get_Ability_Score --
   -----------------------

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Concorde.Db.Ability_Reference;
      Score   : out Natural)
   is
   begin
      if Config.Child_Count = 0 then
         Ability := Concorde.Db.Null_Ability_Reference;
         Score := 0;
      elsif Config.Contains ("ability") then
         Ability :=
           Concorde.Db.Ability.Get_Reference_By_Tag
             (Config.Get ("ability"));
         Score := Config.Get ("score", 99);
      else
         declare
            Child : constant Tropos.Configuration :=
                      Config.Child (1);
         begin
            Ability :=
              Concorde.Db.Ability.Get_Reference_By_Tag
                (Child.Config_Name);
            Score := Child.Value;
         end;
      end if;
   end Get_Ability_Score;

end Concorde.Configure.Careers;
