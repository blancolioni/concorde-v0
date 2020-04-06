with Tropos.Reader;

with Concorde.Db.Ability;
with Concorde.Db.Assignment;
with Concorde.Db.Career;

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

      Concorde.Db.Assignment.Create
        (Tag              => Assignment_Config.Config_Name,
         Career           => Career,
         Survival_Ability => Survival_Ability,
         Survival_Check   => Survival_Check,
         Advance_Ability  => Advance_Ability,
         Advance_Check    => Advance_Check);

   end Configure_Assignment;

   ----------------------
   -- Configure_Career --
   ----------------------

   procedure Configure_Career
     (Career_Config : Tropos.Configuration)
   is
      Qualification : Concorde.Db.Ability_Reference;
      Score         : Natural;
   begin
      Get_Ability_Score (Career_Config.Child ("qualification"),
                         Qualification, Score);

      declare
         Career : constant Concorde.Db.Career_Reference :=
                    Concorde.Db.Career.Create
                      (Tag           => Career_Config.Config_Name,
                       Qualification => Qualification,
                       Check         => Score);
      begin
         for Assignment_Config of Career_Config.Child ("assignment") loop
            Configure_Assignment (Career, Assignment_Config);
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

   -----------------------
   -- Get_Ability_Score --
   -----------------------

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Concorde.Db.Ability_Reference;
      Score   : out Natural)
   is
   begin
      if Config.Contains ("ability") then
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
