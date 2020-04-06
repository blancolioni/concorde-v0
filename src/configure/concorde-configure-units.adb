with Tropos.Reader;

with Concorde.Configure.Commodities;

with Concorde.Db.Movement;
with Concorde.Db.Unit;

package body Concorde.Configure.Units is

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration);

   function Get_Movement
     (Tag : String)
      return Concorde.Db.Movement_Reference;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Real (Float'(Unit_Config.Get (Name, 0.0))));

      Unit : constant Concorde.Db.Unit_Reference :=
        Concorde.Db.Unit.Create
          (Tag        => Unit_Config.Config_Name,
           Enabled_By => Concorde.Db.Null_Technology_Reference,
           Movement   =>
             Get_Movement (Unit_Config.Get ("movement-type")),
           Base_Speed => Get ("movement-speed"),
           Armour     => Get ("armour"),
           Attack     => Get ("attack"),
           Discipline => Get ("discipline"),
           Recon      => Get ("recon"),
           Camoflage  => Get ("camoflage"));

      Constructed : constant Concorde.Db.Constructed_Reference :=
        Concorde.Db.Unit.Get (Unit).Get_Constructed_Reference;
      Supplied    : constant Concorde.Db.Supplied_Reference :=
        Concorde.Db.Unit.Get (Unit).Get_Supplied_Reference;
   begin
      Concorde.Configure.Commodities.Configure_Constructed
        (Constructed, Unit_Config);
      Concorde.Configure.Commodities.Configure_Supplied
        (Supplied, Unit_Config);
   end Configure_Unit;

   ---------------------
   -- Configure_Units --
   ---------------------

   procedure Configure_Units (Scenario_Name : String) is
   begin
      for Unit_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "units"),
           "unit")
      loop
         Configure_Unit (Unit_Config);
      end loop;
   end Configure_Units;

   ------------------
   -- Get_Movement --
   ------------------

   function Get_Movement
     (Tag : String)
      return Concorde.Db.Movement_Reference
   is
      use Concorde.Db;
      Ref : constant Movement_Reference :=
        Movement.Get_Reference_By_Tag (Tag);
   begin
      if Ref = Null_Movement_Reference then
         return Movement.Create (Tag);
      else
         return Ref;
      end if;
   end Get_Movement;

end Concorde.Configure.Units;
