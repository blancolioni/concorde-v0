with Concorde.Configure.Star_Systems;

with Concorde.Db.Faction;
with Concorde.Db.Generation;
with Concorde.Db.Scenario;
with Concorde.Db.Star;
with Concorde.Db.Star_System;
with Concorde.Db.World;

package body Concorde.Star_Systems is

   First_Star_System : Concorde.Db.Star_System_Reference :=
                         Concorde.Db.Null_Star_System_Reference;

   procedure Check_Worlds
     (Star_System : Concorde.Db.Star_System_Reference);

   ------------------
   -- Check_Worlds --
   ------------------

   procedure Check_Worlds
     (Star_System : Concorde.Db.Star_System_Reference)
   is
      Is_Gen : constant Concorde.Db.Is_Generated_Reference :=
                 Concorde.Db.Star_System.Get (Star_System)
                 .Get_Is_Generated_Reference;
      Gen    : constant Concorde.Db.Generation.Generation_Type :=
                 Concorde.Db.Generation.Get_By_Is_Generated
                   (Is_Gen);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Concorde.Configure.Star_Systems.Generate_Star_System
           (Star_System);
         if Gen.Has_Element then
            Concorde.Db.Generation.Update_Generation
              (Gen.Get_Generation_Reference)
              .Set_Ready (True)
              .Done;
         else
            Concorde.Db.Generation.Create (Is_Gen, True);
         end if;
      end if;
   end Check_Worlds;

   -----------
   -- Claim --
   -----------

   procedure Claim
     (Star_System : Concorde.Db.Star_System_Reference)
   is
   begin
      Concorde.Db.Star_System.Update_Star_System (Star_System)
        .Set_Claimed (True)
        .Done;
   end Claim;

   -------------
   -- Claimed --
   -------------

   function Claimed
     (Star_System : Concorde.Db.Star_System_Reference)
      return Boolean
   is
   begin
      return Concorde.Db.Star_System.Get (Star_System).Claimed;
   end Claimed;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact
     (Name : String)
      return Concorde.Db.Star_System_Reference
   is
   begin
      return Concorde.Db.Star_System.First_Reference_By_Name (Name);
   end Find_Exact;

   -----------
   -- First --
   -----------

   function First return Concorde.Db.Star_System_Reference is
      use Concorde.Db;
   begin
      if First_Star_System = Null_Star_System_Reference then
         declare
            Scenario : constant Concorde.Db.Scenario.Scenario_Type :=
                         Concorde.Db.Scenario.Get_By_Active (True);
         begin
            if Scenario.Has_Element then
               First_Star_System := Scenario.Central_System;
            end if;
         end;
      end if;

      if First_Star_System = Null_Star_System_Reference then
         First_Star_System :=
           Concorde.Db.Star_System.First_Reference_By_Top_Record
             (Concorde.Db.R_Star_System);
      end if;

      return First_Star_System;
   end First;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Concorde.Db.Star_System_Reference)
      return Star_System_Type
   is
   begin
      return Star_System_Type'(Reference => Reference);
   end Get;

   ---------------------
   -- Has_Star_System --
   ---------------------

   function Has_Star_System
     (Star_System : Star_System_Type'Class)
      return Boolean
   is
      use type Concorde.Db.Star_System_Reference;
   begin
      return Star_System.Reference /= Concorde.Db.Null_Star_System_Reference;
   end Has_Star_System;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Star_System_Type'Class)
      return String
   is
   begin
      return Concorde.Db.Star_System.Get (Star_System.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Concorde.Db.Star_System_Reference)
      return String
   is
   begin
      return Concorde.Db.Star_System.Get (Star_System).Name;
   end Name;

   function Owner
     (Star_System : Star_System_Type'Class)
      return Concorde.Factions.Faction_Type'Class
   is
   begin
      return Concorde.Factions.Get
        (Concorde.Db.Faction.First_Reference_By_Capital_System
           (Star_System.Reference));
   end Owner;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Concorde.Db.Star_System_Reference)
      return Interstellar_Position
   is
      Rec : constant Concorde.Db.Star_System.Star_System_Type :=
              Concorde.Db.Star_System.Get (Star_System);
   begin
      return (Rec.X, Rec.Y, Rec.Z);
   end Position;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Star_System_Type'Class)
      return Interstellar_Position
   is
   begin
      return Position (Star_System.Reference);
   end Position;

   -------------
   -- Primary --
   -------------

   function Primary
     (Star_System : Star_System_Type'Class)
      return Concorde.Stars.Star_Type'Class
   is
   begin
      return Concorde.Stars.Get
        (Concorde.Db.Star.First_Reference_By_Star_System
           (Star_System.Reference));
   end Primary;

   ------------------------
   -- Terrestrial_Worlds --
   ------------------------

   function Terrestrial_Worlds
     (Star_System : Concorde.Db.Star_System_Reference)
      return Concorde.Worlds.World_Selection
   is
   begin
      return Selection : Concorde.Worlds.World_Selection :=
        Worlds (Star_System)
      do
         Selection.Filter (Concorde.Worlds.Is_Terrestrial'Access);
      end return;
   end Terrestrial_Worlds;

   ------------
   -- Worlds --
   ------------

   function Worlds
     (Star_System : Concorde.Db.Star_System_Reference)
      return Concorde.Worlds.World_Selection
   is
   begin
      Check_Worlds (Star_System);
      return Selection : Concorde.Worlds.World_Selection do
         for World of
           Concorde.Db.World.Select_By_Star_System
             (Star_System)
         loop
            Selection.Insert (World.Get_World_Reference);
         end loop;
      end return;
   end Worlds;

end Concorde.Star_Systems;
