with Concorde.Configure.Star_Systems;

with Concorde.Handles.Generation;
with Concorde.Handles.Scenario;
with Concorde.Handles.World;

with Concorde.Db;

package body Concorde.Star_Systems is

   First_Star_System : Concorde.Handles.Star_System.Star_System_Handle :=
                         Concorde.Handles.Star_System.Empty_Handle;

   procedure Check_Worlds
     (Star_System : Concorde.Handles.Star_System.Star_System_Class);

   ------------------
   -- Check_Worlds --
   ------------------

   procedure Check_Worlds
     (Star_System : Concorde.Handles.Star_System.Star_System_Class)
   is
      Gen    : constant Concorde.Handles.Generation.Generation_Handle :=
                 Concorde.Handles.Generation.Get_By_Is_Generated
                   (Star_System);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Concorde.Configure.Star_Systems.Generate_Star_System
           (Star_System);
         if Gen.Has_Element then
            Gen.Update_Generation
              .Set_Ready (True)
              .Done;
         else
            Concorde.Handles.Generation.Create (Star_System, True);
         end if;
      end if;
   end Check_Worlds;

   -----------
   -- Claim --
   -----------

   procedure Claim
     (Star_System : Star_System_Class)
   is
   begin
      Concorde.Handles.Star_System.Update_Star_System (Star_System)
        .Set_Claimed (True)
        .Done;
   end Claim;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact
     (Name : String)
      return Concorde.Handles.Star_System.Star_System_Class
   is
   begin
      return Concorde.Handles.Star_System.First_By_Name (Name);
   end Find_Exact;

   -----------
   -- First --
   -----------

   function First return Concorde.Handles.Star_System.Star_System_Class is
   begin
      if not First_Star_System.Has_Element then
         declare
            Scenario : constant Concorde.Handles.Scenario.Scenario_Handle :=
                         Concorde.Handles.Scenario.Get_By_Active (True);
         begin
            if Scenario.Has_Element then
               First_Star_System :=
                 Scenario.Central_System.To_Star_System_Handle;
            end if;
         end;
      end if;

      if not First_Star_System.Has_Element then
         First_Star_System :=
           Concorde.Handles.Star_System.First_By_Top_Record
             (Concorde.Db.R_Star_System);
      end if;

      return First_Star_System;
   end First;

   function Owner
     (Star_System : Star_System_Class)
      return Concorde.Handles.Faction.Faction_Class
   is
   begin
      return Concorde.Handles.Faction.First_By_Capital_System
        (Star_System);
   end Owner;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Star_System_Class)
      return Interstellar_Position
   is
   begin
      return (Star_System.X, Star_System.Y, Star_System.Z);
   end Position;

   -------------
   -- Primary --
   -------------

   function Primary
     (Star_System : Star_System_Class)
      return Concorde.Handles.Star.Star_Class
   is
   begin
      return Concorde.Handles.Star.First_By_Star_System (Star_System);
   end Primary;

   ------------------------
   -- Terrestrial_Worlds --
   ------------------------

   function Terrestrial_Worlds
     (Star_System : Star_System_Class)
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
     (Star_System : Star_System_Class)
      return Concorde.Worlds.World_Selection
   is
   begin
      Check_Worlds (Star_System);
      return Selection : Concorde.Worlds.World_Selection do
         for World of
           Concorde.Handles.World.Select_By_Star_System
             (Star_System)
         loop
            Selection.Insert (World.To_World_Handle);
         end loop;
      end return;
   end Worlds;

end Concorde.Star_Systems;
