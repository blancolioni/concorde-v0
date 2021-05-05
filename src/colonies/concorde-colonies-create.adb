with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;

with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Money;
with Concorde.Pops;
with Concorde.Quantities;

with Concorde.Configure.Commodities;

with Concorde.Handles.Colony;
with Concorde.Handles.Deposit;
with Concorde.Handles.Facility;
with Concorde.Handles.Facility_Worker;
with Concorde.Handles.Installation;
with Concorde.Handles.Owned_World;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Resource;
with Concorde.Handles.Terrain;

package body Concorde.Colonies.Create is

   function Is_Good_Homeworld
     (World : Concorde.Handles.World.World_Class)
      return Boolean
     with Unreferenced;

   function Evaluate_Capital_Sector
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Non_Negative_Real
     with Unreferenced;

   -----------------------------
   -- Evaluate_Capital_Sector --
   -----------------------------

   function Evaluate_Capital_Sector
     (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
      return Non_Negative_Real
   is
   begin
      if Sector.Elevation < Sector.World.Sea_Level then
         return 0.0;
      end if;
      return Sector.Habitability;
   end Evaluate_Capital_Sector;

   -------------------
   -- Create_Colony --
   -------------------

   procedure Initial_Colony
     (Faction : Concorde.Handles.Faction.Faction_Class;
      World   : Concorde.Handles.World.World_Class;
      Capital : Concorde.Handles.World_Sector.World_Sector_Class;
      Init    : Tropos.Configuration)
   is
      use Concorde.Quantities;

      package Pop_Group_Maps is
        new WL.String_Maps (Quantity_Type);

      Required_Pops : Pop_Group_Maps.Map;

      procedure Add_Worker_Population
        (Facility : Concorde.Handles.Facility.Facility_Handle'Class);

      ---------------------------
      -- Add_Worker_Population --
      ---------------------------

      procedure Add_Worker_Population
        (Facility : Concorde.Handles.Facility.Facility_Handle'Class)
      is
      begin
         for Facility_Worker of
           Concorde.Handles.Facility_Worker.Select_By_Facility
             (Facility)
         loop
            declare
               Tag : constant String := Facility_Worker.Pop_Group.Tag;
            begin
               if not Required_Pops.Contains (Tag) then
                  Required_Pops.Insert
                    (Tag, Facility_Worker.Quantity);
               else
                  declare
                     Q : Quantity_Type renames Required_Pops (Tag);
                  begin
                     Q := Q + Facility_Worker.Quantity;
                  end;
               end if;
            end;
         end loop;
      end Add_Worker_Population;

      Colony : constant Concorde.Handles.Colony.Colony_Handle :=
                 Concorde.Handles.Colony.Create
                   (Account    => Concorde.Agents.New_Account
                                 (Concorde.Money.Zero, Faction.Account),
                    Last_Earn  => Concorde.Money.Zero,
                    Last_Spend => Concorde.Money.Zero,
                    Active     => True,
                    Scheduled  => False,
                    Next_Event => Concorde.Calendar.Clock,
                    Manager    => "default-colony",
                    Identifier => Concorde.Identifiers.Next_Identifier,
                    World      => World,
                    Faction    => Faction,
                    Capital    => Capital,
                    Plurality  => 1.0);
   begin
      for Capital_Installation of Init.Child ("capital") loop
         declare
            Facility : constant Handles.Facility.Facility_Handle'Class :=
                         Concorde.Handles.Facility.Get_By_Tag
                           (Capital_Installation.Config_Name);
            Count    : constant Natural :=
                         (if Capital_Installation.Contains ("count")
                          then Capital_Installation.Get ("count")
                          else Capital_Installation.Value);
            Manager  : constant String :=
                         (if Facility.Default_Manager = ""
                          then "default-installation"
                          else Facility.Default_Manager);
         begin
            for I in 1 .. Count loop
               declare
                  Installation : constant Concorde.Handles.Installation
                    .Installation_Handle :=
                      Concorde.Handles.Installation.Create
                        (Account      =>
                               Concorde.Agents.New_Account
                           (Concorde.Money.Zero, Faction.Account),
                         Last_Earn    => Concorde.Money.Zero,
                         Last_Spend   => Concorde.Money.Zero,
                         Identifier   => Concorde.Identifiers.Next_Identifier,
                         World_Sector => Capital,
                         Colony       => Colony,
                         Facility     => Facility,
                         Active       => True,
                         Scheduled    => False,
                         Next_Event   => Concorde.Calendar.Clock,
                         Manager      => Manager);
               begin
                  Add_Worker_Population (Facility);
                  if Capital_Installation.Contains ("stock") then
                     Concorde.Configure.Commodities.Configure_Stock
                       (Has_Stock => Installation,
                        Config    => Capital_Installation.Child ("stock"));
                  end if;
               end;
            end loop;
         end;
      end loop;

      declare

         package Terrain_Lists is
           new Ada.Containers.Doubly_Linked_Lists
             (Concorde.Handles.Terrain.Terrain_Handle,
              Concorde.Handles.Terrain."=");

         package Resource_Lists is
           new Ada.Containers.Doubly_Linked_Lists
             (Concorde.Handles.Resource.Resource_Handle,
              Concorde.Handles.Resource."=");

         type Facility_Info is
            record
               Facility  : Concorde.Handles.Facility.Facility_Handle;
               Count     : Positive;
               Terrain   : Terrain_Lists.List;
               Resources : Resource_Lists.List;
            end record;

         package Facility_Lists is
           new Ada.Containers.Doubly_Linked_Lists (Facility_Info);

         Facility_List : Facility_Lists.List;

         function Check_Sector
           (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
            return Boolean;

         --  function Score_Facility
         --    (Sector    : Concorde.Handles.World_Sector.World_Sector_Class;
         --     Facility  : Concorde.Handles.Facility.Facility_Class)
         --     return Natural
         --    with Unreferenced;

         ------------------
         -- Check_Sector --
         ------------------

         function Check_Sector
           (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
            return Boolean
         is
            use type Concorde.Handles.World_Sector.World_Sector_Class;
            Best_Facility : Facility_Lists.Cursor := Facility_Lists.No_Element;
         begin
            if Facility_List.Is_Empty then
               return False;
            end if;

            if Sector = Capital then
               return True;
            end if;

            for Position in Facility_List.Iterate loop
               declare
                  Info : constant Facility_Info :=
                           Facility_Lists.Element (Position);
               begin
                  if Info.Terrain.Contains
                    (Sector.Terrain.To_Terrain_Handle)
                  then
                     Best_Facility := Position;
                     exit;
                  end if;
               end;
            end loop;

            if Facility_Lists.Has_Element (Best_Facility) then

               declare
                  Info : constant Facility_Info :=
                           Facility_List (Best_Facility);
                  Manager  : constant String :=
                               (if Info.Facility.Default_Manager = ""
                                then "default-installation"
                                else Info.Facility.Default_Manager);
               begin
                  for Resource of Concorde.Handles.Resource.Scan_By_Tag loop
                     if Info.Resources.Contains
                       (Resource.To_Resource_Handle)
                     then
                        Concorde.Handles.Deposit.Create
                          (World         => Sector.World,
                           World_Sector  => Sector,
                           Resource      => Resource,
                           Concentration => 0.75,
                           Difficulty    => 0.25,
                           Available     =>
                             Concorde.Quantities.To_Quantity (1.0e9));
                     end if;
                  end loop;

                  Sector.Update_World_Sector
                    .Set_Faction (Faction)
                    .Done;

                  Concorde.Handles.Installation.Create
                    (Account      =>
                       Concorde.Agents.New_Account
                         (Concorde.Money.Zero, Faction.Account),
                     Last_Earn    => Concorde.Money.Zero,
                     Last_Spend   => Concorde.Money.Zero,
                     Identifier   => Concorde.Identifiers.Next_Identifier,
                     World_Sector => Sector,
                     Colony       => Colony,
                     Facility     => Info.Facility,
                     Active       => True,
                     Scheduled    => False,
                     Next_Event   => Concorde.Calendar.Clock,
                     Manager      => Manager);

                  Add_Worker_Population (Info.Facility);

                  if Info.Count = 1 then
                     Facility_List.Delete (Best_Facility);
                  else
                     Facility_List.Replace_Element
                       (Best_Facility,
                        (Info with delta Count => Info.Count - 1));
                  end if;
               end;
            end if;

            return True;

         end Check_Sector;

         --------------------
         -- Score_Facility --
         --------------------

         --  function Score_Facility
         --    (Sector    : Concorde.Handles.World_Sector.World_Sector_Class;
         --     Facility  : Concorde.Handles.Facility.Facility_Class)
         --     return Natural
         --  is
         --     Score : Natural := 0;
         --
         --     Handle : constant Handles.Facility.Facility_Handle'Class :=
         --                Concorde.Handles.Facility.Get (Facility);
         --
         --     procedure Check_Deposit
         --       (Deposit : Deposit_Reference);
         --
         --     -------------------
         --     -- Check_Deposit --
         --     -------------------
         --
         --     procedure Check_Deposit
         --       (Deposit : Deposit_Reference)
         --     is
         --        Resource : constant Commodity_Reference :=
         --                 Concorde.Handles.Deposit.Get (Deposit).Resource;
         --     begin
         --        if Handle.Produces_Commodity (Resource) then
         --           Score := Score + 1;
         --        end if;
         --     end Check_Deposit;
         --
         --  begin
         --     Concorde.Handles.Sector.Get (Sector).Iterate_Deposits
         --       (Check_Deposit'Access);
         --     return Score;
         --  end Score_Facility;

      begin
         for Facility_Config of Init.Child ("installations") loop
            declare
               Info : Facility_Info := Facility_Info'
                 (Facility  =>
                    Concorde.Handles.Facility.Get_By_Tag
                      (Facility_Config.Config_Name),
                  Count     => Facility_Config.Get ("count"),
                  Terrain   => <>,
                  Resources => <>);
            begin
               for Terrain_Config of Facility_Config.Child ("terrain") loop
                  Info.Terrain.Append
                    (Concorde.Handles.Terrain.Get_By_Tag
                       (Terrain_Config.Config_Name));
               end loop;

               for Resource_Config of Facility_Config.Child ("resources") loop
                  Info.Resources.Append
                    (Concorde.Handles.Resource.Get_By_Tag
                       (Resource_Config.Config_Name));
               end loop;

               Facility_List.Append (Info);

            end;
         end loop;

         for Sector of
           Concorde.Handles.World_Sector.Select_By_World
             (World)
         loop
            exit when not Check_Sector (Sector);
         end loop;
      end;

      for Position in Required_Pops.Iterate loop
         declare
            Pop_Group : constant Handles.Pop_Group.Pop_Group_Handle :=
                          Handles.Pop_Group.Get_By_Tag
                            (Pop_Group_Maps.Key (Position));
            Size      : constant Concorde.Quantities.Quantity_Type :=
                          Pop_Group_Maps.Element (Position);
            Cash      : constant Concorde.Money.Money_Type :=
                          Concorde.Money.Adjust
                            (Concorde.Money.Total
                               (Price         => Pop_Group.Base_Price,
                                Quantity_Type => Size),
                             10.0);
         begin
            Concorde.Pops.New_Pop
              (Faction => Faction,
               Colony  => Colony,
               Sector  => Capital,
               Group   => Pop_Group,
               Size    => Size,
               Cash    => Cash);
         end;
      end loop;

   end Initial_Colony;

   -----------------------
   -- Is_Good_Homeworld --
   -----------------------

   function Is_Good_Homeworld
     (World : Concorde.Handles.World.World_Class)
      return Boolean
   is
   begin
      return not Concorde.Handles.Owned_World
        .First_By_World (World)
        .Has_Element
        and then World.Habitability >= 0.8;
   end Is_Good_Homeworld;

end Concorde.Colonies.Create;
