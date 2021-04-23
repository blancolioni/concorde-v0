with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Configure;
with Concorde.Identifiers;
with Concorde.Money;

with Concorde.Agents;
with Concorde.Star_Systems;
with Concorde.Worlds;

with Concorde.Colonies.Create;

with Concorde.Handles.Account;
with Concorde.Handles.Company;
with Concorde.Handles.Deposit;
with Concorde.Handles.Market;
with Concorde.Handles.Owned_World;
with Concorde.Handles.Script;
with Concorde.Handles.Script_Line;
with Concorde.Handles.Shareholder;
with Concorde.Handles.Star_System;
with Concorde.Handles.Star_System_Distance;
with Concorde.Handles.World;
with Concorde.Handles.World_Sector;

package body Concorde.Factions.Create is

--     Log_Faction_Creation : constant Boolean := False;

   Faction_Company_Shares : constant := 1000;

   function Find_Homeworld
     return Concorde.Handles.World.World_Class;

   function Find_Home_Sector
     (World : Concorde.Handles.World.World_Class)
      return Concorde.Handles.World_Sector.World_Sector_Class;

   --------------------
   -- Create_Faction --
   --------------------

   function Create_Faction
     (User        : Concorde.Handles.User.User_Class;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Concorde.Handles.Faction.Faction_Class
   is
      Capital : constant Concorde.Handles.World.World_Class :=
                  Find_Homeworld;
   begin
      if not Capital.Has_Element then
         return Concorde.Handles.Faction.Empty_Handle;
      end if;

      declare
         Cash    : constant Concorde.Money.Money_Type :=
                     Concorde.Configure.Configure_Money
                       (Setup, "cash", 1000.0);
         Account : constant Concorde.Handles.Account.Account_Handle :=
                     Concorde.Agents.New_Account
                       (Starting_Balance => Cash,
                        Guarantor        =>
                          Concorde.Handles.Account.Empty_Handle);
         Sector  : constant Concorde.Handles.World_Sector.World_Sector_Class :=
           Find_Home_Sector (Capital);
         Faction : constant Concorde.Handles.Faction.Faction_Handle :=
                     Concorde.Handles.Faction.Create
                       (Identifier    => Concorde.Identifiers.Next_Identifier,
                        Name          => Name,
                        Adjective     =>
                          (if Adjective = "" then Name else Adjective),
                        Plural_Name   =>
                          (if Plural_Name = "" then Name else Plural_Name),
                        Active        => True,
                        Scheduled     => False,
                        Next_Event    => Concorde.Calendar.Clock,
                        Manager       => "default-faction",
                        Account       => Account,
                        Last_Earn     => Concorde.Money.Zero,
                        Last_Spend    => Concorde.Money.Zero,
                        Human         => False,
                        Red           => Color.Red,
                        Green         => Color.Green,
                        Blue          => Color.Blue,
                        User          => User,
                        Capital_System => Capital.Star_System,
                        Capital_World  => Capital);
         Company    : constant Concorde.Handles.Company.Company_Handle :=
           Concorde.Handles.Company.Create
                          (Identifier   =>
                                      Concorde.Identifiers.Next_Identifier,
                           Account      =>
                             Concorde.Agents.New_Account (Concorde.Money.Zero),
                           Last_Earn     => Concorde.Money.Zero,
                           Last_Spend    => Concorde.Money.Zero,
                           Name          => Name,
                           Active        => True,
                           Scheduled     => False,
                           Next_Event    => Concorde.Calendar.Clock,
                           Manager       => "faction-company",
                           Faction       => Faction,
                           Headquarters  => Capital,
                           Shares        => Faction_Company_Shares,
                           Dividend      => 0.2);
         Remaining_Shares : constant Natural := Faction_Company_Shares;
         Script           : constant Concorde.Handles.Script.Script_Handle :=
                              Concorde.Handles.Script.Create ("rc", User);
         Line_Index       : Natural := 0;

      begin

         Concorde.Colonies.Create.Initial_Colony
           (Faction => Faction,
            World   => Capital,
            Capital => Sector,
            Init    => Setup);

         Concorde.Handles.Shareholder.Create
           (Company => Company,
            Agent   => Faction,
            Shares  => Remaining_Shares);

         if not Setup.Contains ("init-script") then
            Ada.Text_IO.Put_Line
              ("warning: no initial script in " & Setup.Config_Name);
         end if;

         for Command of Setup.Child ("init-script") loop
            Line_Index := Line_Index + 1;
            Concorde.Handles.Script_Line.Create
              (Script => Script,
               Index  => Line_Index,
               Line   => Command.Config_Name);
         end loop;

         Concorde.Handles.Owned_World.Create
           (Faction => Faction,
            World   => Capital);

         Concorde.Handles.Market.Create
           (World => Capital);

         return Faction;
      end;
   end Create_Faction;

   ----------------------
   -- Find_Home_Sector --
   ----------------------

   function Find_Home_Sector
     (World : Concorde.Handles.World.World_Class)
      return Concorde.Handles.World_Sector.World_Sector_Class
   is
      function Score_Sector
        (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
         return Real;

      ------------------
      -- Score_Sector --
      ------------------

      function Score_Sector
        (Sector : Concorde.Handles.World_Sector.World_Sector_Class)
         return Real
      is
         Score : Real := 0.0;
      begin
         if Sector.Terrain.Is_Water then
            return Real'First;
         end if;

         declare
            Ns : constant Concorde.Worlds.World_Sector_Array :=
                   Concorde.Worlds.Get_Neighbours (Sector);
         begin
            for N of Ns loop
               for Deposit of
                 Concorde.Handles.Deposit.Select_By_World_Sector (N)
               loop
                  Score :=
                    Real'Max
                      (Score,
                         Deposit.Concentration / (1.0 + Deposit.Difficulty)
                       * (1.0 - Concorde.Worlds.Get_Terrain (N).Hazard));
               end loop;
            end loop;

            return Score * (1.0 - Sector.Terrain.Hazard);
         end;
      end Score_Sector;

   begin
      return Concorde.Worlds.Best_Sector (World, Score_Sector'Access);
   end Find_Home_Sector;

   --------------------
   -- Find_Homeworld --
   --------------------

   function Find_Homeworld
     return Concorde.Handles.World.World_Class
   is

      package Star_System_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Concorde.Handles.Star_System.Star_System_Handle,
           Concorde.Handles.Star_System."=");

      Queue : Star_System_Lists.List;
      Checked : WL.String_Sets.Set;

      function Check_World
        (World : Concorde.Handles.World.World_Class)
         return Boolean;

      -----------------
      -- Check_World --
      -----------------

      function Check_World
        (World : Concorde.Handles.World.World_Class)
         return Boolean
      is
      begin
         return World.Habitability > 0.7;
      end Check_World;

   begin

      Queue.Append (Concorde.Star_Systems.First.To_Star_System_Handle);
      Checked.Include
        (Concorde.Star_Systems.First.Name);

      while not Queue.Is_Empty loop
         declare
            use Concorde.Star_Systems;
            subtype Star_System_Handle is
              Concorde.Handles.Star_System.Star_System_Handle;
            Star_System : constant Star_System_Handle :=
                            Queue.First_Element;
         begin
            Queue.Delete_First;

            if not Star_System.Claimed then
               declare
                  Selection : constant Concorde.Worlds.World_Selection :=
                                Concorde.Star_Systems.Terrestrial_Worlds
                                  (Star_System);
               begin
                  if not Selection.Is_Empty then
                     for W of Selection.Get_Worlds loop
                        if Check_World (W) then
                           Claim (Star_System);
                           return W;
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            for Neighbour of
              Concorde.Handles.Star_System_Distance
                .Select_Star_System_Range_Bounded_By_Distance
                  (Star_System, 0.0, 99.0)
            loop
               declare
                  Neighbour_Name : constant String :=
                                     Neighbour.To.Name;
               begin
                  if not Checked.Contains (Neighbour_Name) then
                     Checked.Include (Neighbour_Name);
                     Queue.Append (Neighbour.To.To_Star_System_Handle);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Concorde.Handles.World.Empty_Handle;
   end Find_Homeworld;

end Concorde.Factions.Create;
